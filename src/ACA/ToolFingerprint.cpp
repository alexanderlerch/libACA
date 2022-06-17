
#include "Util.h"
#include "Matrix.h"
#include "Vector.h"
#include "AudioFileIf.h"
#include "Fft.h"

#include "ToolPreProc.h"
#include "ToolBlockAudio.h"
#include "ToolResample.h"

#include "SubFingerprint.h"

#include "ToolFingerprint.h"


CFingerprint::~CFingerprint()
{
    reset();
    CVector::free(m_pfAudioBuff);

    CBlockAudioIf::destroy(m_pCBlockAudio);
}

Error_t CFingerprint::init(const std::string &strAudioFilePath)
{
    if (strAudioFilePath.empty())
        return Error_t::kFunctionInvalidArgsError;

    long long iInLength = 0;
    CAudioFileIf *pCAudioFile = 0;
    CResample *pCResample = 0;

    CAudioFileIf::FileSpec_t stFileSpec;

    const int iBlockLength = 4096;
    long long iCurrPos = 0;
    float **ppfAudioBlock = 0;
    float *pfAllAudio = 0;

    CAudioFileIf::create(pCAudioFile);
    pCAudioFile->openFile(strAudioFilePath, CAudioFileIf::kFileRead);
    pCAudioFile->getFileSpec(stFileSpec);
    pCAudioFile->getLength(iInLength);

    // alloc read buffer
    CMatrix::alloc(ppfAudioBlock, stFileSpec.iNumChannels, iBlockLength);
    CVector::alloc(pfAllAudio, iInLength);

    while (!pCAudioFile->isEof())
    {
        // set block length variable
        long long iNumSamples = iBlockLength;

        // read data (iNumSamples might be updated!)
        pCAudioFile->readData(ppfAudioBlock, iNumSamples);

        //downmix if multichannel
        CPreProc::downmix(&pfAllAudio[iCurrPos], ppfAudioBlock, stFileSpec.iNumChannels, iNumSamples);

        iCurrPos += iNumSamples;
    }

    pCResample = new CResample(stFileSpec.fSampleRateInHz, m_fProcSampleRate);

    m_iAudioLength = pCResample->getOutputLength(iInLength);

    CVector::alloc(m_pfAudioBuff, m_iAudioLength);

    pCResample->process(m_pfAudioBuff, pfAllAudio, iInLength);

    init_();

    delete pCResample;

    //free internal memory
    CMatrix::free(ppfAudioBlock, stFileSpec.iNumChannels);
    CVector::free(pfAllAudio);

    CAudioFileIf::destroy(pCAudioFile);

    return Error_t::kNoError;
}

Error_t CFingerprint::init(const float *pfIn, long long iNumFrames, float fSampleRate)
{
    if (!pfIn)
        return Error_t::kFunctionInvalidArgsError;
    if (iNumFrames <= 0 || fSampleRate <= 0)
        return Error_t::kFunctionInvalidArgsError;

    CResample *pCResample = 0;

    pCResample = new CResample(fSampleRate, m_fProcSampleRate);

    m_iAudioLength = pCResample->getOutputLength(iNumFrames);

    CVector::alloc(m_pfAudioBuff, m_iAudioLength);

    pCResample->process(m_pfAudioBuff, pfIn, iNumFrames);

    init_();

    delete pCResample;

    return Error_t::kNoError;
}

long long CFingerprint::getFingerprintLength() const
{
    return m_pCBlockAudio->getNumBlocks();
}

float CFingerprint::getTimeStamp(int iBlockIdx) const
{
    return m_pCBlockAudio->getTimeStamp(iBlockIdx);
}

Error_t CFingerprint::getTimeStamps(float *pfAxisTicks) const
{
    if (!m_bIsInitialized)
    {
        return Error_t::kFunctionIllegalCallError;
    }

    if (!pfAxisTicks)
        return Error_t::kFunctionInvalidArgsError;

    assert(m_iBlockLength > 0);
    assert(m_iHopLength > 0);
    assert(m_fProcSampleRate > 0);

    long long iNumBlocks = m_pCBlockAudio->getNumBlocks();
    for (auto n = 0; n < iNumBlocks; n++)
        pfAxisTicks[n] = m_pCBlockAudio->getTimeStamp(n);

    return Error_t::kNoError;
}

Error_t CFingerprint::compFingerprint(uint32_t *puiFingerprint)
{
    if (!m_bIsInitialized)
        return Error_t::kFunctionIllegalCallError;
    if (!puiFingerprint)
        return Error_t::kFunctionInvalidArgsError;

    assert(m_pfProcBuff1);
    assert(m_pfProcBuff2);
    assert(m_pCFft);
    assert(m_pCBlockAudio);
    assert(m_pCSubFingerprint);

    auto iNumBlocks = m_pCBlockAudio->getNumBlocks();

    for (auto n = 0; n < iNumBlocks; n++)
    {
        // retrieve the next audio block
        m_pCBlockAudio->getNextBlock(m_pfProcBuff1);

        computeMagSpectrum_();

        puiFingerprint[n] = m_pCSubFingerprint->compSubFingerprint(m_pfProcBuff1);
    }

    return Error_t::kNoError;
}


Error_t CFingerprint::reset()  
{
    m_bIsInitialized = false;

    CVector::free(m_pfProcBuff1);
    CVector::free(m_pfProcBuff2);

    delete m_pCFft;
    m_pCFft = 0;

    delete m_pCSubFingerprint;
    m_pCSubFingerprint = 0;

    m_iAudioLength = 0;

    return Error_t::kNoError;
}

Error_t CFingerprint::init_() 
{

    CBlockAudioIf::create(m_pCBlockAudio, m_pfAudioBuff, m_iAudioLength, m_iBlockLength, m_iHopLength, m_fProcSampleRate);

    // initialize FFT and buffer
    m_pCFft = new CFft();
    m_pCFft->init(m_iBlockLength);

    // allocate processing memory
    CVector::alloc(m_pfProcBuff1, m_pCFft->getLength(CFft::kLengthFft));
    CVector::alloc(m_pfProcBuff2, m_pCFft->getLength(CFft::kLengthFft));

    m_pCSubFingerprint = new CSubFingerprint();

    m_bIsInitialized = true;

    return Error_t::kNoError;
}

void CFingerprint::computeMagSpectrum_()
{
    assert(m_pCFft);

    // compute magnitude spectrum 
    m_pCFft->compFft(m_pfProcBuff2, m_pfProcBuff1);
    m_pCFft->getMagnitude(m_pfProcBuff1, m_pfProcBuff2);

    CVector::mulC_I(m_pfProcBuff2, 2.F, m_pCFft->getLength(CFft::kLengthMagnitude));
}

