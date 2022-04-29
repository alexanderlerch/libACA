
#include "Util.h"
#include "Ringbuffer.h"
#include "AudioFileIf.h"
#include "Fft.h"
#include "ToolPreProc.h"
#include "ToolBlockAudio.h"

#include "Spectrogram.h"


/////////////////////////////////////////////////////////////////////////////////
// file extraction
class CSpectrogramFromFile : public CSpectrogramIf
{
public:
    CSpectrogramFromFile(std::string strAudioFilePath, int iBlockLength, int iHopLength, bool bNormalize, float* pfWindow);

    virtual ~CSpectrogramFromFile()
    {
        delete m_pCNormalize;
        m_pCNormalize = 0;

        delete[] m_pfProcessBuff;
        m_pfProcessBuff = 0;

        m_pCAudioFile->closeFile();
        CAudioFileIf::destroy(m_pCAudioFile);
    };

private:
    CAudioFileIf* m_pCAudioFile;
};

CSpectrogramFromFile::CSpectrogramFromFile(std::string strAudioFilePath, int iBlockLength, int iHopLength, bool bNormalize, float* pfWindow) :
    m_pCAudioFile(0)
{
    this->reset_();

    CAudioFileIf::FileSpec_t stFileSpec;
    CAudioFileIf::create(m_pCAudioFile);
    m_pCAudioFile->openFile(strAudioFilePath, CAudioFileIf::kFileRead);
    m_pCAudioFile->getFileSpec(stFileSpec);
    m_fSampleRate = stFileSpec.fSampleRateInHz;

    CBlockAudioIf::create(m_pCBlockAudio, m_pCAudioFile, iBlockLength, iHopLength);

    if (bNormalize)
        m_pCNormalize = new CNormalizeAudio(m_pCAudioFile);

    // set length variables
    m_iBlockLength = iBlockLength;
    m_iHopLength = iHopLength;

    init_(pfWindow);
}


/////////////////////////////////////////////////////////////////////////////////
// vector extraction
class CSpectrogramFromVector : public CSpectrogramIf
{
public:
    CSpectrogramFromVector(const float* pfAudio, long long iAudioLength, float fSampleRate, int iBlockLength, int iHopLength, bool bNormalize, float* pfWindow);
    virtual ~CSpectrogramFromVector() {};
};

CSpectrogramFromVector::CSpectrogramFromVector(const float* pfAudio, long long iAudioLength, float fSampleRate, int iBlockLength, int iHopLength, bool bNormalize, float* pfWindow)
{
    // set length variables
    m_iBlockLength = iBlockLength;
    m_iHopLength = iHopLength;

    // set sample rate
    m_fSampleRate = fSampleRate;

    CBlockAudioIf::create(m_pCBlockAudio, pfAudio, iAudioLength, iBlockLength, iHopLength, m_fSampleRate);

    if (bNormalize)
        m_pCNormalize = new CNormalizeAudio(pfAudio, iAudioLength);

    init_(pfWindow);
}


/////////////////////////////////////////////////////////////////////////////////
// base class
CSpectrogramIf::CSpectrogramIf() :
    m_bIsInitialized(false),
    m_pCFft(0),
    m_pfSpectrum(0),
    m_pfProcessBuff(0),
    m_iBlockLength(0),
    m_iHopLength(0),
    m_fSampleRate(0),
    m_pCNormalize(0),
    m_pCBlockAudio(0)
{
    reset_();
}

inline CSpectrogramIf::~CSpectrogramIf()
{
    reset_();
}

Error_t CSpectrogramIf::create(CSpectrogramIf*& pCInstance, std::string strAudioFilePath, int iBlockLength, int iHopLength, bool bNormalize, float* pfWindow)
{
    if (strAudioFilePath.empty())
        return Error_t::kFunctionInvalidArgsError;
    if (iBlockLength <= 2 || !CUtil::isPowOf2(iBlockLength))
        return Error_t::kFunctionInvalidArgsError;
    if (iHopLength <= 0 || iHopLength > iBlockLength)
        return Error_t::kFunctionInvalidArgsError;

    pCInstance = new CSpectrogramFromFile(strAudioFilePath, iBlockLength, iHopLength, bNormalize, pfWindow);


    return Error_t::kNoError;
}

Error_t CSpectrogramIf::create(CSpectrogramIf*& pCInstance, const float* pfAudio, long long iNumFrames, float fSampleRate, int iBlockLength, int iHopLength, bool bNormalize, float* pfWindow)
{
    if (!pfAudio)
        return Error_t::kFunctionInvalidArgsError;
    if (iNumFrames <= 0)
        return Error_t::kFunctionInvalidArgsError;
    if (fSampleRate <= 0)
        return Error_t::kFunctionInvalidArgsError;
    if (iBlockLength <= 2 || !CUtil::isPowOf2(iBlockLength))
        return Error_t::kFunctionInvalidArgsError;
    if (iHopLength <= 0 || iHopLength > iBlockLength)
        return Error_t::kFunctionInvalidArgsError;

    pCInstance = new CSpectrogramFromVector(pfAudio, iNumFrames, fSampleRate, iBlockLength, iHopLength, bNormalize, pfWindow);

    return Error_t::kNoError;
}

Error_t CSpectrogramIf::destroy(CSpectrogramIf*& pCInstance)
{
    delete pCInstance;
    pCInstance = 0;

    return Error_t::kNoError;
}

Error_t CSpectrogramIf::getSpectrogramDimensions(int& iNumRows, int& iNumCols) const
{
    if (!m_bIsInitialized)
    {
        iNumRows = 0;
        iNumCols = 0;
        return Error_t::kFunctionIllegalCallError;
    }

    iNumRows = (m_iBlockLength >> 1) + 1;
    iNumCols = m_pCBlockAudio->getNumBlocks();

    return Error_t::kNoError;
}

Error_t CSpectrogramIf::getAxisVectors(float* pfAxisTicks, AxisLabel_t eAxisLabel) const
{
    if (!m_bIsInitialized)
    {
        return Error_t::kFunctionIllegalCallError;
    }

    if (!pfAxisTicks)
        return Error_t::kFunctionInvalidArgsError;

    assert(m_iBlockLength > 0);
    assert(m_iHopLength > 0);
    assert(m_fSampleRate > 0);

    long long iNumBlocks = m_pCBlockAudio->getNumBlocks();

    if (eAxisLabel == kTimeInS)
    {
        for (auto n = 0; n < iNumBlocks; n++)
            pfAxisTicks[n] = m_pCBlockAudio->getTimeStamp(n);
    }

    if (eAxisLabel == kFrequencyInHz)
    {
        int iNumFreqBins = (m_iBlockLength >> 1) + 1;
        for (auto k = 0; k < iNumFreqBins; k++)
            pfAxisTicks[k] = k * m_fSampleRate / m_iBlockLength;
    }

    return Error_t::kNoError;
}

Error_t CSpectrogramIf::process(float** ppfSpectrogram)
{
    if (!m_bIsInitialized)
        return Error_t::kFunctionIllegalCallError;
    if (!ppfSpectrogram)
        return Error_t::kFunctionInvalidArgsError;
    if (!ppfSpectrogram[0])
        return Error_t::kFunctionInvalidArgsError;

    assert(m_pfProcessBuff);
    assert(m_pfSpectrum);
    assert(m_pCFft);

    long long iNumBlocks = m_pCBlockAudio->getNumBlocks();

    for (auto n = 0; n < iNumBlocks; n++)
    {
        // retrieve the next audio block
        m_pCBlockAudio->getNextBlock(m_pfProcessBuff);

        // normalize if specified
        if (m_pCNormalize)
            m_pCNormalize->normalizeBlock(m_pfProcessBuff, m_iBlockLength);

        // compute magnitude spectrum (hack
        m_pCFft->doFft(m_pfSpectrum, m_pfProcessBuff);
        m_pCFft->getMagnitude(m_pfSpectrum, m_pfSpectrum);

        // copy to output buffer
        auto iLength = m_pCFft->getLength(CFft::kLengthMagnitude);
        CVectorFloat::mulC_I(m_pfSpectrum, 2.F, iLength);
        for (auto k = 0; k < iLength; k++)
            ppfSpectrogram[k][n] = m_pfSpectrum[k];
    }

    return Error_t::kNoError;
}

Error_t CSpectrogramIf::reset_()
{
    delete[] m_pfProcessBuff;
    m_pfProcessBuff = 0;

    delete[] m_pfSpectrum;
    m_pfSpectrum = 0;

    delete m_pCFft;
    m_pCFft = 0;

    delete m_pCNormalize;
    m_pCNormalize = 0;

    CBlockAudioIf::destroy(m_pCBlockAudio);

    m_iBlockLength = 0;
    m_iHopLength = 0;

    m_bIsInitialized = false;

    return Error_t::kNoError;
}

Error_t CSpectrogramIf::init_(float* pfWindow)
{
    // initialize FFT and fft  buffer
    m_pCFft = new CFft();
    m_pCFft->init(m_iBlockLength);
    if (pfWindow)
        m_pCFft->overrideWindow(pfWindow);

    // allocate processing memory
    m_pfSpectrum = new float[m_iBlockLength];
    m_pfProcessBuff = new float[m_iBlockLength];

    m_bIsInitialized = true;

    return Error_t::kNoError;
}
