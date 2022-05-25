
#include "Util.h"
#include "AudioFileIf.h"
#include "Fft.h"
#include "ToolPreProc.h"
#include "ToolBlockAudio.h"
#include "ToolLowpass.h"
#include "NoveltyFromBlock.h"

#include "Novelty.h"


/////////////////////////////////////////////////////////////////////////////////
// file extraction
class CNoveltyFromFile : public CNoveltyIf
{
public:
    CNoveltyFromFile(Novelty_t eNoveltyIdx, std::string strAudioFilePath, int iBlockLength, int iHopLength);

    virtual ~CNoveltyFromFile()
    {
        delete m_pCNormalize;
        m_pCNormalize = 0;

        delete[] m_pfProcessBuff1;
        m_pfProcessBuff1 = 0;

        m_pCAudioFile->closeFile();
        CAudioFileIf::destroy(m_pCAudioFile);
    };

private:
    CAudioFileIf* m_pCAudioFile;
};

CNoveltyFromFile::CNoveltyFromFile(Novelty_t eNoveltyIdx, std::string strAudioFilePath, int iBlockLength, int iHopLength) :
    m_pCAudioFile(0)
{
    this->reset_();

    CAudioFileIf::FileSpec_t stFileSpec;
    CAudioFileIf::create(m_pCAudioFile);
    m_pCAudioFile->openFile(strAudioFilePath, CAudioFileIf::kFileRead);
    m_pCAudioFile->getFileSpec(stFileSpec);
    m_fSampleRate = stFileSpec.fSampleRateInHz;

    CBlockAudioIf::create(m_pCBlockAudio, m_pCAudioFile, iBlockLength, iHopLength);

    m_pCNormalize = new CNormalizeAudio(m_pCAudioFile);

    // set length variables
    m_iBlockLength = iBlockLength;
    m_iHopLength = iHopLength;

    init_(eNoveltyIdx);
}


/////////////////////////////////////////////////////////////////////////////////
// vector extraction
class CNoveltyFromVector : public CNoveltyIf
{
public:
    CNoveltyFromVector(Novelty_t eNoveltyIdx, const float* pfAudio, long long iAudioLength, float fSampleRate, int iBlockLength, int iHopLength);
    virtual ~CNoveltyFromVector() {};
};

CNoveltyFromVector::CNoveltyFromVector(Novelty_t eNoveltyIdx, const float* pfAudio, long long iAudioLength, float fSampleRate, int iBlockLength, int iHopLength)
{
    // set length variables
    m_iBlockLength = iBlockLength;
    m_iHopLength = iHopLength;

    // set sample rate
    m_fSampleRate = fSampleRate;

    CBlockAudioIf::create(m_pCBlockAudio, pfAudio, iAudioLength, iBlockLength, iHopLength, m_fSampleRate);

    m_pCNormalize = new CNormalizeAudio(pfAudio, iAudioLength);

    init_(eNoveltyIdx);
}


/////////////////////////////////////////////////////////////////////////////////
// base class
CNoveltyIf::CNoveltyIf()
{
    reset_();
}

inline CNoveltyIf::~CNoveltyIf()
{
    reset_();
}
                            
Error_t CNoveltyIf::create(CNoveltyIf*& pCInstance, Novelty_t eNoveltyIdx, const std::string& strAudioFilePath, int iBlockLength, int iHopLength)
{
    if (strAudioFilePath.empty())
        return Error_t::kFunctionInvalidArgsError;
    if (iBlockLength <= 2 || !CUtil::isPowOf2(iBlockLength))
        return Error_t::kFunctionInvalidArgsError;
    if (iHopLength <= 0 || iHopLength > iBlockLength)
        return Error_t::kFunctionInvalidArgsError;

    pCInstance = new CNoveltyFromFile(eNoveltyIdx, strAudioFilePath, iBlockLength, iHopLength);


    return Error_t::kNoError;
}

Error_t CNoveltyIf::create(CNoveltyIf*& pCInstance, Novelty_t eNoveltyIdx, const float* pfAudio, long long iNumFrames, float fSampleRate, int iBlockLength, int iHopLength)
{
    if (!pfAudio)
        return Error_t::kFunctionInvalidArgsError;
    if (iNumFrames <= 0)
        return Error_t::kFunctionInvalidArgsError;
    if (fSampleRate <= 0)
        return Error_t::kFunctionInvalidArgsError;
    if (iBlockLength <= 2)
        return Error_t::kFunctionInvalidArgsError;
    if (iHopLength <= 0 || iHopLength > iBlockLength)
        return Error_t::kFunctionInvalidArgsError;

    pCInstance = new CNoveltyFromVector(eNoveltyIdx, pfAudio, iNumFrames, fSampleRate, iBlockLength, iHopLength);

    return Error_t::kNoError;
}

Error_t CNoveltyIf::destroy(CNoveltyIf*& pCInstance)
{
    delete pCInstance;
    pCInstance = 0;

    return Error_t::kNoError;
}

Error_t CNoveltyIf::getNumBlocks(int& iNumBlocks) const
{
    if (!m_bIsInitialized)
    {
        iNumBlocks = 0;
        return Error_t::kFunctionIllegalCallError;
    }

    iNumBlocks = static_cast<int>(m_pCBlockAudio->getNumBlocks());

    return Error_t::kNoError;
}

Error_t CNoveltyIf::getNoveltyTimeStamps(float* pfAxisTicks) const
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
    for (auto n = 0; n < iNumBlocks; n++)
        pfAxisTicks[n] = m_pCBlockAudio->getTimeStamp(n);

    return Error_t::kNoError;
}

Error_t CNoveltyIf::getNovelty(float* pfNovelty, bool* pbIsOnset)
{
    if (!m_bIsInitialized)
        return Error_t::kFunctionIllegalCallError;
    if (!pfNovelty)
        return Error_t::kFunctionInvalidArgsError;

    assert(m_pfProcessBuff1);
    assert(m_pfProcessBuff2);
    assert(m_pCFft);
    assert(m_pCBlockAudio);
    assert(m_pCNormalize);
    assert(m_pCNovelty);

    const float fSmoothLpLenInS = 0.07F;
    const float fThreshLpLenInS = 0.14F;
    auto iNumBlocks = m_pCBlockAudio->getNumBlocks();

    float *pfThreshold = new float[iNumBlocks]; //!< memory allocation is ok since we process the whole signal at once

    for (auto n = 0; n < iNumBlocks; n++)
    {
        // retrieve the next audio block
        m_pCBlockAudio->getNextBlock(m_pfProcessBuff1);

        // normalize if specified
        if (m_pCNormalize)
            m_pCNormalize->normalizeBlock(m_pfProcessBuff1, m_iBlockLength);

        computeMagSpectrum_();

        m_pCNovelty->calcNoveltyFromBlock(&pfNovelty[n], m_pfProcessBuff1);
    }


    // smoothing with moving average
    m_pCLpFilter->reset();
    m_pCLpFilter->setFilterParam(m_pCLpFilter->calcFilterLength(fSmoothLpLenInS, m_fSampleRate / m_iHopLength));
    m_pCLpFilter->filtfilt(pfNovelty, pfNovelty, iNumBlocks);
    
    // HWR
    CVectorFloat::setZeroBelowThresh(pfNovelty, iNumBlocks);

    // threshold computation
    m_pCLpFilter->reset();
    m_pCLpFilter->setFilterParam(m_pCLpFilter->calcFilterLength(fThreshLpLenInS, m_fSampleRate / m_iHopLength));
    m_pCLpFilter->filtfilt(pfThreshold, pfNovelty, iNumBlocks);
    CVectorFloat::addC_I(pfThreshold, .4F * CVectorFloat::getMean(&pfNovelty[1], iNumBlocks - 1), iNumBlocks);
    CVectorFloat::sub_I(pfThreshold, pfNovelty, iNumBlocks);
    CVectorFloat::mulC_I(pfThreshold, -1.F, iNumBlocks);
    CVectorFloat::setZeroBelowThresh(pfThreshold, iNumBlocks);

    // peak picking
    if (pbIsOnset)
    {
        CVector::setValue(pbIsOnset, false, iNumBlocks);
        CVectorFloat::findPeaks(pbIsOnset, pfThreshold, iNumBlocks, 0.F);
    }

    // clean up memory
    delete[] pfThreshold;

    return Error_t::kNoError;
}


std::string CNoveltyIf::getNoveltyString(Novelty_t eNoveltyIdx)
{
    const std::map<CNoveltyIf::Novelty_t, std::string> NoveltyMap
    {
            {kNoveltyFlux, "Flux"},
            {kNoveltyHainsworth, "Hainsworth"},
            {kNoveltyLaroche, "Laroche"}
    };

    return NoveltyMap.at(eNoveltyIdx);
}

CNoveltyIf::Novelty_t CNoveltyIf::getNoveltyIdxFromString(std::string sNoveltyString)
{
    const std::map<std::string, CNoveltyIf::Novelty_t> NoveltyMap
    {
            {"Flux", kNoveltyFlux},
            {"Hainsworth", kNoveltyHainsworth},
            {"Laroche", kNoveltyLaroche}
    };
    auto search = NoveltyMap.find(sNoveltyString);
    if (search != NoveltyMap.end())
        return NoveltyMap.at(sNoveltyString);
    else
        return kNumNoveltyFunctions;
}

void CNoveltyIf::computeMagSpectrum_()
{
    assert(m_pCFft);

    // compute magnitude spectrum (hack
    m_pCFft->doFft(m_pfProcessBuff2, m_pfProcessBuff1);
    m_pCFft->getMagnitude(m_pfProcessBuff1, m_pfProcessBuff2);

    CVectorFloat::mulC_I(m_pfProcessBuff1, 2.F, m_pCFft->getLength(CFft::kLengthMagnitude));
}


Error_t CNoveltyIf::reset_()
{
    delete[] m_pfProcessBuff1;
    m_pfProcessBuff1 = 0;

    delete[] m_pfProcessBuff2;
    m_pfProcessBuff2 = 0;

    delete m_pCFft;
    m_pCFft = 0;

    delete m_pCNormalize;
    m_pCNormalize = 0;

    CBlockAudioIf::destroy(m_pCBlockAudio);
    CNoveltyFromBlockIf::destroy(m_pCNovelty);

    m_iBlockLength = 0;
    m_iHopLength = 0;

    CMovingAverage::destroy(m_pCLpFilter);

    m_bIsInitialized = false;

    return Error_t::kNoError;
}

Error_t CNoveltyIf::init_(Novelty_t eNoveltyIdx)
{
    // initialize FFT and fft  buffer
    m_pCFft = new CFft();
    m_pCFft->init(m_iBlockLength);
    // allocate processing memory
    m_pfProcessBuff1 = new float[m_pCFft->getLength(CFft::kLengthFft)];
    m_pfProcessBuff2 = new float[m_pCFft->getLength(CFft::kLengthFft)];
    
    CNoveltyFromBlockIf::create(m_pCNovelty, eNoveltyIdx, m_pCFft->getLength(CFft::kLengthMagnitude), m_fSampleRate);
    CMovingAverage::create(m_pCLpFilter);

    m_bIsInitialized = true;

    return Error_t::kNoError;
}
