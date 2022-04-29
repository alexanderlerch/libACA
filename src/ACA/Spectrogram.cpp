
#include "helper/Util.h"
#include "helper/Ringbuffer.h"

#include "ToolPreProc.h"
#include "AudioFileIf.h"
#include "Fft.h"
#include "ToolBlockAudio.h"
#include "Spectrogram.h"


class CSpectrogramFromFile : public CSpectrogramIf
{
public:
    CSpectrogramFromFile(std::string strAudioFilePath, int iBlockLength, int iHopLength, bool bNormalize);

    virtual ~CSpectrogramFromFile()
    {
        //CAudioFileIf::FileSpec_t stFileSpec;

        delete m_pCNormalize;
        m_pCNormalize = 0;

        delete[] m_pfProcessBuff;
        m_pfProcessBuff = 0;

        m_pCAudioFile->closeFile();
        CAudioFileIf::destroy(m_pCAudioFile);
    };
    //Error_t process(float** ppfSpectrogram) override;

private:
    CAudioFileIf* m_pCAudioFile;
};

class CSpectrogramFromVector : public CSpectrogramIf
{
public:
    CSpectrogramFromVector(const float* pfAudio, long long iAudioLength, float fSampleRate, int iBlockLength, int iHopLength, bool bNormalize);
    virtual ~CSpectrogramFromVector()
    {
    };
    //Error_t process(float** ppfSpectrogram) override;

private:
};

CSpectrogramFromFile::CSpectrogramFromFile(std::string strAudioFilePath, int iBlockLength, int iHopLength, bool bNormalize) :
    m_pCAudioFile(0)
{
    this->reset_();

    CAudioFileIf::FileSpec_t stFileSpec;
    CAudioFileIf::create(m_pCAudioFile);
    m_pCAudioFile->openFile(strAudioFilePath, CAudioFileIf::kFileRead);
    m_pCAudioFile->getFileSpec(stFileSpec);
    m_fSampleRate = stFileSpec.fSampleRateInHz;

    CBlockAudioIf::create(m_pCBlockAudio, m_pCAudioFile, iBlockLength, iHopLength, m_fSampleRate);

    if (bNormalize)
        m_pCNormalize = new CNormalizeAudio(m_pCAudioFile);

    // set length variables
    m_iBlockLength = iBlockLength;
    m_iHopLength = iHopLength;

    init_();
}

CSpectrogramFromVector::CSpectrogramFromVector(const float *pfAudio, long long iAudioLength, float fSampleRate, int iBlockLength, int iHopLength, bool bNormalize)
{


    // get length of audio file
    //m_iAudioLength = iNumFrames;

    // set length variables
    m_iBlockLength = iBlockLength;
    m_iHopLength = iHopLength;

    // set sample rate
    m_fSampleRate = fSampleRate;

    CBlockAudioIf::create(m_pCBlockAudio, pfAudio, iAudioLength, iBlockLength, iHopLength, m_fSampleRate);

    if (bNormalize)
        m_pCNormalize = new CNormalizeAudio(pfAudio, iAudioLength);

    init_();
}

CSpectrogramIf::CSpectrogramIf() :
    m_bIsInitialized(false),
    m_pCFft(0),
    m_pfSpectrum(0),
    m_pfProcessBuff(0),
    //m_iAudioLength(0),
    m_iBlockLength(0),
    m_iHopLength(0),
    m_fSampleRate(0),
    //m_iNumBlocks(0),
    m_pCNormalize(0),
    m_pCBlockAudio(0)
{
    reset_();
}

inline CSpectrogramIf::~CSpectrogramIf()
{
    reset_();
}

Error_t CSpectrogramIf::init_()
{
    // initialize FFT and fft  buffer
    m_pCFft = new CFft();
    m_pCFft->init(m_iBlockLength);
    m_pfSpectrum = new float[m_iBlockLength];
    m_pfProcessBuff = new float[m_iBlockLength];

    //m_iNumBlocks = m_pCBlockAudio->getNumBlocks();

    m_bIsInitialized = true;

    return Error_t::kNoError;
}

Error_t CSpectrogramIf::create(CSpectrogramIf*& pCInstance, std::string strAudioFilePath, int iBlockLength, int iHopLength, bool bNormalize)
{
    if (strAudioFilePath.empty())
        return Error_t::kFunctionInvalidArgsError;
    if (iBlockLength <= 2 || !CUtil::isPowOf2(iBlockLength))
        return Error_t::kFunctionInvalidArgsError;
    if (iHopLength <= 0 || iHopLength > iBlockLength)
        return Error_t::kFunctionInvalidArgsError;

    pCInstance = new CSpectrogramFromFile(strAudioFilePath, iBlockLength, iHopLength, bNormalize);


    return Error_t::kNoError;
}

Error_t CSpectrogramIf::create(CSpectrogramIf*& pCInstance, const float* pfAudio, long long iNumFrames, float fSampleRate, int iBlockLength, int iHopLength, bool bNormalize)
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

    pCInstance = new CSpectrogramFromVector(pfAudio, iNumFrames, fSampleRate, iBlockLength, iHopLength, bNormalize);

    return Error_t::kNoError;
}

Error_t CSpectrogramIf::destroy(CSpectrogramIf*& pCInstance)
{
    delete pCInstance;
    pCInstance = 0;

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

    //m_iAudioLength = 0;
    m_iBlockLength = 0;
    m_iHopLength = 0;
    //m_iNumBlocks = 0;

    m_bIsInitialized = false;

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
    //assert(m_iNumBlocks > 0);
    assert(m_fSampleRate > 0);

    long long iNumBlocks = m_pCBlockAudio->getNumBlocks();

    if (eAxisLabel == kTimeInS)
    {
        for (auto n = 0; n < iNumBlocks; n++)
            pfAxisTicks[n] = (m_iBlockLength / 2 + n * m_iHopLength) / m_fSampleRate;
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
        m_pCBlockAudio->getNextBlock(m_pfProcessBuff);

        if (m_pCNormalize)
            m_pCNormalize->normalizeBlock(m_pfProcessBuff, m_iBlockLength);

        // compute magnitude spectrum (hack
        m_pCFft->doFft(m_pfSpectrum, m_pfProcessBuff);
        m_pCFft->getMagnitude(m_pfSpectrum, m_pfSpectrum);

        // copy to output buffer
        auto iLength = m_pCFft->getLength(CFft::kLengthMagnitude);
        for (auto k = 0; k < iLength; k++)
            ppfSpectrogram[k][n] = m_pfSpectrum[k];

    }

    return Error_t::kNoError;
}

//Error_t CSpectrogramFromVector::process(float** ppfSpectrogram)
//{
//    if (!m_bIsInitialized)
//        return Error_t::kFunctionIllegalCallError;
//    if (!ppfSpectrogram)
//        return Error_t::kFunctionInvalidArgsError;
//    if (!ppfSpectrogram[0])
//        return Error_t::kFunctionInvalidArgsError;
//
//    assert(m_pfSpectrum);
//    assert(m_pCBlockAudio);
//    assert(m_pCFft);
//
//    for (auto n = 0; n < m_iNumBlocks; n++)
//    {
//        auto iCurrIdx = n * m_iHopLength;
//
//        // compute magnitude spectrum (hack
//        m_pCFft->doFft(m_pfSpectrum, &m_pfInputAudio[iCurrIdx]);
//        m_pCFft->getMagnitude(m_pfSpectrum, m_pfSpectrum);
//
//        // copy to output buffer
//        auto iLength = m_pCFft->getLength(CFft::kLengthMagnitude);
//        for (auto k = 0; k < iLength; k++)
//            ppfSpectrogram[k][n] = 2.F*m_pfSpectrum[k];
//
//    }
//
//    return Error_t::kNoError;
//}
