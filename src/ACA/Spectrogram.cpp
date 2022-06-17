
#include "Util.h"
#include "Matrix.h"
#include "RingBuffer.h"
#include "AudioFileIf.h"
#include "Fft.h"

#include "ToolPreProc.h"
#include "ToolBlockAudio.h"
#include "ToolConversion.h"

#include "Spectrogram.h"


/*! \brief class for computation of the spectrogram from a file
*/
class CSpectrogramFromFile : public CSpectrogramIf
{
public:
    CSpectrogramFromFile(std::string strAudioFilePath, int iBlockLength, int iHopLength, bool bNormalize, float *pfWindow);

    virtual ~CSpectrogramFromFile()
    {
        delete m_pCNormalize;
        m_pCNormalize = 0;

        CVector::free(m_pfProcBuff);

        m_pCAudioFile->closeFile();
        CAudioFileIf::destroy(m_pCAudioFile);
    };

private:
    CAudioFileIf *m_pCAudioFile;
};

CSpectrogramFromFile::CSpectrogramFromFile(std::string strAudioFilePath, int iBlockLength, int iHopLength, bool bNormalize, float *pfWindow) :
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


/*! \brief class for computation of the spectrogram from a vector of audio data
*/
class CSpectrogramFromVector : public CSpectrogramIf
{
public:
    CSpectrogramFromVector(const float *pfAudio, long long iAudioLength, float fSampleRate, int iBlockLength, int iHopLength, bool bNormalize, float *pfWindow);
    virtual ~CSpectrogramFromVector() {};
};

CSpectrogramFromVector::CSpectrogramFromVector(const float *pfAudio, long long iAudioLength, float fSampleRate, int iBlockLength, int iHopLength, bool bNormalize, float *pfWindow)
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
CSpectrogramIf::CSpectrogramIf()
{
    reset_();
}

inline CSpectrogramIf::~CSpectrogramIf()
{
    reset_();
}

Error_t CSpectrogramIf::create(CSpectrogramIf *&pCInstance, const std::string &strAudioFilePath, int iBlockLength, int iHopLength, bool bNormalize, float *pfWindow)
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

Error_t CSpectrogramIf::create(CSpectrogramIf *&pCInstance, const float *pfAudio, long long iNumSamples, float fSampleRate, int iBlockLength, int iHopLength, bool bNormalize, float *pfWindow)
{
    if (!pfAudio)
        return Error_t::kFunctionInvalidArgsError;
    if (iNumSamples <= 0)
        return Error_t::kFunctionInvalidArgsError;
    if (fSampleRate <= 0)
        return Error_t::kFunctionInvalidArgsError;
    if (iBlockLength <= 2 || !CUtil::isPowOf2(iBlockLength))
        return Error_t::kFunctionInvalidArgsError;
    if (iHopLength <= 0 || iHopLength > iBlockLength)
        return Error_t::kFunctionInvalidArgsError;

    pCInstance = new CSpectrogramFromVector(pfAudio, iNumSamples, fSampleRate, iBlockLength, iHopLength, bNormalize, pfWindow);

    return Error_t::kNoError;
}

Error_t CSpectrogramIf::destroy(CSpectrogramIf *&pCInstance)
{
    delete pCInstance;
    pCInstance = 0;

    return Error_t::kNoError;
}

Error_t CSpectrogramIf::getSpectrogramDimensions(int &iNumRows, int &iNumCols) const
{
    if (!m_bIsInitialized)
    {
        iNumRows = 0;
        iNumCols = 0;
        return Error_t::kFunctionIllegalCallError;
    }

    iNumRows = (m_iBlockLength >> 1) + 1;
    iNumCols = static_cast<int>(m_pCBlockAudio->getNumBlocks());

    return Error_t::kNoError;
}

Error_t CSpectrogramIf::getSpectrogramAxisVectors(float *pfAxisTicks, AxisLabel_t eAxisLabel) const
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

    if (eAxisLabel == kTimeInS)
    {
        long long iNumBlocks = m_pCBlockAudio->getNumBlocks();
        for (auto n = 0; n < iNumBlocks; n++)
            pfAxisTicks[n] = m_pCBlockAudio->getTimeStamp(n);
    }

    if (eAxisLabel == kFrequencyInHz)
    {
        int iNumFreqBins = (m_iBlockLength >> 1) + 1;
        for (auto k = 0; k < iNumFreqBins; k++)
            pfAxisTicks[k] = m_pCFft->bin2freq(k, m_fSampleRate);
    }

    return Error_t::kNoError;
}


Error_t CSpectrogramIf::compSpectrogram(float **ppfSpectrogram)
{
    if (!m_bIsInitialized)
        return Error_t::kFunctionIllegalCallError;
    if (!ppfSpectrogram)
        return Error_t::kFunctionInvalidArgsError;
    if (!ppfSpectrogram[0])
        return Error_t::kFunctionInvalidArgsError;

    assert(m_pfProcBuff);
    assert(m_pfSpectrum);
    assert(m_pCFft);

    long long iNumBlocks = m_pCBlockAudio->getNumBlocks();

    for (auto n = 0; n < iNumBlocks; n++)
    {
        auto iLength = m_pCFft->getLength(CFft::kLengthMagnitude);

        computeMagSpectrum_(iLength);

        // copy to output buffer
        for (auto k = 0; k < iLength; k++)
            ppfSpectrogram[k][n] = m_pfSpectrum[k];
    }

    return Error_t::kNoError;
}

void CSpectrogramIf::computeMagSpectrum_(int iLength)
{
    // retrieve the next audio block
    m_pCBlockAudio->getNextBlock(m_pfProcBuff);

    // normalize if specified
    if (m_pCNormalize)
        m_pCNormalize->normalizeBlock(m_pfProcBuff, m_iBlockLength);

    // compute magnitude spectrum 
    m_pCFft->compFft(m_pfSpectrum, m_pfProcBuff);
    m_pCFft->getMagnitude(m_pfSpectrum, m_pfSpectrum);

    CVector::mulC_I(m_pfSpectrum, 2.F, iLength);
}

void CSpectrogramIf::destroyMelFb_(const MelSpectrogramConfig_t *pMelSpecConfig)
{
    CMatrix::free(m_ppfHMel, pMelSpecConfig->iNumMelBins);
}

Error_t CSpectrogramIf::reset_()
{
    CVector::free(m_pfProcBuff);

    CVector::free(m_pfSpectrum);

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

Error_t CSpectrogramIf::init_(float *pfWindow)
{
    // initialize FFT and fft  buffer
    m_pCFft = new CFft();
    m_pCFft->init(m_iBlockLength);
    if (pfWindow)
        m_pCFft->overrideWindow(pfWindow);

    // allocate processing memory
    CVector::alloc(m_pfSpectrum, m_iBlockLength);
    CVector::alloc(m_pfProcBuff, m_iBlockLength);

    m_bIsInitialized = true;

    return Error_t::kNoError;
}


Error_t CSpectrogramIf::getMelSpectrogramDimensions(int &iNumRows, int &iNumCols, const MelSpectrogramConfig_t *pMelSpecConfig) const
{
    if (!pMelSpecConfig)
        return Error_t::kFunctionInvalidArgsError;

    if (!m_bIsInitialized)
    {
        iNumRows = 0;
        iNumCols = 0;
        return Error_t::kFunctionIllegalCallError;
    }

    iNumRows = pMelSpecConfig->iNumMelBins;
    iNumCols = static_cast<int>(m_pCBlockAudio->getNumBlocks());

    return Error_t::kNoError;
}

Error_t CSpectrogramIf::getMelSpectrogramAxisVectors(float *pfAxisTicks, AxisLabel_t eAxisLabel, const MelSpectrogramConfig_t *pMelSpecConfig)
{
    if (!m_bIsInitialized)
    {
        return Error_t::kFunctionIllegalCallError;
    }

    if (!pfAxisTicks || !pMelSpecConfig)
        return Error_t::kFunctionInvalidArgsError;

    assert(m_iBlockLength > 0);
    assert(m_iHopLength > 0);
    assert(m_fSampleRate > 0);


    if (eAxisLabel == kTimeInS)
    {
        long long iNumBlocks = m_pCBlockAudio->getNumBlocks();
        for (auto n = 0; n < iNumBlocks; n++)
            pfAxisTicks[n] = m_pCBlockAudio->getTimeStamp(n);
    }

    if (eAxisLabel == kFrequencyInHz)
    {
        generateMelFb_(pMelSpecConfig);

        assert(m_pffcMel);

        // note that we start with 1 to look at the center freqs only
        CVector::copy(pfAxisTicks, &m_pffcMel[1], pMelSpecConfig->iNumMelBins);

        destroyMelFb_(pMelSpecConfig);
    }

    return Error_t::kNoError;
}

Error_t CSpectrogramIf::compMelSpectrogram(float **ppfMelSpectrogram, const MelSpectrogramConfig_t *pMelSpecConfig)
{
    if (!m_bIsInitialized)
        return Error_t::kFunctionIllegalCallError;
    if (!pMelSpecConfig)
        return Error_t::kFunctionInvalidArgsError;
    if (!ppfMelSpectrogram)
        return Error_t::kFunctionInvalidArgsError;
    if (!ppfMelSpectrogram[0])
        return Error_t::kFunctionInvalidArgsError;

    assert(m_pfProcBuff);
    assert(m_pfSpectrum);
    assert(m_pCFft);

    // initialize mel filterbank
    generateMelFb_(pMelSpecConfig);
    assert(m_ppfHMel);

    long long iNumBlocks = m_pCBlockAudio->getNumBlocks();

    for (auto n = 0; n < iNumBlocks; n++)
    {
        auto iLength = m_pCFft->getLength(CFft::kLengthMagnitude);

        computeMagSpectrum_(iLength);

        // copy to output buffer
        for (auto k = 0; k < pMelSpecConfig->iNumMelBins; k++)
        {
            assert(m_ppfHMel[k]);
            ppfMelSpectrogram[k][n] = CVector::mulScalar(m_ppfHMel[k], m_pfSpectrum, iLength);
        }

        if (pMelSpecConfig->bIsLogarithmic)
        {
            // convert amplitude to level(dB)
            for (auto k = 0; k < pMelSpecConfig->iNumMelBins; k++)
                ppfMelSpectrogram[k][n] = 20.F * std::log10(ppfMelSpectrogram[k][n] + 1e-12F);
        }
    }

    destroyMelFb_(pMelSpecConfig);

    return Error_t::kNoError;
}

Error_t CSpectrogramIf::generateMelFb_(const MelSpectrogramConfig_t *pMelSpecConfig)
{
    assert(pMelSpecConfig);

    int iMagLength = m_pCFft->getLength(CFft::kLengthMagnitude);

    // configuration check
    if (pMelSpecConfig->fMinFreqInHz < 0)
        return Error_t::kFunctionInvalidArgsError;
    if (pMelSpecConfig->fMaxFreqInHz > m_fSampleRate / 2)
        return Error_t::kFunctionInvalidArgsError;
    if (pMelSpecConfig->iNumMelBins <= 0 || pMelSpecConfig->iNumMelBins > iMagLength)
        return Error_t::kFunctionInvalidArgsError;

    // allocate filter matrix and frequency matrix
    CVector::alloc(m_pffcMel, static_cast<long long>(pMelSpecConfig->iNumMelBins) + 2); // +2 for lower and upper bound
    CMatrix::alloc(m_ppfHMel, pMelSpecConfig->iNumMelBins, iMagLength);


    // compute center band frequencies
    m_pffcMel[0] = CConversion::convertFreq2Mel(pMelSpecConfig->fMinFreqInHz);
    m_pffcMel[pMelSpecConfig->iNumMelBins + 1] = CConversion::convertFreq2Mel(pMelSpecConfig->fMaxFreqInHz);
    float fMelInc = (m_pffcMel[pMelSpecConfig->iNumMelBins + 1] - m_pffcMel[0]) / (pMelSpecConfig->iNumMelBins + 1);
    for (auto k = 1; k < pMelSpecConfig->iNumMelBins + 1; k++)
        m_pffcMel[k] = m_pffcMel[k - 1] + fMelInc;
    for (auto k = 0; k < pMelSpecConfig->iNumMelBins + 2; k++)
        m_pffcMel[k] = CConversion::convertMel2Freq(m_pffcMel[k]);

    float *pf_l = &m_pffcMel[0],
        *pf_c = &m_pffcMel[1],
        *pf_u = &m_pffcMel[2];

    for (auto m = 0; m < pMelSpecConfig->iNumMelBins; m++)
    {
        float fFilterMax = 2.F / (pf_u[m] - pf_l[m]); //!< normalization

        int iLowBin = 1 + static_cast<int>(m_pCFft->freq2bin(pf_l[m], m_fSampleRate));
        int iCenterBin = 1 + static_cast<int>(m_pCFft->freq2bin(pf_c[m], m_fSampleRate));
        int iUpBin = 1 + static_cast<int>(m_pCFft->freq2bin(pf_u[m], m_fSampleRate));

        for (auto k = iLowBin; k < iCenterBin; k++)
            m_ppfHMel[m][k] = fFilterMax * (CConversion::convertBin2Freq(static_cast<float>(k), (iMagLength - 1) * 2, m_fSampleRate) - pf_l[m]) / (pf_c[m] - pf_l[m]);
        for (auto k = iCenterBin; k < iUpBin; k++)
            m_ppfHMel[m][k] = fFilterMax * (pf_u[m] - CConversion::convertBin2Freq(static_cast<float>(k), (iMagLength - 1) * 2, m_fSampleRate)) / (pf_u[m] - pf_c[m]);

    }

    return Error_t::kNoError;
}
