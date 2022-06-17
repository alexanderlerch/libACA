
#include "Util.h"
#include "AudioFileIf.h"
#include "Fft.h"

#include "ToolPreProc.h"
#include "ToolBlockAudio.h"

#include "Feature.h"
#include "FeatureFromBlock.h"


/*! \brief class for computation of a feature from a file
*/
class CFeatureFromFile : public CFeatureIf
{
public:
    CFeatureFromFile(Feature_t eFeatureIdx, std::string strAudioFilePath, int iBlockLength, int iHopLength);

    virtual ~CFeatureFromFile()
    {
        delete m_pCNormalize;
        m_pCNormalize = 0;

        CVector::free(m_pfProcBuff1);

        m_pCAudioFile->closeFile();
        CAudioFileIf::destroy(m_pCAudioFile);
    };

private:
    CAudioFileIf *m_pCAudioFile;
};

CFeatureFromFile::CFeatureFromFile(Feature_t eFeatureIdx, std::string strAudioFilePath, int iBlockLength, int iHopLength) :
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

    init_(eFeatureIdx);
}


/*! \brief class for computation of a feature from a vector of audio data
*/
class CFeatureFromVector : public CFeatureIf
{
public:
    CFeatureFromVector(Feature_t eFeatureIdx, const float *pfAudio, long long iAudioLength, float fSampleRate, int iBlockLength, int iHopLength);
    virtual ~CFeatureFromVector() {};
};

CFeatureFromVector::CFeatureFromVector(Feature_t eFeatureIdx, const float *pfAudio, long long iAudioLength, float fSampleRate, int iBlockLength, int iHopLength)
{
    // set length variables
    m_iBlockLength = iBlockLength;
    m_iHopLength = iHopLength;

    // set sample rate
    m_fSampleRate = fSampleRate;

    CBlockAudioIf::create(m_pCBlockAudio, pfAudio, iAudioLength, iBlockLength, iHopLength, m_fSampleRate);

    m_pCNormalize = new CNormalizeAudio(pfAudio, iAudioLength);

    init_(eFeatureIdx);
}


/////////////////////////////////////////////////////////////////////////////////
// base class
CFeatureIf::CFeatureIf()
{
    reset_();
}

inline CFeatureIf::~CFeatureIf()
{
    reset_();
}

Error_t CFeatureIf::create(CFeatureIf *&pCInstance, Feature_t eFeatureIdx, const std::string &strAudioFilePath, int iBlockLength, int iHopLength)
{
    if (strAudioFilePath.empty())
        return Error_t::kFunctionInvalidArgsError;
    if (iBlockLength <= 2 || !CUtil::isPowOf2(iBlockLength))
        return Error_t::kFunctionInvalidArgsError;
    if (iHopLength <= 0 || iHopLength > iBlockLength)
        return Error_t::kFunctionInvalidArgsError;

    pCInstance = new CFeatureFromFile(eFeatureIdx, strAudioFilePath, iBlockLength, iHopLength);


    return Error_t::kNoError;
}

Error_t CFeatureIf::create(CFeatureIf *&pCInstance, Feature_t eFeatureIdx, const float *pfAudio, long long iNumSamples, float fSampleRate, int iBlockLength, int iHopLength)
{
    if (!pfAudio)
        return Error_t::kFunctionInvalidArgsError;
    if (iNumSamples <= 0)
        return Error_t::kFunctionInvalidArgsError;
    if (fSampleRate <= 0)
        return Error_t::kFunctionInvalidArgsError;
    if (iBlockLength <= 2)
        return Error_t::kFunctionInvalidArgsError;
    if (iHopLength <= 0 || iHopLength > iBlockLength)
        return Error_t::kFunctionInvalidArgsError;

    pCInstance = new CFeatureFromVector(eFeatureIdx, pfAudio, iNumSamples, fSampleRate, iBlockLength, iHopLength);

    return Error_t::kNoError;
}

Error_t CFeatureIf::destroy(CFeatureIf *&pCInstance)
{
    delete pCInstance;
    pCInstance = 0;

    return Error_t::kNoError;
}

Error_t CFeatureIf::getFeatureDimensions(int &iNumRows, int &iNumCols) const
{
    if (!m_bIsInitialized)
    {
        iNumRows = 0;
        iNumCols = 0;
        return Error_t::kFunctionIllegalCallError;
    }

    iNumRows = m_pCFeature->getFeatureDimensions();
    iNumCols = static_cast<int>(m_pCBlockAudio->getNumBlocks());

    return Error_t::kNoError;
}

float CFeatureIf::getTimeStamp(int iBlockIdx) const
{
    return m_pCBlockAudio->getTimeStamp(iBlockIdx);
}

Error_t CFeatureIf::getTimeStamps(float *pfAxisTicks) const
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

Error_t CFeatureIf::compFeature1Dim(float *pfFeature)
{
    if (!m_bIsInitialized)
        return Error_t::kFunctionIllegalCallError;
    if (!pfFeature)
        return Error_t::kFunctionInvalidArgsError;

    assert(m_pfProcBuff1);
    assert(m_pfProcBuff2);
    assert(m_pCFft);
    assert(m_pCBlockAudio);
    assert(m_pCNormalize);
    assert(m_pCFeature);

    auto iNumBlocks = m_pCBlockAudio->getNumBlocks();

    for (auto n = 0; n < iNumBlocks; n++)
    {
        // retrieve the next audio block
        m_pCBlockAudio->getNextBlock(m_pfProcBuff1);

        // normalize if specified
        if (m_pCNormalize)
            m_pCNormalize->normalizeBlock(m_pfProcBuff1, m_iBlockLength);

        if (isFeatureSpectral_(m_pCFeature->getFeatureIdx()))
            computeMagSpectrum_();

        m_pCFeature->compFeature(&pfFeature[n], m_pfProcBuff1);
    }

    return Error_t::kNoError;
}

Error_t CFeatureIf::compFeatureNDim(float **ppfFeature)
{
    if (!m_bIsInitialized)
        return Error_t::kFunctionIllegalCallError;
    if (!ppfFeature)
        return Error_t::kFunctionInvalidArgsError;
    if (!ppfFeature[0])
        return Error_t::kFunctionInvalidArgsError;

    assert(m_pfProcBuff1);
    assert(m_pfProcBuff2);
    assert(m_pCBlockAudio);
    assert(m_pCNormalize);
    assert(m_pCFeature);

    auto iNumBlocks = m_pCBlockAudio->getNumBlocks();
    auto iNumFeatures = m_pCFeature->getFeatureDimensions();

    for (auto n = 0; n < iNumBlocks; n++)
    {
        // retrieve the next audio block
        m_pCBlockAudio->getNextBlock(m_pfProcBuff1);

        // normalize if specified
        if (m_pCNormalize)
            m_pCNormalize->normalizeBlock(m_pfProcBuff1, m_iBlockLength);

        if (isFeatureSpectral_(m_pCFeature->getFeatureIdx()))
            computeMagSpectrum_();

        m_pCFeature->compFeature(m_pfProcBuff2, m_pfProcBuff1);

        // copy to output buffer
        for (auto k = 0; k < iNumFeatures; k++)
            ppfFeature[k][n] = m_pfProcBuff2[k];
    }

    return Error_t::kNoError;
}

std::string CFeatureIf::getFeatureString(Feature_t eFeatureIdx)
{
    const std::map<CFeatureIf::Feature_t, std::string> FeatureMap
    {
            {kFeatureSpectralCentroid, "SpectralCentroid"},
            {kFeatureSpectralCrestFactor, "SpectralCrestFactor"},
            {kFeatureSpectralDecrease, "SpectralDecrease"},
            {kFeatureSpectralFlatness, "SpectralFlatness"},
            {kFeatureSpectralFlux, "SpectralFlux"},
            {kFeatureSpectralKurtosis, "SpectralKurtosis"},
            {kFeatureSpectralMfccs, "SpectralMfccs"},
            {kFeatureSpectralPitchChroma, "SpectralPitchChroma"},
            {kFeatureSpectralRolloff, "SpectralRolloff"},
            {kFeatureSpectralSkewness, "SpectralSkewness"},
            {kFeatureSpectralSlope, "SpectralSlope"},
            {kFeatureSpectralSpread, "SpectralSpread"},
            {kFeatureSpectralTonalPowerRatio, "SpectralTonalPowerRatio"},

            {kFeatureTimeAcfCoeff, "TimeAcfCoeff"},
            {kFeatureTimeMaxAcf, "TimeMaxAcf"},
            {kFeatureTimePeakEnvelope, "TimePeakEnvelope"},
            {kFeatureTimeRms, "TimeRms"},
            {kFeatureTimeStd, "TimeStd"},
            {kFeatureTimeZeroCrossingRate, "TimeZeroCrossingRate"}
    };

    return FeatureMap.at(eFeatureIdx);
}

CFeatureIf::Feature_t CFeatureIf::getFeatureIdxFromString(std::string sFeatureString)
{
    const std::map<std::string, CFeatureIf::Feature_t> FeatureMap
    {
            {"SpectralCentroid", kFeatureSpectralCentroid},
            {"SpectralCrestFactor", kFeatureSpectralCrestFactor},
            {"SpectralDecrease", kFeatureSpectralDecrease},
            {"SpectralFlatness", kFeatureSpectralFlatness},
            {"SpectralFlux", kFeatureSpectralFlux},
            {"SpectralKurtosis", kFeatureSpectralKurtosis},
            {"SpectralMfccs", kFeatureSpectralMfccs},
            {"SpectralPitchChroma", kFeatureSpectralPitchChroma},
            {"SpectralRolloff", kFeatureSpectralRolloff},
            {"SpectralSkewness", kFeatureSpectralSkewness},
            {"SpectralSlope", kFeatureSpectralSlope},
            {"SpectralSpread", kFeatureSpectralSpread},
            {"SpectralTonalPowerRatio", kFeatureSpectralTonalPowerRatio},

            {"TimeAcfCoeff", kFeatureTimeAcfCoeff},
            {"TimeMaxAcf", kFeatureTimeMaxAcf},
            {"TimePeakEnvelope", kFeatureTimePeakEnvelope},
            {"TimeRms", kFeatureTimeRms},
            {"TimeStd", kFeatureTimeStd},
            {"TimeZeroCrossingRate", kFeatureTimeZeroCrossingRate}
    };
    auto search = FeatureMap.find(sFeatureString);
    if (search != FeatureMap.end())
        return FeatureMap.at(sFeatureString);
    else
        return kNumFeatures;
}

void CFeatureIf::computeMagSpectrum_()
{
    assert(m_pCFft);

    // compute magnitude spectrum (hack
    m_pCFft->compFft(m_pfProcBuff2, m_pfProcBuff1);
    m_pCFft->getMagnitude(m_pfProcBuff1, m_pfProcBuff2);

    CVector::mulC_I(m_pfProcBuff2, 2.F, m_pCFft->getLength(CFft::kLengthMagnitude));
}


Error_t CFeatureIf::reset_()
{
    CVector::free(m_pfProcBuff1);

    CVector::free(m_pfProcBuff2);

    delete m_pCFft;
    m_pCFft = 0;

    delete m_pCNormalize;
    m_pCNormalize = 0;

    CBlockAudioIf::destroy(m_pCBlockAudio);
    CFeatureFromBlockIf::destroy(m_pCFeature);

    m_iBlockLength = 0;
    m_iHopLength = 0;

    m_bIsInitialized = false;

    return Error_t::kNoError;
}

Error_t CFeatureIf::init_(Feature_t eFeatureIdx)
{
    if (isFeatureSpectral_(eFeatureIdx))
    {
        // initialize FFT and fft  buffer
        m_pCFft = new CFft();
        m_pCFft->init(m_iBlockLength);
        // allocate processing memory
        CVector::alloc(m_pfProcBuff1, m_pCFft->getLength(CFft::kLengthFft));
        CVector::alloc(m_pfProcBuff2, m_pCFft->getLength(CFft::kLengthFft));
        CFeatureFromBlockIf::create(m_pCFeature, eFeatureIdx, m_pCFft->getLength(CFft::kLengthMagnitude), m_fSampleRate);
    }
    else
    {
        // allocate processing memory
        CVector::alloc(m_pfProcBuff1, m_iBlockLength);
        CFeatureFromBlockIf::create(m_pCFeature, eFeatureIdx, m_iBlockLength, m_fSampleRate);
    }
    m_bIsInitialized = true;

    return Error_t::kNoError;
}

bool CFeatureIf::isFeatureSpectral_(Feature_t eFeatureIdx)
{
    return (eFeatureIdx <= kFeatureSpectralTonalPowerRatio) ? true : false;
}
