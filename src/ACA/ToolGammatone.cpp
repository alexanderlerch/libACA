

#include <cmath>
#include <complex>

#include "AudioFileIf.h"
#include "Filter.h"

#include "ToolPreProc.h"
#include "ToolBlockAudio.h"

#include "ToolGammatone.h"

//#define ACA_USE_DOUBLE

/*! \brief class for a single gammtone filter
*/
class CGammatone
{
public:
    CGammatone();
    virtual ~CGammatone();

    /*! initializes a Gammatone instance (one band)
    \param fFreqCenter center frequency
    \param fSampleRate sample rate
    \return Error_t
    */
    Error_t init(float fFreqCenter, float fSampleRate);

    /*! performs the Gammatone filter computation
    \param pfOut filter result (user-allocated, to be written, length iNumSamples)
    \param pfIn input data of length iNumSamples
    \param iNumSamples length of buffers
    return Error_t
    */
    Error_t process(float *pfOut, const float *pfIn, long long iNumSamples);

    /*! clears internal buffers and sets parameters to default
    \return Error_t
    */
    Error_t reset();

private:
    CGammatone(const CGammatone &that);
    CGammatone &operator=(const CGammatone &c);

    /*! compute the filter coeffs
    \param iOrder filter order
    */
    void calcFilterCoeffs_(int iOrder = 1);

    enum
    {
        kNumCoeffs = 3,
        kNumFilters = 4
    };

    float m_fFreqCenter = 0;  //!< center frequency of this filter
    float m_fSampleRate = 0; //!< sample rate in Hz

#ifdef ACA_USE_DOUBLE
    double m_aafCoeffB[kNumFilters][kNumCoeffs] = { { 0 } };
    double m_aafCoeffA[kNumFilters][kNumCoeffs] = { { 0 } };
    CFilter<double> *m_apCFilter[kNumFilters] = { 0 };
#else
    float m_aafCoeffB[kNumFilters][kNumCoeffs] = { { 0 } }; //!< FIR coefficients
    float m_aafCoeffA[kNumFilters][kNumCoeffs] = { { 0 } }; //!< IIR coefficients
    CFilter<float> *m_apCFilter[kNumFilters] = { 0 }; //!< number crunching (multiple instances b/c cascaded)
#endif // ACA_USE_DOUBLE
    bool m_bIsInitialized = false; //!< flag indicating that this is initialized
};


#ifdef ACA_USE_DOUBLE
CGammatone::CGammatone()
{
    for (auto c = 0; c < kNumFilters; c++)
        m_apCFilter[c] = new CFilter<double>();
}
#else
CGammatone::CGammatone()
{
    for (auto c = 0; c < kNumFilters; c++)
        m_apCFilter[c] = new CFilter<float>();
}
#endif // ACA_USE_DOUBLE

CGammatone::~CGammatone()
{
    for (auto c = 0; c < kNumFilters; c++)
        delete m_apCFilter[c];
}

Error_t CGammatone::init(float fFreqCenter, float fSampleRate)
{
    m_fFreqCenter = fFreqCenter;
    m_fSampleRate = fSampleRate;

    calcFilterCoeffs_();

    m_bIsInitialized = true;

    return Error_t::kNoError;
}

Error_t CGammatone::process(float *pfOut, const float *pfIn, long long iNumSamples)
{
    if (!pfOut || !pfIn || iNumSamples <= 0)
        return Error_t::kFunctionInvalidArgsError;

    if (!m_bIsInitialized)
        return Error_t::kFunctionIllegalCallError;

#ifdef ACA_USE_DOUBLE
    for (auto i = 0; i < iNumSamples; i++)
    {
        double fTmp = pfIn[i];
        for (auto c = 0; c < kNumFilters; c++)
            m_apCFilter[c]->process(&fTmp, &fTmp, 1);
        pfOut[i] = static_cast<float>(fTmp);
    }
#else
    CVector::copy(pfOut, pfIn, iNumSamples);
    for (auto c = 0; c < kNumFilters; c++)
        m_apCFilter[c]->process(pfOut, pfOut, iNumSamples);
#endif // ACA_USE_DOUBLE

    return Error_t::kNoError;
}

Error_t CGammatone::reset()
{
    m_fFreqCenter = 0.F;

    for (auto c = 0; c < kNumFilters; c++)
    {
        m_apCFilter[c]->reset();
        delete m_apCFilter[c];
        m_apCFilter[c] = 0;
    }

    return Error_t::kNoError;
}

#ifdef ACA_USE_DOUBLE
void CGammatone::calcFilterCoeffs_(int iOrder)
{
    assert(m_fSampleRate > 0);
    assert(m_fFreqCenter >= 0);

    const double f2Pi = 2 * M_PI;
    const double fEarQ = 9.26449;
    const double fBandwidth = 24.7;
    const double B = 1.019 * f2Pi * std::pow(std::pow(m_fFreqCenter / fEarQ, iOrder) + std::pow(fBandwidth, iOrder), 1. / iOrder);

    double fArg = f2Pi * m_fFreqCenter / m_fSampleRate;
    double fCos = std::cos(fArg);
    double fSin = std::sin(fArg);
    double fExp = std::exp(B / m_fSampleRate);
    double fSqrtA = 2 * std::sqrt(3. + std::pow(2., 1.5F));
    double fSqrtS = 2 * std::sqrt(3. - std::pow(2., 1.5F));
    double fGain = 0;

    for (auto c = 0; c < kNumFilters; c++)
    {
        m_aafCoeffB[c][0] = 1. / m_fSampleRate;
        m_aafCoeffB[c][2] = 0.;

        m_aafCoeffA[c][0] = 1.;
        m_aafCoeffA[c][1] = -2. * fCos / fExp;
        m_aafCoeffA[c][2] = std::exp(-2. * B / m_fSampleRate);
    }

    m_aafCoeffB[0][1] = -(2. / m_fSampleRate * fCos / fExp + fSqrtA / m_fSampleRate * fSin / fExp) / 2.;
    m_aafCoeffB[1][1] = -(2. / m_fSampleRate * fCos / fExp - fSqrtA / m_fSampleRate * fSin / fExp) / 2.;
    m_aafCoeffB[2][1] = -(2. / m_fSampleRate * fCos / fExp + fSqrtS / m_fSampleRate * fSin / fExp) / 2.;
    m_aafCoeffB[3][1] = -(2. / m_fSampleRate * fCos / fExp - fSqrtS / m_fSampleRate * fSin / fExp) / 2.;

    // apply gain to first cascade
    {
        fSqrtA *= .5;
        fSqrtS *= .5;

        double fScale = 2 * 2 * 2 * 2 / (m_fSampleRate * m_fSampleRate * m_fSampleRate * m_fSampleRate);
        std::complex<double> num1(-std::cos(2. * fArg), -std::sin(2 * fArg));
        std::complex<double> num2(std::cos(fArg) * std::exp(-B / m_fSampleRate), std::sin(fArg) * std::exp(-B / m_fSampleRate));
        std::complex<double> denom(-2. / std::exp(2. * B / m_fSampleRate) + 2. / fExp + std::cos(2 * fArg) * (2. / fExp - 2.), std::sin(2 * fArg) * (2. / fExp - 2.));

        auto numerator = fScale *
            (num1 + num2 * (fCos - fSqrtS * fSin)) *
            (num1 + num2 * (fCos + fSqrtS * fSin)) *
            (num1 + num2 * (fCos - fSqrtA * fSin)) *
            (num1 + num2 * (fCos + fSqrtA * fSin));
        auto denominator = denom * denom * denom * denom;
        fGain = std::abs(numerator / denominator);
    }
    for (auto k = 0; k < kNumCoeffs; k++)
        m_aafCoeffB[0][k] /= fGain;

    for (auto c = 0; c < kNumFilters; c++)
        m_apCFilter[c]->init(m_aafCoeffB[c], m_aafCoeffA[c], 3);
}
#else
void CGammatone::calcFilterCoeffs_(int iOrder)
{
    assert(m_fSampleRate > 0);
    assert(m_fFreqCenter >= 0);

    const float f2Pi = static_cast<float>(2 * M_PI);
    const float fEarQ = 9.26449F;
    const float fBandwidth = 24.7F;
    const float B = static_cast<float>(1.019 * f2Pi * std::pow(std::pow(m_fFreqCenter / fEarQ, iOrder) + std::pow(fBandwidth, iOrder), 1. / iOrder));

    float fArg = f2Pi * m_fFreqCenter / m_fSampleRate;
    float fCos = std::cos(fArg);
    float fSin = std::sin(fArg);
    float fExp = std::exp(B / m_fSampleRate);
    float fSqrtA = 2 * std::sqrt(3.F + std::pow(2.F, 1.5F));
    float fSqrtS = 2 * std::sqrt(3.F - std::pow(2.F, 1.5F));
    float fGain = 0;

    for (auto c = 0; c < kNumFilters; c++)
    {
        m_aafCoeffB[c][0] = 1.F / m_fSampleRate;
        m_aafCoeffB[c][2] = 0.F;

        m_aafCoeffA[c][0] = 1.F;
        m_aafCoeffA[c][1] = -2.F * fCos / fExp;
        m_aafCoeffA[c][2] = std::exp(-2.F * B / m_fSampleRate);
    }

    m_aafCoeffB[0][1] = -(2.F / m_fSampleRate * fCos / fExp + fSqrtA / m_fSampleRate * fSin / fExp) / 2.F;
    m_aafCoeffB[1][1] = -(2.F / m_fSampleRate * fCos / fExp - fSqrtA / m_fSampleRate * fSin / fExp) / 2.F;
    m_aafCoeffB[2][1] = -(2.F / m_fSampleRate * fCos / fExp + fSqrtS / m_fSampleRate * fSin / fExp) / 2.F;
    m_aafCoeffB[3][1] = -(2.F / m_fSampleRate * fCos / fExp - fSqrtS / m_fSampleRate * fSin / fExp) / 2.F;

    // apply gain to first cascade
    {
        fSqrtA *= .5F;
        fSqrtS *= .5F;

        float fScale = 2 * 2 * 2 * 2 / (m_fSampleRate * m_fSampleRate * m_fSampleRate * m_fSampleRate);
        std::complex<float> num1(-std::cos(2.F * fArg), -std::sin(2 * fArg));
        std::complex<float> num2(std::cos(fArg) * std::exp(-B / m_fSampleRate), std::sin(fArg) * std::exp(-B / m_fSampleRate));
        std::complex<float> denom(-2.F / std::exp(2.F * B / m_fSampleRate) + 2.F / fExp + std::cos(2 * fArg) * (2.F / fExp - 2.F), std::sin(2 * fArg) * (2.F / fExp - 2.F));

        auto numerator = fScale *
            (num1 + num2 * (fCos - fSqrtS * fSin)) *
            (num1 + num2 * (fCos + fSqrtS * fSin)) *
            (num1 + num2 * (fCos - fSqrtA * fSin)) *
            (num1 + num2 * (fCos + fSqrtA * fSin));
        auto denominator = denom * denom * denom * denom;
        fGain = std::abs(numerator / denominator);
    }
    CVector::mulC_I(m_aafCoeffB[0], 1.F / fGain, kNumCoeffs);

    for (auto c = 0; c < kNumFilters; c++)
        m_apCFilter[c]->init(m_aafCoeffB[c], m_aafCoeffA[c], 3);
}
#endif // ACA_USE_DOUBLE


/*! \brief class for computation of the filterbank outputs from a file
*/
class CGammaToneFbFromFile : public CGammaToneFbIf
{
public:
    CGammaToneFbFromFile(const std::string &strAudioFilePath, int iNumBands, float fStartInHz);

    virtual ~CGammaToneFbFromFile()
    {
        delete m_pCNormalize;
        m_pCNormalize = 0;

        m_pCAudioFile->closeFile();
        CAudioFileIf::destroy(m_pCAudioFile);
    };

private:
    CAudioFileIf *m_pCAudioFile;
};

CGammaToneFbFromFile::CGammaToneFbFromFile(const std::string &strAudioFilePath, int iNumBands, float fStartInHz) :
    m_pCAudioFile(0)

{
    this->reset_();

    m_iNumBands = iNumBands;
    m_fStartInHz = fStartInHz;

    CAudioFileIf::FileSpec_t stFileSpec;
    CAudioFileIf::create(m_pCAudioFile);
    m_pCAudioFile->openFile(strAudioFilePath, CAudioFileIf::kFileRead);
    m_pCAudioFile->getFileSpec(stFileSpec);
    m_fSampleRate = stFileSpec.fSampleRateInHz;

    CBlockAudioIf::create(m_pCBlockAudio, m_pCAudioFile, m_iBlockLength, m_iBlockLength);

    m_pCNormalize = new CNormalizeAudio(m_pCAudioFile);

    init_();
}


/*! \brief class for computation of the filterbank outputs from a vector of audio data
*/
class CGammaToneFbFromVector : public CGammaToneFbIf
{
public:
    CGammaToneFbFromVector(const float *pfAudio, long long iNumSamples, float fSampleRate, int iNumBands, float fStartInHz);
    virtual ~CGammaToneFbFromVector() {};
};

CGammaToneFbFromVector::CGammaToneFbFromVector(const float *pfAudio, long long iNumSamples, float fSampleRate, int iNumBands, float fStartInHz)
{
    m_iNumBands = iNumBands;
    m_fSampleRate = fSampleRate;
    m_fStartInHz = fStartInHz;

    CBlockAudioIf::create(m_pCBlockAudio, pfAudio, iNumSamples, m_iBlockLength, m_iBlockLength, m_fSampleRate);

    m_pCNormalize = new CNormalizeAudio(pfAudio, iNumSamples);

    init_();
}

/*! \brief class for computation of the filterbank outputs in real time block by block
*/
class CGammaToneFbRealTime : public CGammaToneFbIf
{
public:
    CGammaToneFbRealTime(float fSampleRate, int iNumBands, float fStartInHz);
    virtual ~CGammaToneFbRealTime() {};
};

CGammaToneFbRealTime::CGammaToneFbRealTime(float fSampleRate, int iNumBands, float fStartInHz)
{
    m_iNumBands = iNumBands;
    m_fSampleRate = fSampleRate;
    m_fStartInHz = fStartInHz;

    init_();
}


/////////////////////////////////////////////////////////////////////////////////
// base class
CGammaToneFbIf::CGammaToneFbIf()
{
    reset_();
}

inline CGammaToneFbIf::~CGammaToneFbIf()
{
    reset_();
}

Error_t CGammaToneFbIf::create(CGammaToneFbIf *&pCInstance, const std::string &strAudioFilePath, int iNumBands, float fStartInHz)
{
    if (strAudioFilePath.empty())
        return Error_t::kFunctionInvalidArgsError;
    if (iNumBands <= 0)
        return Error_t::kFunctionInvalidArgsError;
    if (fStartInHz <= 0)
        return Error_t::kFunctionInvalidArgsError;

    pCInstance = new CGammaToneFbFromFile(strAudioFilePath, iNumBands, fStartInHz);


    return Error_t::kNoError;
}

Error_t CGammaToneFbIf::create(CGammaToneFbIf *&pCInstance, const float *pfAudio, long long iNumSamples, float fSampleRate, int iNumBands, float fStartInHz)
{
    if (!pfAudio)
        return Error_t::kFunctionInvalidArgsError;
    if (iNumSamples <= 0)
        return Error_t::kFunctionInvalidArgsError;
    if (fSampleRate <= 0)
        return Error_t::kFunctionInvalidArgsError;
    if (iNumBands <= 0)
        return Error_t::kFunctionInvalidArgsError;
    if (fStartInHz <= 0 || fStartInHz >= fSampleRate / 2)
        return Error_t::kFunctionInvalidArgsError;

    pCInstance = new CGammaToneFbFromVector(pfAudio, iNumSamples, fSampleRate, iNumBands, fStartInHz);

    return Error_t::kNoError;
}

Error_t CGammaToneFbIf::create(CGammaToneFbIf *&pCInstance, float fSampleRate, int iNumBands, float fStartInHz)
{
    if (fSampleRate <= 0)
        return Error_t::kFunctionInvalidArgsError;
    if (iNumBands <= 0)
        return Error_t::kFunctionInvalidArgsError;
    if (fStartInHz <= 0 || fStartInHz >= fSampleRate / 2)
        return Error_t::kFunctionInvalidArgsError;

    pCInstance = new CGammaToneFbRealTime(fSampleRate, iNumBands, fStartInHz);

    return Error_t::kNoError;
}

Error_t CGammaToneFbIf::destroy(CGammaToneFbIf *&pCInstance)
{
    delete pCInstance;
    pCInstance = 0;

    return Error_t::kNoError;
}

Error_t CGammaToneFbIf::getOutputDimensions(long long &iNumRows, long long &iNumCols) const
{
    if (!m_bIsInitialized)
    {
        iNumRows = 0;
        iNumCols = 0;
        return Error_t::kFunctionIllegalCallError;
    }

    iNumRows = m_iNumBands;
    iNumCols = m_pCBlockAudio->getLengthInSamples();

    return Error_t::kNoError;
}

float CGammaToneFbIf::getCenterFreq(int iBandIdx) const
{
    return compMidFreqs_(m_fStartInHz, m_fSampleRate / 2, iBandIdx);
}

Error_t CGammaToneFbIf::process(float **ppfOut, const float *pfIn /*= 0*/, long long iNumSamples /*= 0*/)
{
    if (!m_bIsInitialized)
        return Error_t::kFunctionIllegalCallError;
    if (!ppfOut)
        return Error_t::kFunctionInvalidArgsError;
    if (!ppfOut[0])
        return Error_t::kFunctionInvalidArgsError;

    // special case of block by block processing - no normalization, no blocking
    if (pfIn)
    {
        assert(iNumSamples > 0);
        for (auto i = 0; i < m_iNumBands; i++)
        {
            assert(ppfOut[i]);
            m_ppCGammatone[i]->process(ppfOut[i], pfIn, iNumSamples);
        }
        return Error_t::kNoError;
    }

    assert(m_pCBlockAudio);

    auto iNumBlocks = m_pCBlockAudio->getNumBlocks();

    for (auto n = 0; n < iNumBlocks; n++)
    {
        // retrieve the next audio block
        int iNumFrames = m_pCBlockAudio->getNextBlock(m_pfProcBuff);

        // normalize if specified
        if (m_pCNormalize)
            m_pCNormalize->normalizeBlock(m_pfProcBuff, m_iBlockLength);

        for (auto i = 0; i < m_iNumBands; i++)
        {
            assert(ppfOut[i]);
            m_ppCGammatone[i]->process(&ppfOut[i][n * m_iBlockLength], m_pfProcBuff, iNumFrames);
        }
    }

    return Error_t::kNoError;
}

Error_t CGammaToneFbIf::reset_()
{
    CVector::free(m_pfProcBuff);

    delete m_pCNormalize;
    m_pCNormalize = 0;

    CBlockAudioIf::destroy(m_pCBlockAudio);
    for (auto i = 0; i < m_iNumBands; i++)
        delete m_ppCGammatone[i];
    CVector::free(m_ppCGammatone);

    m_bIsInitialized = false;

    return Error_t::kNoError;
}

Error_t CGammaToneFbIf::init_()
{
    // allocate processing memory
    CVector::alloc(m_pfProcBuff, m_iBlockLength);

    CVector::alloc(m_ppCGammatone, m_iNumBands);
    for (auto i = 0; i < m_iNumBands; i++)
    {
        m_ppCGammatone[i] = new CGammatone();
        m_ppCGammatone[i]->init(compMidFreqs_(m_fStartInHz, m_fSampleRate / 2, i), m_fSampleRate);
    }

    m_bIsInitialized = true;

    return Error_t::kNoError;
}


inline float CGammaToneFbIf::compMidFreqs_(float fFreqLow, float fFreqHigh, int k) const
{
    const float fEarQ = 9.26449F;
    const float fBandwidth = 24.7F;

    return -(fEarQ * fBandwidth) + std::exp((m_iNumBands - k) * (-std::log(fFreqHigh + fEarQ * fBandwidth) + std::log(fFreqLow + fEarQ * fBandwidth)) / m_iNumBands) * (fFreqHigh + fEarQ * fBandwidth);
}

