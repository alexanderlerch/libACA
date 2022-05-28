
#define _USE_MATH_DEFINES
#include <cmath>
#include <complex>

#include "AudioFileIf.h"
#include "ToolPreProc.h"
#include "ToolBlockAudio.h"
#include "Filter.h"

#include "ToolGammatone.h"

CGammatone::CGammatone()
{
    for (auto c = 0; c < kNumFilters; c++)
        m_apCFilter[c] = new CFilter<float>();
}

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

Error_t CGammatone::process(float* pfOutput, const float* pfInput, long long iNumSamples)
{
    if (!pfOutput || !pfInput || iNumSamples <= 0)
        return Error_t::kFunctionInvalidArgsError;

    CVectorFloat::copy(pfOutput, pfInput, iNumSamples);
    for (auto c = 0; c < kNumFilters; c++)
        m_apCFilter[c]->process(pfOutput, pfOutput, iNumSamples);

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
    CVectorFloat::mulC_I(m_aafCoeffB[0], 1.F / fGain, kNumCoeffs);

    for (auto c = 0; c < kNumFilters; c++)
        m_apCFilter[c]->init(m_aafCoeffB[c], m_aafCoeffA[c], 3);
}


/////////////////////////////////////////////////////////////////////////////////
// file extraction
class CGammaToneFbFromFile : public CGammaToneFbIf
{
public:
    CGammaToneFbFromFile(const std::string& strAudioFilePath, int iNumBands, float fStartInHz);

    virtual ~CGammaToneFbFromFile()
    {
        delete m_pCNormalize;
        m_pCNormalize = 0;

        m_pCAudioFile->closeFile();
        CAudioFileIf::destroy(m_pCAudioFile);
    };

private:
    CAudioFileIf* m_pCAudioFile;
};

CGammaToneFbFromFile::CGammaToneFbFromFile(const std::string& strAudioFilePath, int iNumBands, float fStartInHz) :
    m_pCAudioFile(0)
    
{
    this->reset_();

    m_iNumBands = iNumBands;

    CAudioFileIf::FileSpec_t stFileSpec;
    CAudioFileIf::create(m_pCAudioFile);
    m_pCAudioFile->openFile(strAudioFilePath, CAudioFileIf::kFileRead);
    m_pCAudioFile->getFileSpec(stFileSpec);
    m_fSampleRate = stFileSpec.fSampleRateInHz;

    CBlockAudioIf::create(m_pCBlockAudio, m_pCAudioFile, m_iBlockLength, m_iBlockLength);

    m_pCNormalize = new CNormalizeAudio(m_pCAudioFile);

    init_(fStartInHz);
}


/////////////////////////////////////////////////////////////////////////////////
// vector extraction
class CGammaToneFbFromVector : public CGammaToneFbIf
{
public:
    CGammaToneFbFromVector(const float* pfAudio, long long iNumFrames, float fSampleRate, int iNumBands, float fStartInHz);
    virtual ~CGammaToneFbFromVector() {};
};

CGammaToneFbFromVector::CGammaToneFbFromVector(const float* pfAudio, long long iNumFrames, float fSampleRate, int iNumBands, float fStartInHz) 
{
    m_iNumBands = iNumBands;
    m_fSampleRate = fSampleRate;

    CBlockAudioIf::create(m_pCBlockAudio, pfAudio, iNumFrames, m_iBlockLength, m_iBlockLength, m_fSampleRate);

    m_pCNormalize = new CNormalizeAudio(pfAudio, iNumFrames);

    init_(fStartInHz);
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

Error_t CGammaToneFbIf::create(CGammaToneFbIf*& pCInstance, const std::string& strAudioFilePath, int iNumBands, float fStartInHz)
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

Error_t CGammaToneFbIf::create(CGammaToneFbIf*& pCInstance, const float* pfAudio, long long iNumFrames, float fSampleRate, int iNumBands, float fStartInHz)
{
    if (!pfAudio)
        return Error_t::kFunctionInvalidArgsError;
    if (iNumFrames <= 0)
        return Error_t::kFunctionInvalidArgsError;
    if (fSampleRate <= 0)
        return Error_t::kFunctionInvalidArgsError;
    if (iNumBands <= 0)
        return Error_t::kFunctionInvalidArgsError;
    if (fStartInHz <= 0 || fStartInHz >= fSampleRate/2)
        return Error_t::kFunctionInvalidArgsError;

    pCInstance = new CGammaToneFbFromVector( pfAudio, iNumFrames, fSampleRate, iNumBands, fStartInHz);

    return Error_t::kNoError;
}

Error_t CGammaToneFbIf::destroy(CGammaToneFbIf*& pCInstance)
{
    delete pCInstance;
    pCInstance = 0;

    return Error_t::kNoError;
}

Error_t CGammaToneFbIf::getOutputDimensions(long long& iNumRows, long long& iNumColumns) const
{
    if (!m_bIsInitialized)
    {
        iNumRows = 0;
        iNumColumns = 0;
        return Error_t::kFunctionIllegalCallError;
    }

    iNumRows = m_iNumBands;
    iNumColumns = m_pCBlockAudio->getLengthInSamples();

    return Error_t::kNoError;
}

Error_t CGammaToneFbIf::process(float** ppfOutput, const float* pfInput)
{
    if (!m_bIsInitialized)
        return Error_t::kFunctionIllegalCallError;
    if (!ppfOutput)
        return Error_t::kFunctionInvalidArgsError;
    if (!ppfOutput[0])
        return Error_t::kFunctionInvalidArgsError;
    if (!pfInput)
        return Error_t::kFunctionInvalidArgsError;

    assert(m_pCBlockAudio);
    assert(m_pCNormalize);

    auto iNumBlocks = m_pCBlockAudio->getNumBlocks();

    for (auto n = 0; n < iNumBlocks; n++)
    {
        // retrieve the next audio block
        m_pCBlockAudio->getNextBlock(m_pfProcessBuff);

        // normalize if specified
        if (m_pCNormalize)
            m_pCNormalize->normalizeBlock(m_pfProcessBuff, m_iBlockLength);

        for (auto i = 0; i < m_iNumBands; i++)
        {
            assert(ppfOutput[i]);
            m_ppCGammatone[i]->process(&ppfOutput[i][n*m_iBlockLength], m_pfProcessBuff, m_iBlockLength);
        }
    }

    return Error_t::kNoError;
}

Error_t CGammaToneFbIf::reset_()
{
    delete[] m_pfProcessBuff;
    m_pfProcessBuff = 0;

    delete m_pCNormalize;
    m_pCNormalize = 0;

    CBlockAudioIf::destroy(m_pCBlockAudio);
    for (auto i = 0; i < m_iNumBands; i++)
        delete m_ppCGammatone[i];
    delete[] m_ppCGammatone;

    m_bIsInitialized = false;

    return Error_t::kNoError;
}

Error_t CGammaToneFbIf::init_(float fStartInHz)
{
    // allocate processing memory
    m_pfProcessBuff = new float[m_iBlockLength];

    m_ppCGammatone = new CGammatone* [m_iNumBands];
    for (auto i = 0; i < m_iNumBands; i++)
    {
        m_ppCGammatone[i] = new CGammatone();
        m_ppCGammatone[i]->init(compMidFreqs_(fStartInHz, m_fSampleRate/2, i), m_fSampleRate);
    }

    m_bIsInitialized = true;

    return Error_t::kNoError;
}


inline float CGammaToneFbIf::compMidFreqs_(float fFreqLow, float fFreqHigh, int k)
{
    const float fEarQ = 9.26449F;
    const float fBandwidth = 24.7F;

    return -(fEarQ * fBandwidth) + std::exp(k * (-std::log(fFreqHigh + fEarQ * fBandwidth) + std::log(fFreqLow + fEarQ * fBandwidth)) / m_iNumBands) * (fFreqHigh + fEarQ * fBandwidth);
}

