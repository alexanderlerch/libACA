
#include "Vector.h"
#include "Util.h"

#include "FeatureFromBlock.h"

class CFeatureSpectralCentroid : public CFeatureFromBlockIf
{
public:
    CFeatureSpectralCentroid(int iDataLength, float fSampleRate)
    {
        m_iDataLength = iDataLength;
        m_fSampleRate = fSampleRate;
    };

    virtual ~CFeatureSpectralCentroid() {};

    Error_t calcFeatureFromBlock(float* pfFeature, const float* pfInput) override
    {
        *pfFeature = compFeatureSpectralCentroid(pfInput, m_iDataLength, m_fSampleRate);

        return Error_t::kNoError;
    };

private:
    CFeatureSpectralCentroid() {};
    CFeatureSpectralCentroid(const CFeatureSpectralCentroid& that);
};

class CFeatureSpectralCrestFactor : public CFeatureFromBlockIf
{
public:
    CFeatureSpectralCrestFactor(int iDataLength, float fSampleRate)
    {
        m_iDataLength = iDataLength;
        m_fSampleRate = fSampleRate;
    };

    virtual ~CFeatureSpectralCrestFactor() {};

    Error_t calcFeatureFromBlock(float* pfFeature, const float* pfInput) override
    {
        *pfFeature = compFeatureSpectralCrestFactor(pfInput, m_iDataLength, m_fSampleRate);

        return Error_t::kNoError;
    };

private:
    CFeatureSpectralCrestFactor() {};
    CFeatureSpectralCrestFactor(const CFeatureSpectralCrestFactor& that);
};

class CFeatureSpectralDecrease : public CFeatureFromBlockIf
{
public:
    CFeatureSpectralDecrease(int iDataLength, float fSampleRate)
    {
        m_iDataLength = iDataLength;
        m_fSampleRate = fSampleRate;
    };

    virtual ~CFeatureSpectralDecrease() {};

    Error_t calcFeatureFromBlock(float* pfFeature, const float* pfInput) override
    {
        *pfFeature = compFeatureSpectralDecrease(pfInput, m_iDataLength, m_fSampleRate);

        return Error_t::kNoError;
    };

private:
    CFeatureSpectralDecrease() {};
    CFeatureSpectralDecrease(const CFeatureSpectralDecrease& that);
};

class CFeatureSpectralFlatness : public CFeatureFromBlockIf
{
public:
    CFeatureSpectralFlatness(int iDataLength, float fSampleRate)
    {
        m_iDataLength = iDataLength;
        m_fSampleRate = fSampleRate;
    };

    virtual ~CFeatureSpectralFlatness() {};

    Error_t calcFeatureFromBlock(float* pfFeature, const float* pfInput) override
    {
        *pfFeature = compFeatureSpectralFlatness(pfInput, m_iDataLength, m_fSampleRate);

        return Error_t::kNoError;
    };

private:
    CFeatureSpectralFlatness() {};
    CFeatureSpectralFlatness(const CFeatureSpectralFlatness& that);
};

//class CFeatureSpectralFlux : public CFeatureFromBlockIf
//{
//public:
//    CFeatureSpectralFlux(int iDataLength, float fSampleRate)
//    {
//        m_iDataLength = iDataLength;
//        m_fSampleRate = fSampleRate;
//    };
//
//    virtual ~CFeatureSpectralFlux() {};
//
//    Error_t calcFeatureFromBlock(float* pfFeature, const float* pfInput) override
//    {
//        *pfFeature = compFeatureSpectralFlux(pfInput, m_iDataLength, m_fSampleRate);
//
//        return Error_t::kNoError;
//    };
//
//private:
//    CFeatureSpectralFlux() {};
//    CFeatureSpectralFlux(const CFeatureSpectralFlux& that);
//};

class CFeatureSpectralKurtosis : public CFeatureFromBlockIf
{
public:
    CFeatureSpectralKurtosis(int iDataLength, float fSampleRate)
    {
        m_iDataLength = iDataLength;
        m_fSampleRate = fSampleRate;
    };

    virtual ~CFeatureSpectralKurtosis() {};

    Error_t calcFeatureFromBlock(float* pfFeature, const float* pfInput) override
    {
        *pfFeature = compFeatureSpectralKurtosis(pfInput, m_iDataLength, m_fSampleRate);

        return Error_t::kNoError;
    };

private:
    CFeatureSpectralKurtosis() {};
    CFeatureSpectralKurtosis(const CFeatureSpectralKurtosis& that);
};

class CFeatureSpectralRolloff : public CFeatureFromBlockIf
{
public:
    CFeatureSpectralRolloff(int iDataLength, float fSampleRate)
    {
        m_iDataLength = iDataLength;
        m_fSampleRate = fSampleRate;
    };

    virtual ~CFeatureSpectralRolloff() {};

    Error_t calcFeatureFromBlock(float* pfFeature, const float* pfInput) override
    {
        *pfFeature = compFeatureSpectralRolloff(pfInput, m_iDataLength, m_fSampleRate);

        return Error_t::kNoError;
    };

private:
    CFeatureSpectralRolloff() {};
    CFeatureSpectralRolloff(const CFeatureSpectralRolloff& that);
};

class CFeatureSpectralSkewness : public CFeatureFromBlockIf
{
public:
    CFeatureSpectralSkewness(int iDataLength, float fSampleRate)
    {
        m_iDataLength = iDataLength;
        m_fSampleRate = fSampleRate;
    };

    virtual ~CFeatureSpectralSkewness() {};

    Error_t calcFeatureFromBlock(float* pfFeature, const float* pfInput) override
    {
        *pfFeature = compFeatureSpectralSkewness(pfInput, m_iDataLength, m_fSampleRate);

        return Error_t::kNoError;
    };

private:
    CFeatureSpectralSkewness() {};
    CFeatureSpectralSkewness(const CFeatureSpectralSkewness& that);
};

class CFeatureSpectralSlope : public CFeatureFromBlockIf
{
public:
    CFeatureSpectralSlope(int iDataLength, float fSampleRate)
    {
        m_iDataLength = iDataLength;
        m_fSampleRate = fSampleRate;
    };

    virtual ~CFeatureSpectralSlope() {};

    Error_t calcFeatureFromBlock(float* pfFeature, const float* pfInput) override
    {
        *pfFeature = compFeatureSpectralSlope(pfInput, m_iDataLength, m_fSampleRate);

        return Error_t::kNoError;
    };

private:
    CFeatureSpectralSlope() {};
    CFeatureSpectralSlope(const CFeatureSpectralSlope& that);
};

class CFeatureSpectralSpread : public CFeatureFromBlockIf
{
public:
    CFeatureSpectralSpread(int iDataLength, float fSampleRate)
    {
        m_iDataLength = iDataLength;
        m_fSampleRate = fSampleRate;
    };

    virtual ~CFeatureSpectralSpread() {};

    Error_t calcFeatureFromBlock(float* pfFeature, const float* pfInput) override
    {
        *pfFeature = compFeatureSpectralSpread(pfInput, m_iDataLength, m_fSampleRate);

        return Error_t::kNoError;
    };

private:
    CFeatureSpectralSpread() {};
    CFeatureSpectralSpread(const CFeatureSpectralSpread& that);
};

class CFeatureSpectralTonalPowerRatio : public CFeatureFromBlockIf
{
public:
    CFeatureSpectralTonalPowerRatio(int iDataLength, float fSampleRate)
    {
        m_iDataLength = iDataLength;
        m_fSampleRate = fSampleRate;
    };

    virtual ~CFeatureSpectralTonalPowerRatio() {};

    Error_t calcFeatureFromBlock(float* pfFeature, const float* pfInput) override
    {
        *pfFeature = compFeatureSpectralTonalPowerRatio(pfInput, m_iDataLength, m_fSampleRate);

        return Error_t::kNoError;
    };

private:
    CFeatureSpectralTonalPowerRatio() {};
    CFeatureSpectralTonalPowerRatio(const CFeatureSpectralTonalPowerRatio& that);
};

class CFeatureTimePeakEnvelope : public CFeatureFromBlockIf
{
public:
    CFeatureTimePeakEnvelope(int iDataLength, float fSampleRate)
    {
        m_iDataLength = iDataLength;
        m_fSampleRate = fSampleRate;
    };

    virtual ~CFeatureTimePeakEnvelope() {};

    Error_t calcFeatureFromBlock(float* pfFeature, const float* pfInput) override
    {
        *pfFeature = compFeatureTimePeakEnvelope(pfInput, m_iDataLength, m_fSampleRate);

        return Error_t::kNoError;
    };

private:
    CFeatureTimePeakEnvelope() {};
    CFeatureTimePeakEnvelope(const CFeatureTimePeakEnvelope& that);
};

class CFeatureTimeRms : public CFeatureFromBlockIf
{
public:
    CFeatureTimeRms(int iDataLength, float fSampleRate)
    {
        m_iDataLength = iDataLength;
        m_fSampleRate = fSampleRate;
    };

    virtual ~CFeatureTimeRms() {};

    Error_t calcFeatureFromBlock(float* pfFeature, const float* pfInput) override
    {
        *pfFeature = compFeatureTimeRms(pfInput, m_iDataLength, m_fSampleRate);

        return Error_t::kNoError;
    };

private:
    CFeatureTimeRms() {};
    CFeatureTimeRms(const CFeatureTimeRms& that);
};

class CFeatureTimeStd : public CFeatureFromBlockIf
{
public:
    CFeatureTimeStd(int iDataLength, float fSampleRate)
    {
        m_iDataLength = iDataLength;
        m_fSampleRate = fSampleRate;
    };

    virtual ~CFeatureTimeStd() {};

    Error_t calcFeatureFromBlock(float* pfFeature, const float* pfInput) override
    {
        *pfFeature = compFeatureTimeStd(pfInput, m_iDataLength, m_fSampleRate);

        return Error_t::kNoError;
    };

private:
    CFeatureTimeStd() {};
    CFeatureTimeStd(const CFeatureTimeStd& that);
};

class CFeatureTimeZeroCrossingRate : public CFeatureFromBlockIf
{
public:
    CFeatureTimeZeroCrossingRate(int iDataLength, float fSampleRate)
    {
        m_iDataLength = iDataLength;
        m_fSampleRate = fSampleRate;
    };

    virtual ~CFeatureTimeZeroCrossingRate() {};

    Error_t calcFeatureFromBlock(float* pfFeature, const float* pfInput) override
    {
        *pfFeature = compFeatureTimeZeroCrossingRate(pfInput, m_iDataLength, m_fSampleRate);

        return Error_t::kNoError;
    };

private:
    CFeatureTimeZeroCrossingRate() {};
    CFeatureTimeZeroCrossingRate(const CFeatureTimeZeroCrossingRate& that);
};


/*! returns size of output feature (1 in most cases)
\return int
*/

Error_t CFeatureFromBlockIf::create(CFeatureFromBlockIf*& pCInstance, Feature_t eFeatureIdx, int iDataLength, float fSampleRate)
{
    assert(iDataLength > 0);
    assert(fSampleRate > 0);

    switch (eFeatureIdx)
    {
    case kFeatureSpectralCentroid:
        pCInstance = new CFeatureSpectralCentroid(iDataLength, fSampleRate);
        break;

    case kFeatureSpectralCrestFactor:
        pCInstance = new CFeatureSpectralCrestFactor(iDataLength, fSampleRate);
        break;

    case kFeatureSpectralDecrease:
        pCInstance = new CFeatureSpectralDecrease(iDataLength, fSampleRate);
        break;

    case kFeatureSpectralFlatness:
        pCInstance = new CFeatureSpectralFlatness(iDataLength, fSampleRate);
        break;

    //case kFeatureSpectralFlux:
    //    pCInstance = new CFeatureSpectralFlux(iDataLength, fSampleRate);
    //    break;

    case kFeatureSpectralKurtosis:
        pCInstance = new CFeatureSpectralKurtosis(iDataLength, fSampleRate);
        break;

    //case kFeatureSpectralMfccs:
    //    pCInstance = new CFeatureSpectralMfccs(iDataLength, fSampleRate);
    //    break;

    //case kFeatureSpectralPitchChroma:
    //    pCInstance = new CFeatureSpectralPitchChroma(iDataLength, fSampleRate);
    //    break;

    case kFeatureSpectralRolloff:
        pCInstance = new CFeatureSpectralRolloff(iDataLength, fSampleRate);
        break;

    case kFeatureSpectralSkewness:
        pCInstance = new CFeatureSpectralSkewness(iDataLength, fSampleRate);
        break;

    case kFeatureSpectralSlope:
        pCInstance = new CFeatureSpectralSlope(iDataLength, fSampleRate);
        break;

    case kFeatureSpectralSpread:
        pCInstance = new CFeatureSpectralSpread(iDataLength, fSampleRate);
        break;

    case kFeatureSpectralTonalPowerRatio:
        pCInstance = new CFeatureSpectralTonalPowerRatio(iDataLength, fSampleRate);
        break;

    //case kFeatureTimeAcfCoeff:
    //    pCInstance = new CFeatureTimeAcfCoeff(iDataLength, fSampleRate);
    //    break;

    //case kFeatureTimeMaxAcf:
    //    pCInstance = new CFeatureTimeMaxAcf(iDataLength, fSampleRate);
    //    break;

    case kFeatureTimePeakEnvelope:
        pCInstance = new CFeatureTimePeakEnvelope(iDataLength, fSampleRate);
        break;

    case kFeatureTimeRms:
        pCInstance = new CFeatureTimeRms(iDataLength, fSampleRate);
        break;

    case kFeatureTimeStd:
        pCInstance = new CFeatureTimeStd(iDataLength, fSampleRate);
        break;

    case kFeatureTimeZeroCrossingRate:
        pCInstance = new CFeatureTimeZeroCrossingRate(iDataLength, fSampleRate);
        break;

    default:
        pCInstance = 0;
        return Error_t::kFunctionInvalidArgsError;
    }
    
    return Error_t::kNoError;
}

Error_t CFeatureFromBlockIf::destroy(CFeatureFromBlockIf*& pCInstance)
{
    delete pCInstance;

    return Error_t::kNoError;
}

inline int CFeatureFromBlockIf::getFeatureDimensions() const
{
    return 1;
}
