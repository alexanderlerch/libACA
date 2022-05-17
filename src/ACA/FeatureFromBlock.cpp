
#include "Vector.h"
#include "Util.h"

#include "ToolCcf.h"

#include "FeatureFromBlock.h"

const float CFeatureFromBlockIf::m_kfFloatThresh = 1e-30F;      //!< below this we just assume it's zero


class CFeatureSpectralFlux : public CFeatureFromBlockIf
{
public:
    CFeatureSpectralFlux(Feature_t eFeatureIdx, int iDataLength, float fSampleRate) : CFeatureFromBlockIf(eFeatureIdx, iDataLength, fSampleRate)
    {
        m_pfPrevSpec = new float[m_iDataLength];
        CVectorFloat::setZero(m_pfPrevSpec, m_iDataLength);
    };

    virtual ~CFeatureSpectralFlux() 
    {
        delete[] m_pfPrevSpec;
        m_pfPrevSpec = 0;
    };

    Error_t calcFeatureFromBlock(float* pfFeature, const float* pfInput) override
    {
        *pfFeature = compFeatureSpectralFlux(pfInput, m_pfPrevSpec, m_iDataLength, m_fSampleRate);

        CVectorFloat::copy(m_pfPrevSpec, pfInput, m_iDataLength);

        return Error_t::kNoError;
    };

private:
    CFeatureSpectralFlux() {};
    CFeatureSpectralFlux(const CFeatureSpectralFlux& that);

    float* m_pfPrevSpec = 0;
};

class CFeatureSpectralRolloff : public CFeatureFromBlockIf
{
public:
    CFeatureSpectralRolloff(Feature_t eFeatureIdx, int iDataLength, float fSampleRate) : CFeatureFromBlockIf(eFeatureIdx, iDataLength, fSampleRate) {};

    virtual ~CFeatureSpectralRolloff() {};

    Error_t calcFeatureFromBlock(float* pfFeature, const float* pfInput) override
    {
        *pfFeature = compFeatureSpectralRolloff(pfInput, m_iDataLength, m_fSampleRate, m_fKappa);

        return Error_t::kNoError;
    };

    bool hasAdditionalParam() const override
    {
        return true;
    }

    Error_t setAdditionalParam(float fParamValue) override
    {
        if (fParamValue <= 0 || fParamValue > 1)
            return Error_t::kNoError;

        m_fKappa = fParamValue;

        return Error_t::kNoError;
    }

private:
    CFeatureSpectralRolloff() {};
    CFeatureSpectralRolloff(const CFeatureSpectralRolloff& that);

    float m_fKappa = 0.85F;
};

class CFeatureSpectralTonalPowerRatio : public CFeatureFromBlockIf
{
public:
    CFeatureSpectralTonalPowerRatio(Feature_t eFeatureIdx, int iDataLength, float fSampleRate) : CFeatureFromBlockIf(eFeatureIdx, iDataLength, fSampleRate) {};

    virtual ~CFeatureSpectralTonalPowerRatio() {};

    Error_t calcFeatureFromBlock(float* pfFeature, const float* pfInput) override
    {
        *pfFeature = compFeatureSpectralTonalPowerRatio(pfInput, m_iDataLength, m_fSampleRate, m_fThresh);

        return Error_t::kNoError;
    };

    bool hasAdditionalParam() const override
    {
        return true;
    }

    Error_t setAdditionalParam(float fParamValue) override
    {
        if (fParamValue <= 0)
            return Error_t::kNoError;

        m_fThresh = fParamValue;

        return Error_t::kNoError;
    }

private:
    CFeatureSpectralTonalPowerRatio() {};
    CFeatureSpectralTonalPowerRatio(const CFeatureSpectralTonalPowerRatio& that);

    float m_fThresh = 5e-4F;
};

class CFeatureTimePeakEnvelope : public CFeatureFromBlockIf
{
public:
    CFeatureTimePeakEnvelope(Feature_t eFeatureIdx, int iDataLength, float fSampleRate) : CFeatureFromBlockIf(eFeatureIdx, iDataLength, fSampleRate) {};

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

class CFeatureTimeAcfCoeff : public CFeatureFromBlockIf
{
public:
    CFeatureTimeAcfCoeff(Feature_t eFeatureIdx, int iDataLength, float fSampleRate) : CFeatureFromBlockIf(eFeatureIdx, iDataLength, fSampleRate) {};

    virtual ~CFeatureTimeAcfCoeff() {};

    Error_t calcFeatureFromBlock(float* pfFeature, const float* pfInput) override
    {
        *pfFeature = compFeatureTimeAcfCoeff(pfInput, m_iDataLength, m_fSampleRate, m_iEta);

        return Error_t::kNoError;
    };

    bool hasAdditionalParam() const override
    {
        return true;
    }

    Error_t setAdditionalParam(float fParamValue) override
    {
        if (fParamValue <= 0)
            return Error_t::kNoError;

        m_iEta = CUtil::float2int<int>(fParamValue);

        return Error_t::kNoError;
    }

private:
    CFeatureTimeAcfCoeff() {};
    CFeatureTimeAcfCoeff(const CFeatureTimeAcfCoeff& that);

    int m_iEta = 19;
};

class CFeatureTimeMaxAcf : public CFeatureFromBlockIf
{
public:
    CFeatureTimeMaxAcf(Feature_t eFeatureIdx, int iDataLength, float fSampleRate) : CFeatureFromBlockIf(eFeatureIdx, iDataLength, fSampleRate)
    {
        m_pCCcf = new CCcf();
        m_pCCcf->init(iDataLength);

        m_pfAcf = new float[iDataLength - 1];
    };

    virtual ~CFeatureTimeMaxAcf() 
    {
        delete[] m_pfAcf;
        m_pfAcf = 0;

        delete m_pCCcf;
        m_pCCcf = 0;
    };

    Error_t calcFeatureFromBlock(float* pfFeature, const float* pfInput) override
    {
        float fMinThresh = 0.35F;

        int iEta = 0,
            iEtaMin = static_cast<int>(m_fSampleRate / m_fMax);

        m_pCCcf->calcCcf(pfInput, pfInput, true);
        m_pCCcf->getCcf(m_pfAcf, true);

        // avoid main lobe
        while (m_pfAcf[iEta] > fMinThresh)
            iEta++;

        if (iEtaMin < iEta)
            iEtaMin = iEta;

        // only look after first minimum
        iEta = 0;
        while (m_pfAcf[iEta] > m_pfAcf[iEta + 1])
        {
            iEta++;
            if (iEta >= m_iDataLength)
                break;
        }

        if (iEtaMin < iEta)
            iEtaMin = iEta;

        *pfFeature = CVectorFloat::getMax(&m_pfAcf[iEtaMin], m_iDataLength - iEtaMin);

        return Error_t::kNoError;
    };

    bool hasAdditionalParam() const override
    {
        return true;
    }

    Error_t setAdditionalParam(float fParamValue) override
    {
        if (fParamValue <= 0 || fParamValue > m_fSampleRate/2)
            return Error_t::kNoError;

        m_fMax = fParamValue;

        return Error_t::kNoError;
    }

private:
    CFeatureTimeMaxAcf() {};
    CFeatureTimeMaxAcf(const CFeatureTimeMaxAcf& that);

    CCcf *m_pCCcf = 0;
    float* m_pfAcf = 0;

    float m_fMax = 2000.F;
};

class CFeatureTimeRms : public CFeatureFromBlockIf
{
public:
    CFeatureTimeRms(Feature_t eFeatureIdx, int iDataLength, float fSampleRate) : CFeatureFromBlockIf(eFeatureIdx, iDataLength, fSampleRate) {};

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


/*! returns size of output feature (1 in most cases)
\return int
*/

Error_t CFeatureFromBlockIf::create(CFeatureFromBlockIf*& pCInstance, Feature_t eFeatureIdx, int iDataLength, float fSampleRate)
{
    if (iDataLength <= 0 || fSampleRate <= 0)
        return Error_t::kFunctionInvalidArgsError;


    switch (eFeatureIdx)
    {
    case kFeatureSpectralCentroid:
    case kFeatureSpectralCrestFactor:
    case kFeatureSpectralDecrease:
    case kFeatureSpectralFlatness:
    case kFeatureSpectralKurtosis:
    case kFeatureSpectralSkewness:
    case kFeatureSpectralSlope:
    case kFeatureSpectralSpread:
    case kFeatureTimeStd:
    case kFeatureTimeZeroCrossingRate:
        pCInstance = new CFeatureFromBlockIf(eFeatureIdx, iDataLength, fSampleRate);
        break;

    case kFeatureSpectralFlux:
        pCInstance = new CFeatureSpectralFlux(eFeatureIdx, iDataLength, fSampleRate);
        break;

    //case kFeatureSpectralMfccs:
    //    pCInstance = new CFeatureSpectralMfccs(eFeatureIdx, iDataLength, fSampleRate);
    //    break;

    //case kFeatureSpectralPitchChroma:
    //    pCInstance = new CFeatureSpectralPitchChroma(eFeatureIdx, iDataLength, fSampleRate);
    //    break;

    case kFeatureSpectralRolloff:
        pCInstance = new CFeatureSpectralRolloff(eFeatureIdx, iDataLength, fSampleRate);
        break;

    case kFeatureSpectralTonalPowerRatio:
        pCInstance = new CFeatureSpectralTonalPowerRatio(eFeatureIdx, iDataLength, fSampleRate);
        break;

    case kFeatureTimeAcfCoeff:
        pCInstance = new CFeatureTimeAcfCoeff(eFeatureIdx, iDataLength, fSampleRate);
        break;

    case kFeatureTimeMaxAcf:
        pCInstance = new CFeatureTimeMaxAcf(eFeatureIdx, iDataLength, fSampleRate);
        break;

    case kFeatureTimePeakEnvelope:
        pCInstance = new CFeatureTimePeakEnvelope(eFeatureIdx, iDataLength, fSampleRate);
        break;

    case kFeatureTimeRms:
        pCInstance = new CFeatureTimeRms(eFeatureIdx, iDataLength, fSampleRate);
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

    //m_bIsInitialized = false;

    return Error_t::kNoError;
}

int CFeatureFromBlockIf::getFeatureDimensions() const
{
    return 1;
}

bool CFeatureFromBlockIf::hasAdditionalParam() const
{
    return false;
}

Error_t CFeatureFromBlockIf::setAdditionalParam(float /*fParamvalue*/)
{
    return Error_t::kFunctionIllegalCallError;
}
