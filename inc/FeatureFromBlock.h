#if !defined(__FeatureFromBlock_hdr__)
#define __FeatureFromBlock_hdr__

#include "Util.h"
#include "ErrorDef.h"


/*! \brief class for computation of a feature from a block of data (e.g., time or magnitude spectrum)
*/
class CFeatureFromBlockIf
{
public:
    enum Feature_t
    {
        kFeatureSpectralCentroid,
        kFeatureSpectralCrestFactor,
        kFeatureSpectralDecrease,
        kFeatureSpectralFlatness,
        kFeatureSpectralFlux,
        kFeatureSpectralKurtosis,
        kFeatureSpectralMfccs,
        kFeatureSpectralPitchChroma,
        kFeatureSpectralRolloff,
        kFeatureSpectralSkewness,
        kFeatureSpectralSlope,
        kFeatureSpectralSpread,
        kFeatureSpectralTonalPowerRatio,
        kFeatureTimeAcfCoeff,
        kFeatureTimeMaxAcf,
        kFeatureTimePeakEnvelope,
        kFeatureTimeRms,
        kFeatureTimeStd,
        kFeatureTimeZeroCrossingRate,

        kNumFeatures
    };

    /*! initializes a FeatureFromBlock instance with file reading
    \param pCInstance pointer to instance to be written
    \param eFeatureIdx index of Feature to extract
    \param iDataLength: block length
    \param fSampleRate: sample rate (only used when needed)
    \return Error_t
    */
    static Error_t create(CFeatureFromBlockIf*& pCInstance, Feature_t eFeatureIdx, int iDataLength, float fSampleRate = 1.F);

    /*! destroys a FeatureFromBlock instance
    \param pCInstance pointer to instance to be destroyed
    \return Error_t
    */
    static Error_t destroy(CFeatureFromBlockIf*& pCInstance);

    /*! returns size of output feature (1 in most cases)
    \return int
    */
    virtual int getFeatureDimensions() const;

    /*! performs the FeatureFromBlock computation
    \param pfFeature feature result (user-allocated, to be written, length from CFeatureFromBlockIf::getFeatureDimensions)
    \param pfInput input data of length iDataLength
    \return Error_t
    */
    virtual Error_t calcFeatureFromBlock(float* pfFeature, const float *pfInput) = 0;


    ////////////////////////////////////////////////////////////////////////////
    // static functions for some features where it makes sense (use at your own risk)
    static inline float compFeatureSpectralCentroid(const float* pfMagSpec, int iDataLength, float fSampleRate = 1.F);
    static inline float compFeatureSpectralCrestFactor(const float* pfMagSpec, int iDataLength, float fSampleRate = 1.F);
    static inline float compFeatureSpectralDecrease(const float* pfMagSpec, int iDataLength, float fSampleRate = 1.F);
    static inline float compFeatureSpectralFlatness(const float* pfMagSpec, int iDataLength, float fSampleRate = 1.F);
    static inline float compFeatureSpectralFlux(const float* pfMagSpec, const float* pfPrevSpec, int iDataLength, float fSampleRate = 1.F);
    static inline float compFeatureSpectralKurtosis(const float* pfMagSpec, int iDataLength, float fSampleRate = 1.F);
    static inline float compFeatureSpectralRolloff(const float* pfMagSpec, int iDataLength, float fSampleRate = 1.F, float fKappa = .85F);
    static inline float compFeatureSpectralSkewness(const float* pfMagSpec, int iDataLength, float fSampleRate = 1.F);
    static inline float compFeatureSpectralSlope(const float* pfMagSpec, int iDataLength, float fSampleRate = 1.F);
    static inline float compFeatureSpectralSpread(const float* pfMagSpec, int iDataLength, float fSampleRate = 1.F);
    static inline float compFeatureSpectralTonalPowerRatio(const float* pfMagSpec, int iDataLength, float fSampleRate = 1.F, float fThresh = 5e-4);
    static inline float compFeatureTimePeakEnvelope(const float* pfSamples, int iDataLength, float fSampleRate = 1.F);
    static inline float compFeatureTimeRms(const float* pfSamples, int iDataLength, float fSampleRate = 1.F);
    static inline float compFeatureTimeStd(const float* pfSamples, int iDataLength, float fSampleRate = 1.F);
    static inline float compFeatureTimeZeroCrossingRate(const float* pfSamples, int iDataLength, float fSampleRate = 1.F);

    static const float m_kfFloatThresh;      //!< below this we just assume it's zero


protected:
    CFeatureFromBlockIf() {};
    virtual ~CFeatureFromBlockIf() {};
    CFeatureFromBlockIf(const CFeatureFromBlockIf& that);


    int m_iDataLength = 0;               //!< block length

    float m_fSampleRate = 0;             //!< sample rate
};


inline float CFeatureFromBlockIf::compFeatureSpectralCentroid(const float* pfMagSpec, int iDataLength, float fSampleRate /*= 1.F*/)
{
    assert(pfMagSpec);
    assert(iDataLength > 0);
    assert(fSampleRate > 0);

    float fNorm = 0;
    float fvsc = 0;

    for (auto k = 0; k < iDataLength; k++)
    {
        fNorm += pfMagSpec[k];
        fvsc += k * pfMagSpec[k];
    }

    if (fNorm < m_kfFloatThresh)
        return 0;

    // convert from index to Hz
    return fvsc * fSampleRate / (2.F * fNorm * (iDataLength - 1));
}

inline float CFeatureFromBlockIf::compFeatureSpectralCrestFactor(const float* pfMagSpec, int iDataLength, float /*fSampleRate = 1.F*/)
{
    assert(pfMagSpec);
    assert(iDataLength > 0);

    float fNorm = CVectorFloat::getSum(pfMagSpec, iDataLength);

    if (fNorm < m_kfFloatThresh)
        return 0;

    return CVectorFloat::getMax(pfMagSpec, iDataLength) / fNorm;
}

inline float CFeatureFromBlockIf::compFeatureSpectralDecrease(const float* pfMagSpec, int iDataLength, float /*fSampleRate = 1.F*/)
{
    assert(pfMagSpec);
    assert(iDataLength > 0);

    float fNorm = 0;
    float fvsd = 0;

    for (auto k = 1; k < iDataLength; k++)
    {
        fNorm += pfMagSpec[k];
        fvsd += (pfMagSpec[k] - pfMagSpec[0]) / k;
    }

    if (fNorm < m_kfFloatThresh)
        return 0;

    // convert from index to Hz
    return fvsd / fNorm;
}

inline float CFeatureFromBlockIf::compFeatureSpectralFlatness(const float* pfMagSpec, int iDataLength, float /*fSampleRate = 1.F*/)
{
    assert(pfMagSpec);
    assert(iDataLength > 0);

    float fNorm = CVectorFloat::getMean(pfMagSpec, iDataLength);
    float fGeoMean = 0;

    if (fNorm < m_kfFloatThresh || CVectorFloat::getMin(pfMagSpec, iDataLength) < m_kfFloatThresh)
        return 0;

    for (auto k = 0; k < iDataLength; k++)
    {
        fGeoMean += std::log(pfMagSpec[k]);
    }

    return std::exp(fGeoMean / iDataLength) / fNorm;
}

inline float CFeatureFromBlockIf::compFeatureSpectralFlux(const float* pfMagSpec, const float* pfPrevSpec, int iDataLength, float /*fSampleRate = 1.F*/)
{
    assert(pfMagSpec);
    assert(pfPrevSpec);
    assert(iDataLength > 0);

    float fSum = 0;
    for (auto k = 0; k < iDataLength; k++)
    {
        float fDiff = pfMagSpec[k] - pfPrevSpec[k];
        fSum += fDiff * fDiff;
    }

    return std::sqrt(fSum) / iDataLength;
}

inline float CFeatureFromBlockIf::compFeatureSpectralSpread(const float* pfMagSpec, int iDataLength, float fSampleRate /*= 1.F*/)
{
    assert(pfMagSpec);
    assert(iDataLength > 0);
    assert(fSampleRate > 0);

    float fNorm = 0;
    float fvss = 0;
    float fvsc = compFeatureSpectralCentroid(pfMagSpec, iDataLength, fSampleRate) * 2 / fSampleRate * (iDataLength - 1);

    if (fvsc < m_kfFloatThresh)
        return 0;

    for (auto k = 0; k < iDataLength; k++)
    {
        float fAgg = (k - fvsc) * (k - fvsc);
        fNorm += pfMagSpec[k];
        fvss += fAgg * pfMagSpec[k];
    }

    // convert from index to Hz
    return std::sqrt(fvss / fNorm) * fSampleRate / (2.F * (iDataLength - 1));
}

inline float CFeatureFromBlockIf::compFeatureSpectralKurtosis(const float* pfMagSpec, int iDataLength, float fSampleRate /*= 1.F*/)
{
    assert(pfMagSpec);
    assert(iDataLength > 0);
    assert(fSampleRate > 0);

    float fNorm = 0;
    float fvsk = 0;
    float fvsc = compFeatureSpectralCentroid(pfMagSpec, iDataLength, fSampleRate) * 2 / fSampleRate * (iDataLength - 1);
    float fvss = compFeatureSpectralSpread(pfMagSpec, iDataLength, fSampleRate) * 2 / fSampleRate * (iDataLength - 1);

    if (fvsc < m_kfFloatThresh || fvss < m_kfFloatThresh)
        return 0;

    for (auto k = 0; k < iDataLength; k++)
    {
        float fAgg = (k - fvsc) * (k - fvsc);
        fAgg *= fAgg;
        fNorm += pfMagSpec[k];
        fvsk += fAgg * pfMagSpec[k];
    }

    if (fNorm < m_kfFloatThresh)
        return 0;

    return fvsk = fvsk / (fvss * fvss * fvss * fvss * fNorm) - 3;
}

inline float CFeatureFromBlockIf::compFeatureSpectralRolloff(const float* pfMagSpec, int iDataLength, float fSampleRate /*= 1.F*/, float fKappa /*= .85F*/)
{
    assert(pfMagSpec);
    assert(iDataLength > 0);
    assert(fSampleRate > 0);
    assert(fKappa > 0 && fKappa <= 1);

    float fNorm = CVectorFloat::getSum(pfMagSpec, iDataLength);
    float fSum = 0;
    int k = 0;

    if (fNorm < m_kfFloatThresh)
        return 0;

    fNorm *= fKappa;
    while (fSum <= fNorm)
    {
        fSum += pfMagSpec[k];
        k++;

        assert(k < iDataLength);
    }

    // convert from index to Hz
    return (k-1) * fSampleRate / (2.F * (iDataLength - 1));
}

inline float CFeatureFromBlockIf::compFeatureSpectralSkewness(const float* pfMagSpec, int iDataLength, float fSampleRate /*= 1.F*/)
{
    assert(pfMagSpec);
    assert(iDataLength > 0);
    assert(fSampleRate > 0);

    float fNorm = 0;
    float fvssk = 0;
    float fvsc = compFeatureSpectralCentroid(pfMagSpec, iDataLength, fSampleRate) * 2 / fSampleRate * (iDataLength - 1);
    float fvss = compFeatureSpectralSpread(pfMagSpec, iDataLength, fSampleRate) * 2 / fSampleRate * (iDataLength - 1);

    if (fvsc < m_kfFloatThresh || fvss < m_kfFloatThresh)
        return 0;

    for (auto k = 0; k < iDataLength; k++)
    {
        float fAgg = (k - fvsc) * (k - fvsc) * (k - fvsc);
        fNorm += pfMagSpec[k];
        fvssk += fAgg * pfMagSpec[k];
    }

    if (fNorm < m_kfFloatThresh)
        return 0;

    return fvssk = fvssk / (fvss * fvss * fvss * fNorm) ;
}

inline float CFeatureFromBlockIf::compFeatureSpectralSlope(const float* pfMagSpec, int iDataLength, float fSampleRate /*= 1.F*/)
{
    assert(pfMagSpec);
    assert(iDataLength > 0);
    assert(fSampleRate > 0);

    // compute mean
    float fvsc = compFeatureSpectralCentroid(pfMagSpec, iDataLength, fSampleRate) * 2 / fSampleRate * (iDataLength - 1);
    float fvssl = 0;
    float fNorm = 0;

    if (fvsc < m_kfFloatThresh)
        return 0;

    for (auto k = 0; k < iDataLength; k++)
    {
        float fk = k - (iDataLength + 1) / 2.F;
        fvssl += fk * (pfMagSpec[k] - fvsc);
        fNorm += fk * fk;
    }

    if (fNorm < m_kfFloatThresh)
        return 0;

    return fvssl / fNorm;
}
inline float CFeatureFromBlockIf::compFeatureSpectralTonalPowerRatio(const float* pfMagSpec, int iDataLength, float /*fSampleRate = 1.F*/, float fThresh /*= 5e-4F*/)
{
    assert(pfMagSpec);
    assert(iDataLength > 0);

    // compute mean
    float fvtpr = 0;
    float fNorm = pfMagSpec[0] * pfMagSpec[0] + pfMagSpec[iDataLength-1] * pfMagSpec[iDataLength-1];

    for (auto k = 1; k < iDataLength - 1; k++)
    {
        float fSquare = pfMagSpec[k] * pfMagSpec[k];
        fNorm += fSquare;

        // search for local maxima
        if (pfMagSpec[k] <= pfMagSpec[k - 1] || pfMagSpec[k] <= pfMagSpec[k + 1] || pfMagSpec[k] <= fThresh)
            continue;
        else
        {
            fvtpr += fSquare;
            fNorm += pfMagSpec[k+1] * pfMagSpec[k+1];
            k++;
        }
    }

    if (fNorm < m_kfFloatThresh)
        return 0;

    return fvtpr / fNorm;
}

inline float CFeatureFromBlockIf::compFeatureTimePeakEnvelope(const float* pfSamples, int iDataLength, float /*fSampleRate = 1.F*/)
{
    assert(pfSamples);
    assert(iDataLength > 0);

    return CVectorFloat::getMax(pfSamples, iDataLength);
}

inline float CFeatureFromBlockIf::compFeatureTimeStd(const float* pfSamples, int iDataLength, float /*fSampleRate = 1.F*/)
{
    assert(pfSamples);
    assert(iDataLength > 0);

    return CVectorFloat::getStd(pfSamples, iDataLength);
}

inline float CFeatureFromBlockIf::compFeatureTimeRms(const float* pfSamples, int iDataLength, float /*fSampleRate = 1.F*/)
{
    assert(pfSamples);
    assert(iDataLength > 0);

    return CVectorFloat::getStd(pfSamples, iDataLength, 0.F);
}

inline float CFeatureFromBlockIf::compFeatureTimeZeroCrossingRate(const float* pfSamples, int iDataLength, float /*fSampleRate = 1.F*/)
{
    assert(pfSamples);
    assert(iDataLength > 0);

    float fvzc = 0;

    float fPrevSign = 0;
    for (auto i = 0; i < iDataLength; i++)
    {
        float fCurrSign = CUtil::sign(pfSamples[i]);
        fvzc += std::abs(fCurrSign - fPrevSign);
        fPrevSign = fCurrSign;
    }

    return fvzc / (2 * iDataLength);
}


#endif // #if !defined(__FeatureFromBlock_hdr__)



