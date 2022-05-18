#if !defined(__FeatureFromBlock_hdr__)
#define __FeatureFromBlock_hdr__

#include <map>
#include <functional>

#include "Util.h"
#include "ErrorDef.h"


/*! \brief class for computation of a feature from a block of data (e.g., time or magnitude spectrum)
*/
class CFeatureFromBlockIf
{
public:
    /*! \brief enum to index all features
    */
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
    virtual Error_t calcFeatureFromBlock(float* pfFeature, const float* pfInput);

    /*! returns true if there is an additional parameter
    \return bool
    */
    virtual bool hasAdditionalParam() const;

    /*! returns true if there is an additional parameter
    * \param fParamValue new parameter value
    \return bool
    */
    virtual Error_t setAdditionalParam(float fParamvalue);


    ////////////////////////////////////////////////////////////////////////////
    // static functions for some features where it makes sense (use at your own risk)
    static float compFeatureSpectralCentroid(const float* pfMagSpec, int iDataLength, float fSampleRate = 1.F);
    static float compFeatureSpectralCrestFactor(const float* pfMagSpec, int iDataLength, float fSampleRate = 1.F);
    static float compFeatureSpectralDecrease(const float* pfMagSpec, int iDataLength, float fSampleRate = 1.F);
    static float compFeatureSpectralFlatness(const float* pfMagSpec, int iDataLength, float fSampleRate = 1.F);
    static float compFeatureSpectralFlux(const float* pfMagSpec, const float* pfPrevSpec, int iDataLength, float fSampleRate = 1.F);
    static float compFeatureSpectralKurtosis(const float* pfMagSpec, int iDataLength, float fSampleRate = 1.F);
    static float compFeatureSpectralRolloff(const float* pfMagSpec, int iDataLength, float fSampleRate = 1.F, float fKappa = .85F);
    static float compFeatureSpectralSkewness(const float* pfMagSpec, int iDataLength, float fSampleRate = 1.F);
    static float compFeatureSpectralSlope(const float* pfMagSpec, int iDataLength, float fSampleRate = 1.F);
    static float compFeatureSpectralSpread(const float* pfMagSpec, int iDataLength, float fSampleRate = 1.F);
    static float compFeatureSpectralTonalPowerRatio(const float* pfMagSpec, int iDataLength, float fSampleRate = 1.F, float fThresh = 5e-4);
    static float compFeatureTimeAcfCoeff(const float* pfSamples, int iDataLength, float fSampleRate = 1.F, int  iEta = 19);
    static float compFeatureTimePeakEnvelope(const float* pfSamples, int iDataLength, float fSampleRate = 1.F);
    static float compFeatureTimeRms(const float* pfSamples, int iDataLength, float fSampleRate = 1.F);
    static float compFeatureTimeStd(const float* pfSamples, int iDataLength, float fSampleRate = 1.F);
    static float compFeatureTimeZeroCrossingRate(const float* pfSamples, int iDataLength, float fSampleRate = 1.F);

    // dispatcher map for static functions without additional arguments
    const std::map<Feature_t, std::function<float(const float*, int, float)>> m_DispatchMap
    {
            {kFeatureSpectralCentroid, &compFeatureSpectralCentroid},
            {kFeatureSpectralCrestFactor, &compFeatureSpectralCrestFactor},
            {kFeatureSpectralDecrease, &compFeatureSpectralDecrease},
            {kFeatureSpectralFlatness, &compFeatureSpectralFlatness},
            {kFeatureSpectralKurtosis, &compFeatureSpectralKurtosis},
            {kFeatureSpectralSkewness, &compFeatureSpectralSkewness},
            {kFeatureSpectralSlope, &compFeatureSpectralSlope},
            {kFeatureSpectralSpread, &compFeatureSpectralSpread},
            {kFeatureTimeStd, &compFeatureTimeStd},
            {kFeatureTimeZeroCrossingRate, &compFeatureTimeZeroCrossingRate}
    };

    static const float m_kfFloatThresh;         //!< below this we just assume it's zero


protected:
    CFeatureFromBlockIf() {};
    CFeatureFromBlockIf(Feature_t eFeatureIdx, int iDataLength, float fSampleRate) : m_eFeatureIdx(eFeatureIdx), m_iDataLength(iDataLength), m_fSampleRate(fSampleRate) {};
    virtual ~CFeatureFromBlockIf() {};
    CFeatureFromBlockIf(const CFeatureFromBlockIf& that);

    int m_iDataLength = 0;                      //!< block length

    float m_fSampleRate = 0;                    //!< sample rate

    Feature_t m_eFeatureIdx = kNumFeatures;     //!< index of feature to extract
};



#endif // #if !defined(__FeatureFromBlock_hdr__)



