#if !defined(__ACA_FeatureFromBlock_HEADER_INCLUDED__)
#define __ACA_FeatureFromBlock_HEADER_INCLUDED__

#include <map>
#include <functional>

#include "Feature.h"

#include "ErrorDef.h"


/*! \brief class for computation of a feature from a block of data (e.g., time or magnitude spectrum)
*/
class CFeatureFromBlockIf
{
public:

    /*! initializes a FeatureFromBlock instance
    \param pCInstance pointer to instance to be written
    \param eFeatureIdx index of Feature to extract
    \param iDataLength: block length
    \param fSampleRate: sample rate (only used when needed)
    \return Error_t
    */
    static Error_t create(CFeatureFromBlockIf *&pCInstance, CFeatureIf::Feature_t eFeatureIdx, int iDataLength, float fSampleRate = 1.F);

    /*! destroys a FeatureFromBlock instance
    \param pCInstance pointer to instance to be destroyed
    \return Error_t
    */
    static Error_t destroy(CFeatureFromBlockIf *&pCInstance);

    /*! returns size of output feature (1 in most cases)
    \return int
    */
    virtual int getFeatureDimensions() const;

    /*! returns index of the feature to extract
    \return CFeatureIf::Feature_t
    */
    CFeatureIf::Feature_t getFeatureIdx() const
    {
        return m_eFeatureIdx;
    }

    /*! performs the FeatureFromBlock computation
    \param pfFeature feature result (user-allocated, to be written, length from CFeatureFromBlockIf::getFeatureDimensions)
    \param pfIn input data of length iDataLength
    \return Error_t
    */
    virtual Error_t compFeature(float *pfFeature, const float *pfIn);

    /*! returns true if there is an additional parameter
    \return bool
    */
    virtual bool hasAdditionalParam() const;

    /*! returns true if there is an additional parameter
    * \param fParamValue new parameter value
    \return bool
    */
    virtual Error_t setAdditionalParam(float fParamValue);


    ////////////////////////////////////////////////////////////////////////////
    // static functions for some features where it makes sense (use at your own risk)
    /*! returns feature from block
    * \param pfMagSpec magnitude spectrum
    * \param iDataLength length of magnitude spectrum
    * \param fSampleRate audio sample rate (only needed for scaling)
    \return float feature
    */
    static float compFeatureSpectralCentroid(const float *pfMagSpec, int iDataLength, float fSampleRate = 1.F);
    /*! returns feature from block
    * \param pfMagSpec magnitude spectrum
    * \param iDataLength length of magnitude spectrum
    * \param fSampleRate audio sample rate (only needed for scaling)
    \return float feature
    */
    static float compFeatureSpectralCrestFactor(const float *pfMagSpec, int iDataLength, float fSampleRate = 1.F);
    /*! returns feature from block
    * \param pfMagSpec magnitude spectrum
    * \param iDataLength length of magnitude spectrum
    * \param fSampleRate audio sample rate (only needed for scaling)
    \return float feature
    */
    static float compFeatureSpectralDecrease(const float *pfMagSpec, int iDataLength, float fSampleRate = 1.F);
    /*! returns feature from block
    * \param pfMagSpec magnitude spectrum
    * \param iDataLength length of magnitude spectrum
    * \param fSampleRate audio sample rate (only needed for scaling)
    \return float feature
    */
    static float compFeatureSpectralFlatness(const float *pfMagSpec, int iDataLength, float fSampleRate = 1.F);
    /*! returns feature from block
    * \param pfMagSpec magnitude spectrum
    * \param pfPrevSpec preceding magnitude spectrum
    * \param iDataLength length of magnitude spectrum
    * \param fSampleRate audio sample rate (only needed for scaling)
    \return float feature
    */
    static float compFeatureSpectralFlux(const float *pfMagSpec, const float *pfPrevSpec, int iDataLength, float fSampleRate = 1.F);
    /*! returns feature from block
    * \param pfMagSpec magnitude spectrum
    * \param iDataLength length of magnitude spectrum
    * \param fSampleRate audio sample rate (only needed for scaling)
    \return float feature
    */
    static float compFeatureSpectralKurtosis(const float *pfMagSpec, int iDataLength, float fSampleRate = 1.F);
    /*! returns feature from block
    * \param pfMagSpec magnitude spectrum
    * \param iDataLength length of magnitude spectrum
    * \param fSampleRate audio sample rate (only needed for scaling)
    * \param fKappa bandwidth parameter
    \return float feature
    */
    static float compFeatureSpectralRolloff(const float *pfMagSpec, int iDataLength, float fSampleRate = 1.F, float fKappa = .85F);
    /*! returns feature from block
    * \param pfMagSpec magnitude spectrum
    * \param iDataLength length of magnitude spectrum
    * \param fSampleRate audio sample rate (only needed for scaling)
    \return float feature
    */
    static float compFeatureSpectralSkewness(const float *pfMagSpec, int iDataLength, float fSampleRate = 1.F);
    /*! returns feature from block
    * \param pfMagSpec magnitude spectrum
    * \param iDataLength length of magnitude spectrum
    * \param fSampleRate audio sample rate (only needed for scaling)
    \return float feature
    */
    static float compFeatureSpectralSlope(const float *pfMagSpec, int iDataLength, float fSampleRate = 1.F);
    /*! returns feature from block
    * \param pfMagSpec magnitude spectrum
    * \param iDataLength length of magnitude spectrum
    * \param fSampleRate audio sample rate (only needed for scaling)
    \return float feature
    */
    static float compFeatureSpectralSpread(const float *pfMagSpec, int iDataLength, float fSampleRate = 1.F);
    /*! returns feature from block
    * \param pfMagSpec magnitude spectrum
    * \param iDataLength length of magnitude spectrum
    * \param fSampleRate audio sample rate (only needed for scaling)
    * \param fThresh for maximum computation
    \return float feature
    */
    static float compFeatureSpectralTonalPowerRatio(const float *pfMagSpec, int iDataLength, float fSampleRate = 1.F, float fThresh = 5e-4);
    /*! returns feature from block
    * \param pfSamples block of time domain samples
    * \param iDataLength length of magnitude spectrum
    * \param fSampleRate audio sample rate (only needed for scaling)
    * \param iEta index of coefficient to extract
    \return float feature
    */
    static float compFeatureTimeAcfCoeff(const float *pfSamples, int iDataLength, float fSampleRate = 1.F, int  iEta = 19);
    /*! returns feature from block
    * \param pfSamples block of time domain samples
    * \param iDataLength length of magnitude spectrum
    * \param fSampleRate audio sample rate (only needed for scaling)
    \return float feature
    */
    static float compFeatureTimePeakEnvelope(const float *pfSamples, int iDataLength, float fSampleRate = 1.F);
    /*! returns feature from block
    * \param pfSamples block of time domain samples
    * \param iDataLength length of magnitude spectrum
    * \param fSampleRate audio sample rate (only needed for scaling)
    \return float feature
    */
    static float compFeatureTimeRms(const float *pfSamples, int iDataLength, float fSampleRate = 1.F);
    /*! returns feature from block
    * \param pfSamples block of time domain samples
    * \param iDataLength length of magnitude spectrum
    * \param fSampleRate audio sample rate (only needed for scaling)
    \return float feature
    */
    static float compFeatureTimeStd(const float *pfSamples, int iDataLength, float fSampleRate = 1.F);
    /*! returns feature from block
    * \param pfSamples block of time domain samples
    * \param iDataLength length of magnitude spectrum
    * \param fSampleRate audio sample rate (only needed for scaling)
    \return float feature
    */
    static float compFeatureTimeZeroCrossingRate(const float *pfSamples, int iDataLength, float fSampleRate = 1.F);

    static const float m_kfFloatThresh;  //!< below this we just assume everything's zero


protected:
    CFeatureFromBlockIf() {};
    CFeatureFromBlockIf(CFeatureIf::Feature_t eFeatureIdx, int iDataLength, float fSampleRate);
    virtual ~CFeatureFromBlockIf() {};
    CFeatureFromBlockIf(const CFeatureFromBlockIf &that);
    CFeatureFromBlockIf &operator=(const CFeatureFromBlockIf &c);

    CFeatureIf::Feature_t m_eFeatureIdx = CFeatureIf::kNumFeatures; //!< index of feature to extract

    int m_iDataLength = 0; //!< block length
    float m_fSampleRate = 0; //!< sample rate

    //!< dispatcher map for static functions without additional arguments
    const std::map<CFeatureIf::Feature_t, std::function<float(const float *, int, float)>> m_DispatchMap
    {
            {CFeatureIf::kFeatureSpectralCentroid, &compFeatureSpectralCentroid},
            {CFeatureIf::kFeatureSpectralCrestFactor, &compFeatureSpectralCrestFactor},
            {CFeatureIf::kFeatureSpectralDecrease, &compFeatureSpectralDecrease},
            {CFeatureIf::kFeatureSpectralFlatness, &compFeatureSpectralFlatness},
            {CFeatureIf::kFeatureSpectralKurtosis, &compFeatureSpectralKurtosis},
            {CFeatureIf::kFeatureSpectralSkewness, &compFeatureSpectralSkewness},
            {CFeatureIf::kFeatureSpectralSlope, &compFeatureSpectralSlope},
            {CFeatureIf::kFeatureSpectralSpread, &compFeatureSpectralSpread},
            {CFeatureIf::kFeatureTimeStd, &compFeatureTimeStd},
            {CFeatureIf::kFeatureTimeZeroCrossingRate, &compFeatureTimeZeroCrossingRate}
    };
};

#endif // #if !defined(__ACA_FeatureFromBlock_HEADER_INCLUDED__)