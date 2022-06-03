#if !defined(__PitchFromBlock_hdr__)
#define __PitchFromBlock_hdr__

#include <map>
#include <functional>
#include <cassert>

#include "ErrorDef.h"


/*! \brief class for computation of a Pitch from a block of data (e.g., time or magnitude spectrum)
*/
class CPitchFromBlockIf
{
public:
    enum PitchExtractors_t
    {
        kPitchSpectralAcf,
        kPitchSpectralHps,

        kPitchTimeAcf,
        kPitchTimeAmdf,
        kPitchTimeAuditory,
        kPitchTimeZeroCrossings,

        kNumPitchExtractors
    };

    /*! initializes a PitchFromBlock instance 
    \param pCInstance pointer to instance to be written
    \param ePitchIdx index of Pitch to extract
    \param iDataLength: block length
    \param fSampleRate: sample rate (only used when needed)
    \return Error_t
    */
    static Error_t create(CPitchFromBlockIf*& pCInstance, PitchExtractors_t ePitchIdx, int iDataLength, float fSampleRate = 1.F);

    /*! destroys a PitchFromBlock instance
    \param pCInstance pointer to instance to be destroyed
    \return Error_t
    */
    static Error_t destroy(CPitchFromBlockIf*& pCInstance);

    /*! returns size of output Pitch (1 in most cases)
    \return int
    */
    //virtual int getPitchDimensions() const;

    /*! returns index of the Pitch to extract
    \return Pitch_t
    */
    PitchExtractors_t getPitchExtractorIdx() const
    {
        return m_ePitchIdx;
    }

    /*! performs the PitchFromBlock computation
    \param pfInput input data of length iDataLength
    \return float fF0InHz
    */
    virtual float compF0(const float* pfInput) = 0;
        /*! returns true if there is an additional parameter
    \return bool
    */
    //virtual bool hasAdditionalParam() const;

    ///*! returns true if there is an additional parameter
    //* \param fParamValue new parameter value
    //\return bool
    //*/
    //virtual Error_t setAdditionalParam(float fParamValue);


    //////////////////////////////////////////////////////////////////////////////
    //// static functions for some Pitchs where it makes sense (use at your own risk)
    //static float compPitchSpectralAcf(const float* pfMagSpec, int iDataLength, float fSampleRate = 1.F);
    //static float compPitchSpectralHps(const float* pfMagSpec, int iDataLength, float fSampleRate = 1.F);
    //static float compPitchTimeAcf(const float* pfSamples, int iDataLength, float fSampleRate = 1.F);
    //static float compPitchTimeAmdf(const float* pfSamples, int iDataLength, float fSampleRate = 1.F);
    //static float compPitchTimeAuditory(const float* pfSamples, int iDataLength, float fSampleRate = 1.F);
    //static float compPitchTimeZeroCrossings(const float* pfSamples, int iDataLength, float fSampleRate = 1.F);

    //static const float m_kfFloatThresh;         //!< below this we just assume it's zero


protected:
    CPitchFromBlockIf() {};
    CPitchFromBlockIf(PitchExtractors_t ePitchIdx, int iDataLength, float fSampleRate) : m_ePitchIdx(ePitchIdx), m_iDataLength(iDataLength), m_fSampleRate(fSampleRate) {assert(iDataLength > 0);};
    virtual ~CPitchFromBlockIf() {};
    CPitchFromBlockIf(const CPitchFromBlockIf& that);
    CPitchFromBlockIf& operator=(const CPitchFromBlockIf& c);

    PitchExtractors_t m_ePitchIdx = kNumPitchExtractors;     //!< index of Pitch to extract

    int m_iDataLength = 0;                      //!< block length

    float m_fSampleRate = 0;                    //!< sample rate
 
    // dispatcher map for static functions without additional arguments
    //const std::map<PitchExtractors_t, std::function<float(const float*, int, float)>> m_DispatchMap
    //{
    //        {kPitchSpectralAcf, &compPitchSpectralAcf},
    //        {kPitchSpectralHps, &compPitchSpectralHps},
    //        {kPitchTimeAcf, &compPitchTimeAcf},
    //        {kPitchTimeAmdf, &compPitchTimeAmdf},
    //        {kPitchTimeAuditory, &compPitchTimeAuditory},
    //        {kPitchTimeZeroCrossings, &compPitchTimeZeroCrossings}
    //};
};



#endif // #if !defined(__PitchFromBlock_hdr__)



