#if !defined(__ACA_SubFingerprint_HEADER_INCLUDED__)
#define __ACA_SubFingerprint_HEADER_INCLUDED__

#include "ErrorDef.h"

/*! \brief class for computation of a subfingerprint from a magnitude spectrum, 256 subfingerprints comprise one fingerprint
*/
class CSubFingerprint
{
public:
    explicit CSubFingerprint(int iMagSpecLength = 1025, float fSampleRate = 5000.F);

    virtual ~CSubFingerprint();

    /*! performs the SubFingerprint computation
    \param pfMagSpec input data of length iDataLength
    \return Error_t
    */
    uint32_t compSubFingerprint(const float *pfMagSpec);;

    /*! performs the SubFingerprint computation
    \param pfSubFingerprint feature result (user-allocated, to be written, length from CSubFingerprint::getFeatureDimensions)
    \param pfMagSpec input data of length iDataLength
    \return Error_t
    */
    Error_t reset();

protected:
    CSubFingerprint(const CSubFingerprint &that);
    CSubFingerprint &operator=(const CSubFingerprint &c);

    enum Block_t
    {
        kPrev,
        kCurr,
        kTmp,

        kNumProcBuffs
    };

    void genBands_();

    int m_iMagSpecLength = 1025; //!< block length

    const int m_iNumBands = 32; //!< number of bands

    float m_fSampleRate = 5000; //!< sample rate

    float **m_ppfH = 0; //!< transformation matrix bin to band

    float *m_apfProcBuff[kNumProcBuffs] = { 0 }; //!< temporary processing buffers
};

#endif // #if !defined(__ACA_SubFingerprint_HEADER_INCLUDED__)
