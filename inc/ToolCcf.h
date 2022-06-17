#if !defined(__ACA_Ccf_HEADER_INCLUDED__)
#define __ACA_Ccf_HEADER_INCLUDED__

#pragma once

#include "ErrorDef.h"

// forward declarations
class CFft;

/*! \brief computation of correlation (freq domain implementation)
*/
class CCcf
{
public:

    CCcf(void);
    virtual ~CCcf(void);

    /*! initializes the class with the size of the distance matrix
    \param iBlockLength
    \return Error_t
    */
    Error_t init(int iBlockLength);

    /*! resets all internal class members
    \return Error_t
    */
    Error_t reset();

    /*! computes cross correlation function
    \param pfIn1 (block of data to be correlated)
    \param pfIn2 (block of data to be correlated, equals pfIn1 when ACF)
    \param bNormalize flag whether the output is normalized
    \return Error_t
    */
    Error_t compCcf(const float *pfIn1, const float *pfIn2, bool bNormalize = true);

    /*! returns the length of the CCF result
    \return int
    */
    int getCcfLength(bool bisAcf = false);

    /*! returns the correlation result
    \param pfCcfResult result buffer
    \param bisAcf returns only non-redundant ACF result if true
    \return Error_t
    */
    Error_t getCcf(float *pfCcfResult, bool bisAcf = false) const;

    /*! returns the overall max
    \param bisAcf search only non-redundant ACF result if true
    \return float
    */
    float getCcfMax(bool bisAcf = false) const;

    /*! returns the index of the overall max
    \param bisAcf search only non-redundant ACF result if true
    \return float
    */
    int getCcfMaxIdx(bool bisAcf = false) const;

private:
    CCcf(const CCcf &that); //!< disallow copy construction   
    CCcf &operator=(const CCcf &c);

    bool m_bIsInitialized = false; //!< true if init has been called
    bool m_bWasProcessed = false; //!< true if process has been called

    float *m_apfData[2] = { 0,0 }; //!< CCF result
    CFft *m_pCFft = 0; //!< FFT instance

    int   m_iBlockLength = 0; //!< length of input
    int   m_iFftLength = 0; //!< length of FFT
};

#endif // __ACA_Ccf_HEADER_INCLUDED__
