#if !defined(__Ccf_HEADER_INCLUDED__)
#define __Ccf_HEADER_INCLUDED__

#pragma once

#include "ErrorDef.h"

// forward declarations
class CFft;

/*! \brief computation of correlation (freq domain)
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
    Error_t init (int iBlockLength);
    
    /*! resets all internal class members
    \return Error_t
    */
    Error_t reset ();

    /*! computes cross correlation function
    \param pfInput1 (block of data to be correlated)
    \param pfInput2 (block of data to be correlated, equals pfInput1 when ACF)
    \param bNormalize flag whether the output is normalized
    \return Error_t
    */
    Error_t compCcf (const float* pfInput1, const float* pfInput2, bool bNormalize = true);
 
    /*! returns the length of the CCF result
    \return int
    */
    int getCcfLength (bool bIsAcf = false);
    
    /*! returns the correlation result
    \param pfCcfResult result buffer
    \param bIsAcf returns only non-redundant ACF result if true
    \return Error_t
    */
    Error_t getCcf (float *pfCcfResult, bool bIsAcf = false) const;

    /*! returns the overall max
    \param bIsAcf search only non-redundant ACF result if true
    \return float
    */
    float getCcfMax(bool bIsAcf = false) const;

    /*! returns the index of the overall max
    \param bIsAcf search only non-redundant ACF result if true
    \return float
    */
    int getCcfMaxIdx(bool bIsAcf = false) const;

private:
    CCcf(const CCcf& that);     //!< disallow copy construction   

    bool m_bIsInitialized = false;  //!< true if init has been called
    bool m_bWasProcessed = false;   //!< true if process has been called

    float* m_apfData[2] = { 0,0 };  //!< CCF result
    CFft *m_pCFft= 0;               //!< FFT instance

    int   m_iBlockLength = 0;       //!< length of input
    int   m_iFftLength = 0;         //!< length of FFT
};


#endif
