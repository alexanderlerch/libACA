#if !defined(__Pca_HEADER_INCLUDED__)
#define __Pca_HEADER_INCLUDED__

#pragma once

#include "ErrorDef.h"

/*! \brief computation of principal component analysis
*/
class CPca
{
public:
    CPca(void) {};
    virtual ~CPca(void);;

    Error_t init(int iNumFeatures, int iNumObservations);

    Error_t reset();

    Error_t compPca(float** ppfRes, float* pfEigenValues, float** ppfIn);




    //each row is a variable
    static Error_t compCov(float** ppfCovOut, float** ppfIn, int iNumRows, int iNumCols);

    // a = uwv, u replaces a (*this)

    static Error_t calcSVD(float** ppfU, float** ppfW, float** ppfV, float** ppfMat, int iNumRows, int iNumCols, int iMaxIterations = 100);

private:
    CPca(const CPca& that);     //!< disallow copy construction   
    CPca& operator=(const CPca& c);

    static float matPythag(float dA, float dB);;

    int m_iNumFeatures = 0,
        m_iNumObs = 0;

    float** m_ppfProcTmp = 0;
    float** m_ppfU = 0;
    float** m_ppfW = 0;
    float** m_ppfV = 0;
};


#endif
