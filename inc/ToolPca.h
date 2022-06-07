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


    /*! initializes PCA instance
    \param iNumFeatures number of rows in the input and output matrices
    \param iNumObservations number of columns in the input and output matrices
    \return Error_t
    */
    Error_t init(int iNumFeatures, int iNumObservations);

    /*! resets PCA instance
    \return Error_t
    */
    Error_t reset();

    /*! computes the principal components
    \param ppfRes matrix with principal components (user-allocated, to be written, dimensions equal input matrix)
    \param pfEigenValues eigenvalues of principle components (user-allocated, to be written, length equals number of features)
    \param ppfIn input matrix (dimensions iNumFeatures X iNumObservations)
    \return Error_t
    */
    Error_t compPca(float** ppfRes, float* pfEigenValues, float** ppfIn);


    /*! computes covariance matrix
    \param ppfCovOut resulting covariance matrix (user-allocated, to be written, dimension iNumRows X iNumRows)
    \param ppfIn input matrix (dimensions iNumRows X iNumCols), each row is a variable
    \param iNumRows number of rows/features in the input  matrix
    \param iNumCols number of columns/observations in the input matrix
    \return Error_t
    */
    static Error_t compCov(float** ppfCovOut, float** ppfIn, int iNumRows, int iNumCols);


    /*! computes singular value decomposition (ppfMat = ppfU * ppfW * ppfV)
    \param ppfU (user-allocated, to be written, dimension iNumRows X iNumCols)
    \param ppfW  (user-allocated, to be written, dimension iNumCols X iNumCols)
    \param ppfV (user-allocated, to be written, dimension iNumCols X iNumCols)
    \param ppfU (dimension iNumRows X iNumCols)
    \param iNumRows number of rows in the input  matrix
    \param iNumCols number of columns/in the input matrix
    \param iMaxIterations maximum number of iterations
    \return Error_t
    */
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
