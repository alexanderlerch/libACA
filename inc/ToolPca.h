#if !defined(__ACA_Pca_HEADER_INCLUDED__)
#define __ACA_Pca_HEADER_INCLUDED__

#pragma once

#include "ErrorDef.h"

/*! \brief computation of principal component analysis
*/
class CPca
{
public:
    CPca(void) {};
    virtual ~CPca(void);


    /*! initializes PCA instance
    \param iNumFeatures number of rows in the input and output matrices
    \param iNumObs number of columns in the input and output matrices
    \return Error_t
    */
    Error_t init(int iNumFeatures, int iNumObs);

    /*! resets PCA instance
    \return Error_t
    */
    Error_t reset();

    /*! computes the principal components
    \param ppfRes matrix with principal components (user-allocated, to be written, dimensions equal input matrix)
    \param pfEigenValues eigenvalues of principle components (user-allocated, to be written, length equals number of features)
    \param ppfIn input matrix (dimensions iNumFeatures X iNumObs)
    \return Error_t
    */
    Error_t compPca(float **ppfRes, float *pfEigenValues, const float *const *const ppfIn);


    /*! computes covariance matrix
    \param ppfCovOut resulting covariance matrix (user-allocated, to be written, dimension iNumRows X iNumRows)
    \param ppfIn input matrix (dimensions iNumRows X iNumCols), each row is a variable
    \param iNumRows number of rows/features in the input  matrix
    \param iNumCols number of columns/observations in the input matrix
    \return Error_t
    */
    static Error_t compCov(float **ppfCovOut, const float *const *const  ppfIn, int iNumRows, int iNumCols);


    /*! computes singular value decomposition (ppfMat = ppfU * ppfW * ppfV)
    \param ppfU (user-allocated, to be written, dimension iNumRows X iNumCols)
    \param ppfW  (user-allocated, to be written, dimension iNumCols X iNumCols)
    \param ppfV (user-allocated, to be written, dimension iNumCols X iNumCols)
    \param ppfMat input matrix (dimension iNumRows X iNumCols)
    \param iNumRows number of rows in the input  matrix
    \param iNumCols number of columns/in the input matrix
    \param iMaxIterations maximum number of iterations
    \return Error_t
    */
    static Error_t calcSVD(float **ppfU, float **ppfW, float **ppfV, const float *const *const  ppfMat, int iNumRows, int iNumCols, int iMaxIterations = 100);

private:
    CPca(const CPca &that);     //!< disallow copy construction   
    CPca &operator=(const CPca &c);

    static float matPythag(float dA, float dB);

    int m_iNumFeatures = 0, //!< number of features
        m_iNumObs = 0; //!< number of observations

    float **m_ppfProcTmp = 0; //!< temporary processing memoru
    float **m_ppfU = 0; //!< SVD result: U
    float **m_ppfW = 0; //!< SVD result: W
    float **m_ppfV = 0; //!< SVD result: V
    int *piSortIndices = 0; //!< array for sorted indices
};

#endif // __ACA_Pca_HEADER_INCLUDED__
