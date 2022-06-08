#if !defined(__Knn_HEADER_INCLUDED__)
#define __Knn_HEADER_INCLUDED__

#pragma once

#include "ErrorDef.h"

/*! \brief computation of k nearest neighbor
*/
class CKnn
{
public:
    CKnn(void) {};
    virtual ~CKnn(void);;


    /*! initializes Knn instance
    \param iNumFeatures number of rows in the input and output matrices
    \param iNumObservations number of columns in the input and output matrices
    \return Error_t
    */
    Error_t init(float** ppfTrainFeatures, int* piTrainClassIndices, int iNumFeatures, int iNumObservations);

    /*! resets Knn instance
    \return Error_t
    */
    Error_t reset();

    /*! classifies a new query vector
    \param ppfRes matrix with principal components (user-allocated, to be written, dimensions equal input matrix)
    \param pfEigenValues eigenvalues of principle components (user-allocated, to be written, length equals number of features)
    \param ppfIn input matrix (dimensions iNumFeatures X iNumObservations)
    \return int
    */
    int compKnn(float* pfQuery, int iK);


    /*! whole KNN in one function (doesn't need instantiation
    \param ppfCovOut resulting covariance matrix (user-allocated, to be written, dimension iNumRows X iNumRows)
    \param ppfIn input matrix (dimensions iNumRows X iNumCols), each row is a variable
    \param iNumRows number of rows/features in the input  matrix
    \param iNumCols number of columns/observations in the input matrix
    \return int
    */
    static int classify(float* pfQuery, float** ppfTrainFeatures, int* piTrainClassIndices, int iNumFeatures, int iNumObservations, int iK)
    {
        // compute distance to all training observations

        // sort distances

        // infer class
    }


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
    static Error_t calcSVD(float** ppfU, float** ppfW, float** ppfV, float** ppfMat, int iNumRows, int iNumCols, int iMaxIterations = 100);

private:
    CKnn(const CKnn& that);     //!< disallow copy construction   
    CKnn& operator=(const CKnn& c);

    static float matPythag(float dA, float dB);;

    int m_iNumFeatures = 0,
        m_iNumObs = 0;

    float** m_ppfProcTmp = 0;
    float** m_ppfU = 0;
    float** m_ppfW = 0;
    float** m_ppfV = 0;
    int* piSortIndices = 0;
};


#endif
