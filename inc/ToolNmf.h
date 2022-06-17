#if !defined(__ACA_Nmf_HEADER_INCLUDED__)
#define __ACA_Nmf_HEADER_INCLUDED__

#pragma once

#include "ErrorDef.h"


/*! \brief class holding the result of Non-Negative Matrix Factorization executed with ::CNmf
*/
class CNmfResult
{
    friend class CNmf;

public:
    enum NmfMatrices_t
    {
        kW, //!< template matrix
        kH, //!< activation matrix
        kXHat, //!< reconstructed input matrix

        kNumMatrices
    };
    CNmfResult();
    virtual ~CNmfResult(void);


    /*! returns the number of columns in the matrix specified in eMatIdx
    \param eMatIdx
    \return int
    */
    int getMatCols(NmfMatrices_t eMatIdx) const;

    /*! returns the number of rows in the matrix specified in eMatIdx
    \param eMatIdx
    \return int
    */
    int getMatRows(NmfMatrices_t eMatIdx) const;

    /*! writes the number of rows and columns in the matrix specified in eMatIdx
    \param eMatIdx
    \param iNumRows number of rows
    \param iNumCols number of columns
    */
    void getMatDims(NmfMatrices_t eMatIdx, int &iNumRows, int &iNumCols) const;


    /*! returns the number of rows in the matrix specified in eMatIdx
    \param ppfDest matrix to copy to (user allocated, dimensions see CNmfResult::getMatDims
    \param eMatIdx matrix to copy
    \return Error_t
    */
    Error_t getMat(float **ppfDest, NmfMatrices_t eMatIdx);

    /*! returns true if the instance is initialized
    \return bool
    */
    bool isInitialized() const;

protected:
    Error_t init(int iRank, int iNumRows, int iNumCols);
    Error_t reset();
    float **getMatPointer(NmfMatrices_t eMatIdx) const;
private:
    CNmfResult(const CNmfResult &that); //!< disallow copy construction   

    float **m_appfMatrices[kNumMatrices] = { 0 }; //!< arry holding all matrix pointers

    int m_aaiMatrixDims[kNumMatrices][2] = { { 0 } }; //!< array holding the various matrix dimensions

    bool m_bIsInitialized = false; //!< flag to indicate whether instance is initialized
};

/*! \brief computation of non-negative matrix factorization
*/
class CNmf
{
public:

    CNmf(void);
    virtual ~CNmf(void);

    /*! initializes the class with the size of the distance matrix
    \param pCNmfResult class holding the result to be initialized (user allocated)
    \param iRank NMF rank
    \param iNumRows number of rows of matrix to be factorized
    \param iNumCols number of columns of matrix to be factorized
    \param iMaxIter maximum number of iterations
    \param fSparsity sparsity weight
    \return Error_t
    */
    Error_t init(CNmfResult *pCNmfResult, int iRank, int iNumRows, int iNumCols, int iMaxIter = 300, float fSparsity = 0);

    /*! resets all internal class members
    \return Error_t
    */
    Error_t reset();

    Error_t compNmf(CNmfResult *pCNmfResult, const float *const *const  ppfIn);


private:
    CNmf(const CNmf &that);     //!< disallow copy construction   
    CNmf &operator=(const CNmf &c);

    float runNmfIter(CNmfResult *pNmfResult, const float *const *const  ppfIn);

    float **m_ppfX = 0, //!< input matrix 
        **m_ppfOnes = 0, //!< matrix with ones
        **m_ppfTransp = 0, //!< matrix for transpositions
        **m_ppfNum = 0, //!< numerator matrix
        **m_ppfDenom = 0; //!< denominator matrix

    float m_fSparsity = 0; //!< sparsity weight

    int m_iMaxIter = 0; //!< maximum number of iteration
    int m_iAllocSize = 0; //!< temp variable
    int m_iNumRows = 0; //!< number of rows in matrix

    bool m_bIsInitialized = false;  //!< true if init has been called

    static const float m_kMinOffset;  //!< small constant for avoiding zeros
};

#endif // __ACA_Nmf_HEADER_INCLUDED__
