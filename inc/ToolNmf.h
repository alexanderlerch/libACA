#if !defined(__Nmf_HEADER_INCLUDED__)
#define __Nmf_HEADER_INCLUDED__

#pragma once

#include "ErrorDef.h"

class CNmfResult
{
friend class CNmf;

public:
    enum NmfMatrices_t
    {
        kW,
        kH,
        kXHat,

        kNumMatrices
    };
    CNmfResult();
    virtual ~CNmfResult(void);

    int getMatCols(NmfMatrices_t eMatIdx) const;
    int getMatRows(NmfMatrices_t eMatIdx) const;
    void getMatDims(NmfMatrices_t eMatIdx, int& iNumRows, int& iNumCols) const;

    Error_t getMat(float** ppfDest, NmfMatrices_t eMatIdx);

    bool isInitialized() const;

protected:
    Error_t init(int iRank, int iNumRows, int iNumCols);
    Error_t reset();
    float** getMatPointer(NmfMatrices_t eMatIdx) const;
private:

    float** m_appfMatrices[kNumMatrices] = { 0 };

    int m_aaiMatrixDims[kNumMatrices][2] = { 0 };

    bool m_bIsInitialized = false;
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
    Error_t init (CNmfResult* pCNmfResult, int iRank, int iNumRows, int iNumCols, int iMaxIter = 300, float fSparsity = 0);
    
    /*! resets all internal class members
    \return Error_t
    */
    Error_t reset ();

    Error_t compNmf (CNmfResult *pCNmfResult, float** ppfInput);
 
 
private:
    CNmf(const CNmf& that);     //!< disallow copy construction   
    CNmf& operator=(const CNmf& c);

    float runNmfIter(CNmfResult* pNmfResult, float** ppfInput);

    float** m_ppfX = 0,
        ** m_ppfOnes = 0,
        ** m_ppfTransp = 0,
        **m_ppfNum = 0,
        **m_ppfDenom = 0;
    float m_fSparsity = 0;
    int m_iMaxIter = 0;
    int m_iAllocSize = 0;
    int m_iNumRows = 0;

    bool m_bIsInitialized = false;  //!< true if init has been called

    static const float m_kMinOffset;
};


#endif
