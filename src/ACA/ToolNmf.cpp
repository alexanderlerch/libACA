#include <algorithm>

#include "Matrix.h"

#include "ToolNmf.h"

const float  CNmf::m_kMinOffset = 1e-30F;

CNmf::CNmf(void)
{
    reset();
}

CNmf::~CNmf(void)
{
    reset();
}

Error_t CNmf::init(CNmfResult *pCNmfResult, int iRank, int iNumRows, int iNumCols, int iMaxIter /*= 300*/, float fSparsity /*= 0*/)
{
    if (!pCNmfResult || iRank <= 0 || iNumRows <= 0 || iNumCols <= 0 || iMaxIter <= 0)
        return Error_t::kFunctionInvalidArgsError;

    pCNmfResult->init(iRank, iNumRows, iNumCols);
    m_iNumRows = iNumRows;

    m_iMaxIter = iMaxIter;
    m_fSparsity = fSparsity;

    CMatrix::alloc(m_ppfX, iNumRows, iNumCols);
    CMatrix::alloc(m_ppfOnes, iNumRows, iNumCols);
    CMatrix::setValue(m_ppfOnes, 1.F, iNumRows, iNumCols);

    m_iAllocSize = std::max(std::max(iRank, iNumRows), iNumCols);
    CMatrix::alloc(m_ppfTransp, m_iAllocSize, m_iAllocSize);
    CMatrix::alloc(m_ppfNum, m_iAllocSize, m_iAllocSize);
    CMatrix::alloc(m_ppfDenom, m_iAllocSize, m_iAllocSize);

    m_bIsInitialized = true;

    return Error_t::kNoError;
}

Error_t CNmf::reset()
{
    m_bIsInitialized = false;
    CMatrix::free(m_ppfX, m_iNumRows);
    CMatrix::free(m_ppfOnes, m_iNumRows);
    CMatrix::free(m_ppfTransp, m_iAllocSize);
    CMatrix::free(m_ppfNum, m_iAllocSize);
    CMatrix::free(m_ppfDenom, m_iAllocSize);

    m_iMaxIter = 0;
    m_fSparsity = 0;

    return Error_t::kNoError;
}

Error_t CNmf::compNmf(CNmfResult *pCNmfResult, const float *const *const ppfIn)
{

    if (!pCNmfResult || !ppfIn)
        return Error_t::kFunctionInvalidArgsError;

    if (!m_bIsInitialized || !pCNmfResult->isInitialized())
        return Error_t::kFunctionIllegalCallError;

    enum Cost_t
    {
        kStartCost,
        kPrevCost,
        kCurrCost,

        kNumCosts
    };
    float afCost[kNumCosts] = { 0 };

    for (int k = 0; k < m_iMaxIter; k++)
    {
        // this is the heart of it
        afCost[kCurrCost] = runNmfIter(pCNmfResult, ppfIn);

        assert(!std::isnan(afCost[kCurrCost]));

        // check if we can stop
        if (k > 0)
        {
            if (std::abs((afCost[kCurrCost] - afCost[kPrevCost]) / (afCost[kStartCost] - afCost[kCurrCost] + m_kMinOffset)) < .001F)
                break;
        }
        else
            afCost[kStartCost] = afCost[kCurrCost];

        afCost[kPrevCost] = afCost[kCurrCost];
    }
    return Error_t::kNoError;
}

float CNmf::runNmfIter(CNmfResult *pCNmfResult, const float *const *const ppfIn)
{
    float **ppfW = pCNmfResult->getMatPointer(CNmfResult::kW);
    float **ppfH = pCNmfResult->getMatPointer(CNmfResult::kH);
    float **ppfXHat = pCNmfResult->getMatPointer(CNmfResult::kXHat);

    int iRank = pCNmfResult->getMatCols(CNmfResult::kW);
    int iNumObs = pCNmfResult->getMatCols(CNmfResult::kH);
    int iNumBins = pCNmfResult->getMatRows(CNmfResult::kW);

    // there are special cases were we don't want to update both matrices
    const bool bUpdateH = true,
        bUpdateW = true;

    // compute current estimate
    CMatrix::mulMatMat(ppfXHat, ppfW, ppfH, iNumBins, iRank, iRank, iNumObs);
    CMatrix::addC_I(ppfXHat, m_kMinOffset, iNumBins, iNumObs);

    //m_ppfX
    CMatrix::copy(m_ppfX, ppfIn, iNumBins, iNumObs);
    CMatrix::div_I(m_ppfX, ppfXHat, iNumBins, iNumObs);

    // update H
    if (bUpdateH)
    {
        CMatrix::transpose(m_ppfTransp, ppfW, iNumBins, iRank);

        // denominator
        CMatrix::mulMatMat(m_ppfDenom, m_ppfTransp, m_ppfOnes, iRank, iNumBins, iNumBins, iNumObs);
        CMatrix::addC_I(m_ppfDenom, m_fSparsity + m_kMinOffset, iRank, iNumObs);

        // numerator
        CMatrix::mulMatMat(m_ppfNum, m_ppfTransp, m_ppfX, iRank, iNumBins, iNumBins, iNumObs);
        CMatrix::div_I(m_ppfNum, m_ppfDenom, iRank, iNumObs);
        CMatrix::mul_I(ppfH, m_ppfNum, iRank, iNumObs);

        // clean small values
        CMatrix::setZeroBelowThresh(ppfH, iRank, iNumObs, m_kMinOffset);
    }

    // update W
    if (bUpdateW)
    {
        CMatrix::transpose(m_ppfTransp, ppfH, iRank, iNumObs);

        // denominator
        CMatrix::mulMatMat(m_ppfDenom, m_ppfOnes, m_ppfTransp, iNumBins, iNumObs, iNumObs, iRank);
        CMatrix::addC_I(m_ppfDenom, m_kMinOffset, iNumBins, iRank);

        // numerator
        CMatrix::mulMatMat(m_ppfNum, m_ppfX, m_ppfTransp, iNumBins, iNumObs, iNumObs, iRank);
        CMatrix::div_I(m_ppfNum, m_ppfDenom, iNumBins, iRank);
        CMatrix::mul_I(ppfW, m_ppfNum, iNumBins, iRank);

        // normalize
        CMatrix::vecnorm_I(ppfW, iNumBins, iRank);

        // clean small values
        CMatrix::setZeroBelowThresh(ppfW, iNumBins, iRank, m_kMinOffset);
    }

    // compute error
    CMatrix::mulMatMat(ppfXHat, ppfW, ppfH, iNumBins, iRank, iRank, iNumObs);
    CMatrix::addC_I(ppfXHat, m_kMinOffset, iNumBins, iNumObs);

    // deal with sparsity
    CMatrix::addC_I(ppfXHat, m_fSparsity * CMatrix::getNorm(ppfXHat, iNumBins, iNumObs), iNumBins, iNumObs);

    return CMatrix::calcKlDivergence(ppfIn, ppfXHat, iNumBins, iNumObs);
}


/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
CNmfResult::CNmfResult()
{}

CNmfResult::~CNmfResult(void)
{
    reset();
}

Error_t CNmfResult::init(int iRank, int iNumRows, int iNumCols)
{
    if (iRank <= 0 || iNumRows <= 0 || iNumCols <= 0)
        return Error_t::kFunctionInvalidArgsError;

    m_aaiMatrixDims[kW][0] = iNumRows;
    m_aaiMatrixDims[kW][1] = iRank;
    m_aaiMatrixDims[kH][0] = iRank;
    m_aaiMatrixDims[kH][1] = iNumCols;
    m_aaiMatrixDims[kXHat][0] = iNumRows;
    m_aaiMatrixDims[kXHat][1] = iNumCols;

    // alloc memory
    for (auto m = 0; m < kNumMatrices; m++)
        CMatrix::alloc(m_appfMatrices[m], m_aaiMatrixDims[m][0], m_aaiMatrixDims[m][1]);

    // init with random values
    CMatrix::setRand(m_appfMatrices[kW], m_aaiMatrixDims[kW][0], m_aaiMatrixDims[kW][1]);
    CMatrix::setRand(m_appfMatrices[kH], m_aaiMatrixDims[kH][0], m_aaiMatrixDims[kH][1]);

    // normalize
    CMatrix::vecnorm_I(m_appfMatrices[kW], m_aaiMatrixDims[kW][0], m_aaiMatrixDims[kW][1]);

    m_bIsInitialized = true;

    return Error_t::kNoError;
}


Error_t CNmfResult::reset(void)
{
    m_bIsInitialized = false;

    for (auto m = 0; m < kNumMatrices; m++)
    {
        CMatrix::free(m_appfMatrices[m], m_aaiMatrixDims[m][0]);
        CVector::setZero(m_aaiMatrixDims[m], 2);
    }

    return Error_t::kNoError;
}

int CNmfResult::getMatCols(NmfMatrices_t eMatIdx) const
{
    return  m_aaiMatrixDims[eMatIdx][1];
}

int CNmfResult::getMatRows(NmfMatrices_t eMatIdx) const
{
    return  m_aaiMatrixDims[eMatIdx][0];
}

void CNmfResult::getMatDims(NmfMatrices_t eMatIdx, int &iNumRows, int &iNumCols) const
{
    iNumRows = getMatRows(eMatIdx);
    iNumCols = getMatCols(eMatIdx);
    return;
}

Error_t CNmfResult::getMat(float **ppfDest, NmfMatrices_t eMatIdx)
{
    if (!m_bIsInitialized)
        return Error_t::kFunctionIllegalCallError;
    if (!ppfDest)
        return Error_t::kFunctionInvalidArgsError;
    if (!ppfDest[0])
        return Error_t::kFunctionInvalidArgsError;

    CMatrix::copy(ppfDest, m_appfMatrices[eMatIdx], m_aaiMatrixDims[eMatIdx][0], m_aaiMatrixDims[eMatIdx][1]);

    return Error_t::kNoError;
}

bool CNmfResult::isInitialized() const
{
    return m_bIsInitialized;
}

float **CNmfResult::getMatPointer(NmfMatrices_t eMatIdx) const
{
    return m_appfMatrices[eMatIdx];
}
