
#include "Vector.h"
#include "Matrix.h"
#include "Util.h"

#include "ToolSimpleDtw.h"

const int CDtw::aiDecrement[kNumDirections][kNumMatrixDimensions] =
{
    { 0, -1 }, // kHoriz
    { -1, 0 }, // kVert
    { -1, -1 } // kDiag
};

CDtw::CDtw(void)
{
    CVector::setZero(m_apfCost, kNumVectors);
    CVector::setZero(m_aiMatrixDims, kNumMatrixDimensions);

    reset();
}

CDtw::~CDtw(void)
{
    reset();
}

Error_t CDtw::init(int iNumRows, int iNumCols)
{
    if (iNumRows <= 0 || iNumCols <= 0)
        return Error_t::kFunctionInvalidArgsError;

    reset();

    m_aiMatrixDims[kRow] = iNumRows;
    m_aiMatrixDims[kCol] = iNumCols;

    // allocate memory
    for (int i = 0; i < kNumVectors; i++)
        CVector::alloc(m_apfCost[i], m_aiMatrixDims[kCol]);

    CMatrix::alloc(m_ppePathIdx, m_aiMatrixDims[kRow], m_aiMatrixDims[kCol]);

    // all done here
    m_bIsInitialized = true;

    return Error_t::kNoError;
}

Error_t CDtw::reset()
{
    m_bIsInitialized = false;
    m_bWasProcessed = false;

    CMatrix::free(m_ppePathIdx, m_aiMatrixDims[kRow]);

    for (int i = 0; i < kNumVectors; i++)
        CVector::free(m_apfCost[i]);

    m_aiMatrixDims[kRow] = 0;
    m_aiMatrixDims[kCol] = 0;
    m_fOverallCost = 0;
    m_iPathLength = 0;

    return Error_t::kNoError;
}

Error_t CDtw::compDtw(const float *const *const ppfDistanceMatrix)
{
    if (!m_bIsInitialized)
        return Error_t::kNotInitializedError;

    if (!ppfDistanceMatrix)
        return Error_t::kFunctionInvalidArgsError;

    float fFirstColCost = 0;//!< variable that will only contain the cost of the first column (for the current row to be processed)

    // initialize
    CVector::setZero(m_apfCost[kRowNext], m_aiMatrixDims[kCol]);
    m_apfCost[kRowCurr][0] = ppfDistanceMatrix[0][0];
    fFirstColCost = ppfDistanceMatrix[0][0];
    m_ppePathIdx[0][0] = kDiag;
    for (int j = 1; j < m_aiMatrixDims[kCol]; j++)
    {
        m_apfCost[kRowCurr][j] = m_apfCost[kRowCurr][j - 1] + ppfDistanceMatrix[0][j];
        m_ppePathIdx[0][j] = kHoriz;
    }
    for (int i = 1; i < m_aiMatrixDims[kRow]; i++)
        m_ppePathIdx[i][0] = kVert;

    // compute cost 'matrix' (we only use two rows here) and store backtracking path
    for (int i = 1; i < m_aiMatrixDims[kRow]; i++)
    {
        // init the variables we need for the current row (remember the cost in the first column, and then increment it)
        m_apfCost[kRowCurr][0] = fFirstColCost;
        fFirstColCost += ppfDistanceMatrix[i][0];
        m_apfCost[kRowNext][0] = fFirstColCost;

        for (int j = 1; j < m_aiMatrixDims[kCol]; j++)
        {
            m_ppePathIdx[i][j] = static_cast<unsigned char>(findMin_(m_apfCost[kRowNext][j - 1],   // horiz
                m_apfCost[kRowCurr][j],     // vert
                m_apfCost[kRowCurr][j - 1],   // diag
                m_apfCost[kRowNext][j]));   // minimum cost output
            m_apfCost[kRowNext][j] += ppfDistanceMatrix[i][j];
        }

        // swap the pointers of our two buffers as we proceed to the next row
        CUtil::swap(m_apfCost[kRowCurr], m_apfCost[kRowNext]);
    }

    // all done
    m_bWasProcessed = true;

    return Error_t::kNoError;
}

int CDtw::getPathLength()
{
    int i = m_aiMatrixDims[kRow] - 1;
    int j = m_aiMatrixDims[kCol] - 1;

    if (!m_bWasProcessed)
        return 0;

    m_iPathLength = 1;

    // dummy traceback
    while (i > 0 || j > 0)
    {
        int iNewI = i + aiDecrement[m_ppePathIdx[i][j]][kRow];
        j += aiDecrement[m_ppePathIdx[i][j]][kCol];
        i = iNewI;
        m_iPathLength++;
    }

    return m_iPathLength;
}

float CDtw::getPathCost() const
{
    return m_apfCost[kRowCurr][m_aiMatrixDims[kCol] - 1];
}

Error_t CDtw::getPath(int **ppiPathResult) const
{
    if (!ppiPathResult)
        return Error_t::kFunctionInvalidArgsError;

    if (m_iPathLength <= 0)
        return Error_t::kFunctionIllegalCallError;

    int iIdx = m_iPathLength - 1;

    // init
    ppiPathResult[kRow][0] = 0;
    ppiPathResult[kCol][0] = 0;
    ppiPathResult[kRow][iIdx] = m_aiMatrixDims[kRow] - 1;
    ppiPathResult[kCol][iIdx] = m_aiMatrixDims[kCol] - 1;

    // traceback
    while (iIdx > 0)
    {
        ppiPathResult[kRow][iIdx - 1] = ppiPathResult[kRow][iIdx] + aiDecrement[m_ppePathIdx[ppiPathResult[kRow][iIdx]][ppiPathResult[kCol][iIdx]]][kRow];
        ppiPathResult[kCol][iIdx - 1] = ppiPathResult[kCol][iIdx] + aiDecrement[m_ppePathIdx[ppiPathResult[kRow][iIdx]][ppiPathResult[kCol][iIdx]]][kCol];
        iIdx--;
    }

    return Error_t::kNoError;
}

