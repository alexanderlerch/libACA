
#include "Matrix.h"
#include "Vector.h"
#include "Synthesis.h"

#include "ToolSimpleKmeans.h"


CKmeans::~CKmeans(void)
{
    this->reset();
}

Error_t CKmeans::init(int iK, int iNumFeatures, int iNumObs, int iMaxIter)
{
    if (iK <= 0 || iNumFeatures <= 0 || iNumObs <= iK || iMaxIter < 1)
        return Error_t::kFunctionInvalidArgsError;

    m_iK = iK;
    m_iNumFeatures = iNumFeatures;
    m_iNumObs = iNumObs;
    m_iMaxIter = iMaxIter;

    CMatrix::alloc(m_appfClusterMeans[kPrev], m_iK, m_iNumFeatures);
    CMatrix::alloc(m_appfClusterMeans[kCurr], m_iK, m_iNumFeatures);

    CVector::alloc(m_pfProc, std::max(m_iK, m_iNumFeatures));
    CVector::alloc(m_pfDist, m_iK);
    CVector::alloc(m_piClusterSize, m_iK);

    m_bIsInitialized = true;

    return Error_t::kNoError;
}

Error_t CKmeans::reset()
{
    m_bIsInitialized = false;

    CMatrix::free(m_appfClusterMeans[kPrev], m_iK);
    CMatrix::free(m_appfClusterMeans[kCurr], m_iK);
    CVector::free(m_pfProc);
    CVector::free(m_pfDist);
    CVector::free(m_piClusterSize);

    m_iK = 0;
    m_iNumFeatures = 0;
    m_iNumObs = 0;
    m_iMaxIter = 0;

    return Error_t::kNoError;
}

Error_t CKmeans::compKmeans(int *piResult, const float *const *const ppfFeatures)
{
    if (!m_bIsInitialized)
        return Error_t::kFunctionIllegalCallError;
    if (!piResult || !ppfFeatures)
        return Error_t::kFunctionInvalidArgsError;
    if (!ppfFeatures[0])
        return Error_t::kFunctionInvalidArgsError;

    // init: randomly selected data points as cluster means
    initClusterMeans_(ppfFeatures);

    // assign observations to clusters
    assignClusterLabels_(piResult, ppfFeatures);

    // iterate
    for (auto i = 0; i < m_iMaxIter; i++)
    {
        // copy state to prev state
        CMatrix::copy(m_appfClusterMeans[kPrev], m_appfClusterMeans[kCurr], m_iK, m_iNumFeatures);

        // update cluster means
        compClusterMeans_(ppfFeatures, piResult);

        // re-init empty clusters
        reinitClusterMeans_(ppfFeatures);

        // assign observations to clusters
        assignClusterLabels_(piResult, ppfFeatures);

        // check for change
        if (checkConverged_())
            break;
    }

    return Error_t::kNoError;
}

void CKmeans::reinitClusterMeans_(const float *const *const ppfFeatures)
{
    if (CVector::getMin(m_piClusterSize, m_iK) > 0)
        return;

    CSynthesis::genNoise(m_pfProc, m_iK);
    CVector::mulC_I(m_pfProc, m_iNumObs - 1.F, m_iK);
    for (auto k = 0; k < m_iK; k++)
    {
        if (m_piClusterSize[k] > 0)
            continue;
        int iIdx = CUtil::float2int<int>(m_pfProc[k]);
        assert(iIdx >= 0 && iIdx < m_iNumObs);

        for (auto v = 0; v < m_iNumFeatures; v++)
            m_appfClusterMeans[kCurr][k][v] = ppfFeatures[v][iIdx];
    }
}

void CKmeans::initClusterMeans_(const float *const *const ppfFeatures)
{
    CSynthesis::genNoise(m_pfProc, m_iK);
    CVector::mulC_I(m_pfProc, m_iNumObs - 1.F, m_iK);
    for (auto k = 0; k < m_iK; k++)
    {
        int iIdx = CUtil::float2int<int>(m_pfProc[k]);
        assert(iIdx >= 0 && iIdx < m_iNumObs);

        for (auto v = 0; v < m_iNumFeatures; v++)
            m_appfClusterMeans[kCurr][k][v] = ppfFeatures[v][iIdx];
    }
}

void CKmeans::compClusterMeans_(const float *const *const ppfFeatures, const int *piResult)
{
    CMatrix::setZero(m_appfClusterMeans[kCurr], m_iK, m_iNumFeatures);
    for (auto n = 0; n < m_iNumObs; n++)
    {
        // add all observations per feature per cluster
        for (auto v = 0; v < m_iNumFeatures; v++)
            m_appfClusterMeans[kCurr][piResult[n]][v] += ppfFeatures[v][n];
    }

    // norm sum by number of entries
    for (auto k = 0; k < m_iK; k++)
    {
        if (!m_piClusterSize[k])
            continue;
        CVector::mulC_I(m_appfClusterMeans[kCurr][k], 1.F / m_piClusterSize[k], m_iNumFeatures);
    }
}

bool CKmeans::checkConverged_()
{
    float fSum = 0;
    int k = 0;
    CVector::copy(m_pfProc, m_appfClusterMeans[kCurr][k], m_iNumFeatures);
    CVector::sub_I(m_pfProc, m_appfClusterMeans[kPrev][k], m_iNumFeatures);
    while ((fSum += CVector::getSum(m_pfProc, m_iNumFeatures, true)) <= 0 && k < m_iK - 1)
    {
        k++;
        CVector::copy(m_pfProc, m_appfClusterMeans[kCurr][k], m_iNumFeatures);
        CVector::sub_I(m_pfProc, m_appfClusterMeans[kPrev][k], m_iNumFeatures);
    }
    if (fSum <= 0)
        return true;

    return false;
}

void CKmeans::assignClusterLabels_(int *piResult, const float *const *const ppfFeatures)
{
    CVector::setZero(m_piClusterSize, m_iK);
    for (auto n = 0; n < m_iNumObs; n++)
    {
        float fMin = 0;
        long long iMin = -1;

        CMatrix::getCol(m_pfProc, ppfFeatures, n, m_iNumFeatures);
        // compute distance to all training observations
        for (auto k = 0; k < m_iK; k++)
            m_pfDist[k] = CVector::distEuclidean(m_appfClusterMeans[kCurr][k], m_pfProc, m_iNumFeatures);

        // add observation to closets cluster
        CVector::findMin(m_pfDist, fMin, iMin, m_iK);
        piResult[n] = static_cast<int>(iMin);
        m_piClusterSize[iMin]++;
    }
}
