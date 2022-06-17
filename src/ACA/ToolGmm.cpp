
#include "Matrix.h"
#include "Vector.h"
#include "Synthesis.h"
#include "ToolPca.h"

#include "ToolGmm.h"

CGmm::~CGmm(void) { reset(); }

Error_t CGmm::init(CGmmResult *pCResult, int iK, int iNumFeatures, int iNumObs, int iMaxIter)
{
    if (!pCResult || iK < 1 || iNumFeatures < 1 || iNumObs < 1 || iMaxIter < 1)
        return Error_t::kFunctionInvalidArgsError;

    reset();

    // set variables
    m_iMaxIter = iMaxIter;
    m_iNumFeatures = iNumFeatures;
    m_iNumObs = iNumObs;
    m_iK = iK;

    // init result class
    pCResult->init(iK, iNumFeatures);
    PrevState = CGmmResult(*pCResult);

    // alloc memory
    for (auto i = 0; i < 2; i++)
    {
        CVector::alloc(m_apfProc[i], std::max(iK, iNumFeatures));
        CMatrix::alloc(m_appfSigma[i], m_iNumFeatures, m_iNumFeatures);
    }
    CMatrix::alloc(m_ppfProb, m_iK, m_iNumObs);

    // all done
    m_bisInitialized = true;

    return Error_t::kNoError;
}

Error_t CGmm::reset()
{
    m_bisInitialized = false;

    for (auto i = 0; i < 2; i++)
    {
        CVector::free(m_apfProc[i]);
        CMatrix::free(m_appfSigma[i], m_iNumFeatures);
    }
    CMatrix::free(m_ppfProb, m_iK);

    m_iMaxIter = 300;
    m_iNumFeatures = 0;
    m_iNumObs = 0;
    m_iK = 0;

    return Error_t::kNoError;

}

Error_t CGmm::compGmm(CGmmResult *pCResult, const float *const *const  ppfFeatures)
{
    if (!pCResult || !ppfFeatures)
        return Error_t::kFunctionInvalidArgsError;
    if (!ppfFeatures[0])
        return Error_t::kFunctionInvalidArgsError;
    if (!m_bisInitialized)
        return Error_t::kFunctionIllegalCallError;

    // init: randomly selected data points as cluster means
    initState_(ppfFeatures, pCResult);

    // iterate
    for (auto i = 0; i < m_iMaxIter; i++)
    {
        // copy state to prev state
        PrevState = *pCResult;

        // update cluster means
        compProbabilities_(ppfFeatures, pCResult);

        // assign observations to clusters
        updateState_(ppfFeatures, pCResult);

        // check for change
        if (checkConverged_(pCResult))
            break;
    }
    return Error_t::kNoError;
}

void CGmm::initState_(const float *const *const  ppfFeatures, CGmmResult *pCCurrState)
{
    // generate some noise
    CSynthesis::genNoise(m_apfProc[0], m_iK);
    CVector::mulC_I(m_apfProc[0], m_iNumObs - 1.F, m_iK);

    CPca::compCov(m_appfSigma[0], ppfFeatures, m_iNumFeatures, m_iNumObs);
    for (auto k = 0; k < m_iK; k++)
    {
        int iIdx = CUtil::float2int<int>(m_apfProc[0][k]);
        assert(iIdx >= 0 && iIdx < m_iNumObs);

        // set sigma
        pCCurrState->setSigma(k, m_appfSigma[0]);

        // set mean
        for (auto v = 0; v < m_iNumFeatures; v++)
            pCCurrState->setMu(k, v, ppfFeatures[v][iIdx]);

        // set prior
        pCCurrState->setPrior(k, 1.F / m_iK);
    }
}

void CGmm::compProbabilities_(const float *const *const ppfFeatures, CGmmResult *pCCurrState)
{
    // compute gaussian per cluster per observation
    for (auto k = 0; k < m_iK; k++)
    {
        pCCurrState->getSigma(m_appfSigma[0], k);
        float fDet = CMatrix::det(m_appfSigma[0], m_iNumFeatures, m_iNumFeatures);
        if (fDet < 1e-30F)
        {
            CVector::setZero(m_ppfProb[k], m_iNumObs);
            continue;
        }

        float fNorm = static_cast<float>(1. / std::sqrt(std::pow(2. * M_PI, m_iNumFeatures) * fDet));
        CMatrix::inv_I(m_appfSigma[0], m_iNumFeatures, m_iNumFeatures);

        for (auto n = 0; n < m_iNumObs; n++)
        {
            for (auto v = 0; v < m_iNumFeatures; v++)
                m_apfProc[0][v] = ppfFeatures[v][n] - pCCurrState->getMu(k, v);

            CMatrix::mulMatColvec(m_apfProc[1], m_appfSigma[0], m_apfProc[0], m_iNumFeatures, m_iNumFeatures);

            m_ppfProb[k][n] = fNorm * std::exp(-.5F * CVector::mulScalar(m_apfProc[0], m_apfProc[1], m_iNumFeatures));
        }
        CVector::mulC_I(m_ppfProb[k], pCCurrState->getPrior(k), m_iNumObs);
    }

    // norm over clusters
    for (auto n = 0; n < m_iNumObs; n++)
    {
        float fSum = CMatrix::getSumCol(m_ppfProb, n, m_iK);

        if (fSum > 0)
            CMatrix::mulColC_I(m_ppfProb, 1.F / fSum, n, m_iK);
    }
}

void CGmm::updateState_(const float *const *const ppfFeatures, CGmmResult *pCCurrState)
{
    for (auto k = 0; k < m_iK; k++)
    {
        // update priors
        pCCurrState->setPrior(k, CVector::getMean(m_ppfProb[k], m_iNumObs));

        // update means
        float fNorm = CVector::getSum(m_ppfProb[k], m_iNumObs);
        if (fNorm < 1e-30F)
            fNorm = 1.F;
        for (auto v = 0; v < m_iNumFeatures; v++)
            pCCurrState->setMu(k, v, CVector::mulScalar(ppfFeatures[v], m_ppfProb[k], m_iNumObs) / fNorm);

        // update sigma
        CMatrix::setZero(m_appfSigma[0], m_iNumFeatures, m_iNumFeatures);
        for (auto n = 0; n < m_iNumObs; n++)
        {
            for (auto v = 0; v < m_iNumFeatures; v++)
                m_apfProc[0][v] = ppfFeatures[v][n] - pCCurrState->getMu(k, v);

            CMatrix::mulColvecRowvec(m_appfSigma[1], m_apfProc[0], m_apfProc[0], m_iNumFeatures, m_iNumFeatures);
            CMatrix::mulC_I(m_appfSigma[1], m_ppfProb[k][n], m_iNumFeatures, m_iNumFeatures);
            CMatrix::add_I(m_appfSigma[0], m_appfSigma[1], m_iNumFeatures, m_iNumFeatures);
        }
        fNorm = CVector::getSum(m_ppfProb[k], m_iNumObs);
        if (fNorm < 1e-30F)
            fNorm = 1.F;
        CMatrix::mulC_I(m_appfSigma[0], 1.F / fNorm, m_iNumFeatures, m_iNumFeatures);
        pCCurrState->setSigma(k, m_appfSigma[0]);
    }
}

bool CGmm::checkConverged_(CGmmResult *pCCurrState)
{
    float fSum = 0;
    for (auto k = 0; k < m_iK; k++)
    {
        for (auto v = 0; v < m_iNumFeatures; v++)
            fSum += std::abs(pCCurrState->getMu(k, v) - PrevState.getMu(k, v));
    }

    if (fSum / m_iK <= 1e-20F)
        return true;

    return false;
}

CGmmResult::~CGmmResult(void) { reset(); }

CGmmResult &CGmmResult::operator=(const CGmmResult &that)
{
    // should be all the same
    if (this->m_iK != that.m_iK || this->m_iNumFeatures != that.m_iNumFeatures || this->m_bIsInitialized != that.m_bIsInitialized)
        this->init(that.m_iK, that.m_iNumFeatures);

    CMatrix::copy(this->m_ppfMu, that.m_ppfMu, this->m_iK, this->m_iNumFeatures);
    CVector::copy(this->m_pfPrior, that.m_pfPrior, this->m_iK);
    for (auto i = 0; i < kSigma; i++)
    {
        for (auto k = 0; k < this->m_iK; k++)
            CMatrix::copy(this->m_apppfSigma[i][k], that.m_apppfSigma[i][k], this->m_iNumFeatures, this->m_iNumFeatures);
    }

    return *this;
}

float CGmmResult::getProb(const float *pfQuery)
{
    float fProb = 0;
    for (auto k = 0; k < m_iK; k++)
    {
        float fDet = CMatrix::det(m_apppfSigma[kNormal][k], m_iNumFeatures, m_iNumFeatures);
        if (fDet < 1e-30F)
            continue;
        float fNorm = static_cast<float>(1. / std::sqrt(std::pow(2. * M_PI, m_iNumFeatures) * fDet));

        CVector::copy(m_apfProc[0], pfQuery, m_iNumFeatures);
        CVector::sub_I(m_apfProc[0], m_ppfMu[k], m_iNumFeatures);

        CMatrix::mulMatColvec(m_apfProc[1], m_apppfSigma[kInv][k], m_apfProc[0], m_iNumFeatures, m_iNumFeatures);

        fProb += this->getPrior(k) * fNorm * std::exp(-.5F * CVector::mulScalar(m_apfProc[0], m_apfProc[1], m_iNumFeatures));
    }

    return fProb;
}

CGmmResult::CGmmResult(const CGmmResult &that) :
    m_iK(that.m_iK),
    m_iNumFeatures(that.m_iNumFeatures),
    m_bIsInitialized(that.m_bIsInitialized)
{
    CMatrix::alloc(m_ppfMu, m_iK, m_iNumFeatures);
    CMatrix::copy(m_ppfMu, that.m_ppfMu, m_iK, m_iNumFeatures);

    CVector::alloc(m_pfPrior, m_iK);
    CVector::copy(m_pfPrior, that.m_pfPrior, m_iK);

    for (auto i = 0; i < kSigma; i++)
    {
        CVector::alloc(m_apppfSigma[i], m_iK);
        for (auto k = 0; k < m_iK; k++)
        {
            CMatrix::alloc(m_apppfSigma[i][k], m_iNumFeatures, m_iNumFeatures);
            CMatrix::copy(m_apppfSigma[i][k], that.m_apppfSigma[i][k], m_iNumFeatures, m_iNumFeatures);
        }
    }
}

int CGmmResult::getNumGaussians() const
{
    return m_iK;
}

int CGmmResult::getNumDimensions() const
{
    return m_iNumFeatures;
}

float CGmmResult::getMu(int iGaussianIdx, int iFeatureIdx) const
{
    return m_ppfMu[iGaussianIdx][iFeatureIdx];
}

float CGmmResult::getPrior(int iGaussianIdx) const
{
    return m_pfPrior[iGaussianIdx];
}

float CGmmResult::getSigma(int iGaussianIdx, int iRowIdx, int iColIdx) const
{
    return m_apppfSigma[kNormal][iGaussianIdx][iRowIdx][iColIdx];
}

void CGmmResult::getSigma(float **ppfSigma, int iGaussianIdx) const
{
    CMatrix::copy(ppfSigma, m_apppfSigma[kNormal][iGaussianIdx], m_iNumFeatures, m_iNumFeatures);
    return;
}

bool CGmmResult::isInitialized() const
{
    return m_bIsInitialized;
}

Error_t CGmmResult::init(int iK, int iNumFeatures)
{
    reset();

    m_iK = iK;
    m_iNumFeatures = iNumFeatures;

    CMatrix::alloc(m_ppfMu, m_iK, m_iNumFeatures);
    CVector::alloc(m_pfPrior, m_iK);

    for (auto i = 0; i < kSigma; i++)
    {
        CVector::alloc(m_apppfSigma[i], m_iK);
        for (auto k = 0; k < m_iK; k++)
            CMatrix::alloc(m_apppfSigma[i][k], m_iNumFeatures, m_iNumFeatures);
    }
    for (auto i = 0; i < 2; i++)
        CVector::alloc(m_apfProc[i], m_iNumFeatures);

    m_bIsInitialized = true;

    return Error_t::kNoError;
}

Error_t CGmmResult::reset()
{
    m_bIsInitialized = false;

    CMatrix::free(m_ppfMu, m_iK);
    CVector::free(m_pfPrior);

    for (auto i = 0; i < kSigma; i++)
    {
        for (auto k = 0; k < m_iK; k++)
            CMatrix::free(m_apppfSigma[i][k], m_iNumFeatures);
        CVector::free(m_apppfSigma[i]);
    }
    for (auto i = 0; i < 2; i++)
        CVector::free(m_apfProc[i]);

    m_iK = 0;
    m_iNumFeatures = 0;

    return Error_t::kNoError;
}

Error_t CGmmResult::setMu(int iGaussianIdx, int iFeatureIdx, float fParamValue)
{
    m_ppfMu[iGaussianIdx][iFeatureIdx] = fParamValue;

    return Error_t::kNoError;
}

Error_t CGmmResult::setPrior(int iGaussianIdx, float fParamValue)
{
    m_pfPrior[iGaussianIdx] = fParamValue;

    return Error_t::kNoError;
}

Error_t CGmmResult::setSigma(int iGaussianIdx, float **ppfSigma)
{
    CMatrix::copy(m_apppfSigma[kNormal][iGaussianIdx], ppfSigma, m_iNumFeatures, m_iNumFeatures);

    CMatrix::copy(m_apppfSigma[kInv][iGaussianIdx], ppfSigma, m_iNumFeatures, m_iNumFeatures);
    CMatrix::inv_I(m_apppfSigma[kInv][iGaussianIdx], m_iNumFeatures, m_iNumFeatures);

    return Error_t::kNoError;
}
