#include "ToolGmm.h"

#include "Matrix.h"
#include "Vector.h"
#include "Synthesis.h"
#include "ToolPca.h"

CGmm::~CGmm(void) { reset(); }

Error_t CGmm::init(CGmmResult* pCResult, int iK, int iNumFeatures, int iNumObservations, int iMaxIter)
{
    reset();

    // set variables
    m_iMaxIter = iMaxIter;
    m_iNumFeatures = iNumFeatures;
    m_iNumObs = iNumObservations;
    m_iK = iK;

    // init result class
    pCResult->init(iK, iNumFeatures);
    PrevState = CGmmResult(*pCResult);


    // alloc memory
    for (auto i = 0; i < 2; i++)
    {
        CVector::alloc(m_apfProc[i], iK);
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

Error_t CGmm::compGmm(CGmmResult* pCResult, float** ppfFeatures)
{

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

void CGmm::initState_(float** ppfFeatures, CGmmResult* pCCurrState)
{
    // generate some noise
    CSynthesis::genNoise(m_apfProc[0], m_iK);
    CVector::addC_I(m_apfProc[0], 1.F, m_iK);
    CVector::mulC_I(m_apfProc[0], (m_iNumObs - 1) / 2.F, m_iK);

    CPca::compCov(m_appfSigma[0], ppfFeatures, m_iNumFeatures, m_iNumObs);
    for (auto k = 0; k < m_iK; k++)
    {
        int iIdx = CUtil::float2int<int>(m_apfProc[0][k]);
        assert(iIdx > 0 && iIdx < m_iNumObs);

        // set sigma
        pCCurrState->setSigma(k, m_appfSigma[0]);

        // set mean
        for (auto v = 0; v < m_iNumFeatures; v++)
            pCCurrState->setMu(k, v, ppfFeatures[v][iIdx]);

        // set prior
        pCCurrState->setPrior(k, 1.F / m_iK);
    }
}

void CGmm::compProbabilities_(float** ppfFeatures, CGmmResult* pCCurrState)
{
    // compute gaussian per cluster per observation
    for (auto k = 0; k < m_iK; k++)
    {
        pCCurrState->getSigma(m_appfSigma[0], k);
        float fNorm = static_cast<float>(1. / std::sqrt(std::pow(2. * M_PI, m_iNumFeatures) * CMatrix::det(m_appfSigma[0], m_iNumFeatures, m_iNumFeatures)));
        CMatrix::inv_I(m_appfSigma[0], m_iNumFeatures, m_iNumFeatures);

        for (auto n = 0; n < m_iNumObs; n++)
        {
            for (auto v = 0; v < m_iNumFeatures; v++)
                m_apfProc[0][v] = ppfFeatures[v][n] - pCCurrState->getMu(k, v);

            CMatrix::mulMatColVec(m_apfProc[1], m_appfSigma[0], m_apfProc[0], m_iNumFeatures, m_iNumFeatures);

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

void CGmm::updateState_(float** ppfFeatures, CGmmResult* pCCurrState)
{
    for (auto k = 0; k < m_iK; k++)
    {
        // update priors
        pCCurrState->setPrior(k, CVector::getMean(m_ppfProb[k], m_iNumObs));

        // update means
        for (auto v = 0; v < m_iNumFeatures; v++)
            pCCurrState->setMu(k, v, CVector::mulScalar(ppfFeatures[v], m_ppfProb[k], m_iNumObs) / CVector::getSum(m_ppfProb[k], m_iNumObs));

        // update sigma
        CMatrix::setZero(m_appfSigma[0], m_iNumFeatures, m_iNumFeatures);
        for (auto n = 0; n < m_iNumObs; n++)
        {
            for (auto v = 0; v < m_iNumFeatures; v++)
                m_apfProc[0][v] = ppfFeatures[v][n] - pCCurrState->getMu(k, v);

            CMatrix::mulColVecRowVec(m_appfSigma[1], m_apfProc[0], m_apfProc[0], m_iNumFeatures, m_iNumFeatures);
            CMatrix::mulC_I(m_appfSigma[1], m_ppfProb[k][n], m_iNumFeatures, m_iNumFeatures);
            CMatrix::add_I(m_appfSigma[0], m_appfSigma[1], m_iNumFeatures, m_iNumFeatures);
        }
        CMatrix::mulC_I(m_appfSigma[0], 1.F / CVector::getSum(m_ppfProb[k], m_iNumObs), m_iNumFeatures, m_iNumFeatures);
        pCCurrState->setSigma(k, m_appfSigma[0]);
    }
}

bool CGmm::checkConverged_(CGmmResult* pCCurrState)
{
    float fSum = 0;
    for (auto k = 0; k < m_iK; k++)
    {
        for (auto v = 0; v < m_iNumFeatures; v++)
            fSum += std::abs(pCCurrState->getMu(k, v) - PrevState.getMu(k, v));
    }

    if (fSum <= 1e-20F)
        return true;

    return false;
}

CGmmResult::~CGmmResult(void) { reset(); }

CGmmResult& CGmmResult::operator=(const CGmmResult& that)
{
    // should be all the same
    if (this->m_iK != that.m_iK || this->m_iNumFeatures != that.m_iNumFeatures || this->m_bIsInitialized != that.m_bIsInitialized)
        this->init(that.m_iK, that.m_iNumFeatures);

    CMatrix::copy(this->m_ppfMu, that.m_ppfMu, this->m_iK, this->m_iNumFeatures);
    CVector::copy(this->m_pfPrior, that.m_pfPrior, this->m_iK);
    for (auto k = 0; k < this->m_iK; k++)
        CMatrix::copy(this->m_pppfSigma[k], that.m_pppfSigma[k], this->m_iNumFeatures, this->m_iNumFeatures);

    return *this;
}

CGmmResult::CGmmResult(const CGmmResult& that) :
    m_iK(that.m_iK),
    m_iNumFeatures(that.m_iNumFeatures),
    m_bIsInitialized(that.m_bIsInitialized)
{
    CMatrix::alloc(m_ppfMu, m_iK, m_iNumFeatures);
    CMatrix::copy(m_ppfMu, that.m_ppfMu, m_iK, m_iNumFeatures);

    CVector::alloc(m_pfPrior, m_iK);
    CVector::copy(m_pfPrior, that.m_pfPrior, m_iK);

    CVector::alloc(m_pppfSigma, m_iK);
    for (auto k = 0; k < m_iK; k++)
    {
        CMatrix::alloc(m_pppfSigma[k], m_iNumFeatures, m_iNumFeatures);
        CMatrix::copy(m_pppfSigma[k], that.m_pppfSigma[k], m_iNumFeatures, m_iNumFeatures);
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
    return m_pppfSigma[iGaussianIdx][iRowIdx][iColIdx];
}

void CGmmResult::getSigma(float** ppfSigma, int iGaussianIdx) const
{
    CMatrix::copy(ppfSigma, m_pppfSigma[iGaussianIdx], m_iNumFeatures, m_iNumFeatures);
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

    CVector::alloc(m_pppfSigma, m_iK);
    for (auto k = 0; k < m_iK; k++)
        CMatrix::alloc(m_pppfSigma[k], m_iNumFeatures, m_iNumFeatures);

    m_bIsInitialized = true;

    return Error_t::kNoError;
}

Error_t CGmmResult::reset()
{
    m_bIsInitialized = false;

    CMatrix::free(m_ppfMu, m_iK);
    CVector::free(m_pfPrior);

    for (auto k = 0; k < m_iK; k++)
        CMatrix::free(m_pppfSigma[k], m_iNumFeatures);
    CVector::free(m_pppfSigma);

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

Error_t CGmmResult::setSigma(int iGaussianIdx, float** ppfSigma)
{
    CMatrix::copy(m_pppfSigma[iGaussianIdx], ppfSigma, m_iNumFeatures, m_iNumFeatures);

    return Error_t::kNoError;
}
