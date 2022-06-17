
#include "Util.h"
#include "Vector.h"
#include "Fft.h"

#include "ToolCcf.h"

CCcf::CCcf(void)
{
    m_pCFft = new CFft();
}

CCcf::~CCcf(void)
{
    reset();

    delete m_pCFft;
}

Error_t CCcf::init(int iBlockLength)
{
    if (iBlockLength <= 0)
        return Error_t::kFunctionInvalidArgsError;

    if (m_bIsInitialized)
        reset();

    m_iBlockLength = iBlockLength;
    if (CUtil::isPowOf2(m_iBlockLength))
        m_iFftLength = 2 * m_iBlockLength;
    else
        m_iFftLength = CUtil::nextPowOf2(2 * iBlockLength);

    m_pCFft->init(m_iBlockLength, 2, CFft::kWindowHann, CFft::kNoWindow);

    for (auto j = 0; j < 2; j++)
        CVector::alloc(m_apfData[j], m_iFftLength);

    m_bIsInitialized = true;

    return Error_t::kNoError;
}

Error_t CCcf::reset()
{
    m_bWasProcessed = false;
    m_bIsInitialized = false;

    m_pCFft->reset();

    for (auto j = 0; j < 2; j++)
    {
        CVector::free(m_apfData[j]);
    }

    m_iFftLength = 0;
    m_iBlockLength = 0;

    return Error_t::kNoError;
}

Error_t CCcf::compCcf(const float *pfIn1, const float *pfIn2, bool bNormalize)
{
    if (!pfIn1 || !pfIn2)
        return Error_t::kFunctionInvalidArgsError;

    if (!m_bIsInitialized)
        return Error_t::kFunctionIllegalCallError;

    // extract standard deviation for normalization
    float afStd[2] = { 1.F, 1.F };
    if (bNormalize)
    {
        float fPower = std::sqrt(CVector::mulScalar(pfIn1, pfIn1, m_iBlockLength));
        afStd[0] = fPower > 0 ? fPower : 1.F;
        fPower = std::sqrt(CVector::mulScalar(pfIn2, pfIn2, m_iBlockLength));
        afStd[1] = fPower > 0 ? fPower : 1.F;
    }

    // compute the FFTs
    m_pCFft->compFft(m_apfData[0], pfIn1);
    m_pCFft->compFft(m_apfData[1], pfIn2);

    // conjugate complex multiply
    CVector::mulC_I(m_apfData[1], static_cast<float>(m_iFftLength), m_iFftLength);
    m_pCFft->conjugate_I(m_apfData[1]);
    m_pCFft->multiply_I(m_apfData[0], m_apfData[1]);

    // normalization
    CVector::mulC_I(m_apfData[0], 1.F / (afStd[0] * afStd[1]), m_iFftLength);

    // inverse Fft
    m_pCFft->compInvFft(m_apfData[0], m_apfData[0]);

    // copy results
    CVector::copy(m_apfData[1], &m_apfData[0][static_cast<long long>(m_iFftLength) - m_iBlockLength + 1], static_cast<long long>(m_iBlockLength) - 1);
    CVector::copy(&m_apfData[1][m_iBlockLength - 1], m_apfData[0], m_iBlockLength);

    m_bWasProcessed = true;
    return Error_t::kNoError;
}

int CCcf::getCcfLength(bool bIsAcf)
{
    if (!m_bIsInitialized)
        return -1;

    return bIsAcf ? m_iBlockLength : 2 * m_iBlockLength - 1;
}

Error_t CCcf::getCcf(float *pfCcfResult, bool bIsAcf) const
{
    if (!(m_bIsInitialized && m_bWasProcessed))
        return Error_t::kFunctionIllegalCallError;
    if (!pfCcfResult)
        return Error_t::kFunctionInvalidArgsError;

    int iStartIdx = bIsAcf ? m_iBlockLength - 1 : 0;
    CVector::copy(pfCcfResult, &m_apfData[1][iStartIdx], static_cast<long long>(2) * m_iBlockLength - 1 - iStartIdx);

    return Error_t::kNoError;
}

float CCcf::getCcfMax(bool bIsAcf) const
{
    if (!(m_bIsInitialized && m_bWasProcessed))
        return -1.F;

    int iStartIdx = bIsAcf ? m_iBlockLength - 1 : 0;
    return CVector::getMax(&m_apfData[1][iStartIdx], static_cast<long long>(2) * m_iBlockLength - 1 - iStartIdx);
}

int CCcf::getCcfMaxIdx(bool bIsAcf) const
{
    if (!(m_bIsInitialized && m_bWasProcessed))
        return -1;
    float fMax = -1.F;
    long long iMax = -1;
    int iStartIdx = bIsAcf ? m_iBlockLength - 1 : 0;

    CVector::findMax(&m_apfData[1][iStartIdx], fMax, iMax, static_cast<long long>(2) * m_iBlockLength - 1 - iStartIdx);

    return static_cast<int>(iMax);
}
