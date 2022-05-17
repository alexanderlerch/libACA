
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
    delete m_pCFft;
}

Error_t CCcf::init(int iBlockSize)
{
    if (iBlockSize <= 0)
        return Error_t::kFunctionInvalidArgsError;

    if (m_bIsInitialized)
        reset();

    m_iBlockLength = iBlockSize;
    if (CUtil::isPowOf2(m_iBlockLength))
        m_iFftLength = 2 * m_iBlockLength;
    else
        m_iFftLength = CUtil::nextPowOf2(2*iBlockSize);

    m_pCFft->init(m_iFftLength / 2, 2, CFft::kWindowHann, CFft::kNoWindow);

    for (auto j = 0; j < 2; j++)
    {
        m_apfData[j] = new float[m_iFftLength];
        CVectorFloat::setZero(m_apfData[0], m_iFftLength);
    }

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
        delete [] m_apfData[j];
        m_apfData[j] = 0;
    }
    
    m_iFftLength = 0;
    m_iBlockLength = 0;

    return Error_t::kNoError;
}

Error_t CCcf::calcCcf(const float* pfInput1, const float* pfInput2, bool bNormalize)
{
    if (!pfInput1 || !pfInput2)
        return Error_t::kFunctionInvalidArgsError;   

    if (!m_bIsInitialized)
        return Error_t::kFunctionIllegalCallError;
    
    // extract standard deviation for normalization
    float afStd[2] = { 1.F, 1.F };
    if (bNormalize)
    {
        float fPower = std::sqrt(CVectorFloat::mulScalar(pfInput1, pfInput1, m_iBlockLength));
        afStd[0] = fPower > 0 ? fPower : 1.F;
        fPower = std::sqrt(CVectorFloat::mulScalar(pfInput2, pfInput2, m_iBlockLength));
        afStd[1] = fPower > 0 ? fPower : 1.F;
    }

    // compute the FFTs
    m_pCFft->doFft(m_apfData[0], pfInput1);
    m_pCFft->doFft(m_apfData[1], pfInput2);

    // conjugate complex multiply
    CVectorFloat::mulC_I(m_apfData[1], static_cast<float>(m_iFftLength), m_iFftLength);
    m_pCFft->conjugate_I(m_apfData[1]);
    m_pCFft->multiply_I(m_apfData[0], m_apfData[1]);

    // normalization
    CVectorFloat::mulC_I(m_apfData[0], 1.F / (afStd[0] * afStd[1]), m_iFftLength);

    // inverse Fft
    m_pCFft->doInvFft(m_apfData[0], m_apfData[0]);

    // copy results
    CVectorFloat::copy(m_apfData[1], &m_apfData[0][m_iFftLength - m_iBlockLength + 1], m_iBlockLength - 1);
    CVectorFloat::copy(&m_apfData[1][m_iBlockLength - 1], m_apfData[0], m_iBlockLength);

    m_bWasProcessed = true;
    return Error_t::kNoError;
}

int CCcf::getCcfLength(bool bIsAcf)
{
    if (!(m_bIsInitialized && m_bWasProcessed))
        return -1;

    return bIsAcf ? m_iBlockLength - 1 : 2 * m_iBlockLength -1;
}

Error_t CCcf::getCcf(float* pfCcfResult, bool bIsAcf) const
{
    if (!(m_bIsInitialized && m_bWasProcessed))
        return Error_t::kFunctionIllegalCallError;
    if (!pfCcfResult)
        return Error_t::kFunctionInvalidArgsError;

    int iStartIdx = bIsAcf ? m_iBlockLength - 1 : 0;
    CVectorFloat::copy(pfCcfResult, &m_apfData[1][iStartIdx], 2 * m_iBlockLength - 1 - iStartIdx);

    return Error_t::kNoError;
}

float CCcf::getCcfMax(bool bIsAcf) const
{
    if (!(m_bIsInitialized && m_bWasProcessed))
        return -1.F;

    int iStartIdx = bIsAcf ? m_iBlockLength - 1 : 0;
    return CVectorFloat::getMax(&m_apfData[1][iStartIdx], 2 * m_iBlockLength - 1 - iStartIdx);
}

int CCcf::getCcfMaxIdx(bool bIsAcf) const
{
    if (!(m_bIsInitialized && m_bWasProcessed))
        return -1.F;
    float fMax = -1.F;
    long long iMax = -1;
    int iStartIdx = bIsAcf ? m_iBlockLength - 1 : 0;
    
    CVectorFloat::findMax(&m_apfData[1][iStartIdx], fMax, iMax, 2 * m_iBlockLength - 1 - iStartIdx);

    return iMax;
}
