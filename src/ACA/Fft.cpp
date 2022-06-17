

#include <cmath>

#include "Util.h"
#include "Vector.h"
#include "Fft.h"

#include "rvfft.h"

const float CFft::m_Pi = static_cast<float>(M_PI);
const float CFft::m_Pi2 = static_cast<float>(M_PI_2);

CFft::CFft()
{
    reset();
}

inline CFft::~CFft()
{
    reset();
}

Error_t CFft::init(int iBlockLength, int iZeroPadFactor, WindowFunction_t eWindow /*= kWindowHann*/, Windowing_t eWindowing /*= kPreWindow*/)
{
    Error_t  rErr = Error_t::kNoError;

    // sanity check
    if (iZeroPadFactor <= 0)
        return Error_t::kFunctionInvalidArgsError;

    // clean up
    reset();

    // make sure the fft length is a power of two
    int iInternalBlockLength = CUtil::isPowOf2(iBlockLength) ? iBlockLength : CUtil::nextPowOf2(iBlockLength);
    m_iDataLength = iBlockLength;
    m_iFftLength = iInternalBlockLength * iZeroPadFactor;

    m_ePrePostWindowOpt = eWindowing;

    rErr = allocMemory_();
    if (rErr != Error_t::kNoError)
        return rErr;

    rErr = computeWindow_(eWindow);

    m_bIsInitialized = true;

    return rErr;
}

Error_t CFft::reset()
{
    freeMemory_();

    m_iDataLength = 0;
    m_iFftLength = 0;
    m_ePrePostWindowOpt = kNoWindow;

    m_bIsInitialized = false;

    return Error_t::kNoError;

}

Error_t CFft::overrideWindow(const float *pfNewWindow)
{
    if (!m_bIsInitialized)
        return Error_t::kNotInitializedError;
    if (!pfNewWindow)
        return Error_t::kFunctionInvalidArgsError;

    CVector::copy(m_pfWindowBuff, pfNewWindow, m_iDataLength);

    return Error_t::kNoError;
}

Error_t CFft::getWindow(float *pfWindow) const
{
    if (!m_bIsInitialized)
        return Error_t::kNotInitializedError;
    if (!pfWindow)
        return Error_t::kFunctionInvalidArgsError;

    CVector::copy(pfWindow, m_pfWindowBuff, m_iDataLength);

    return Error_t::kNoError;
}

Error_t CFft::compFft(complex_t *pfSpectrum, const float *pfIn)
{
    if (!m_bIsInitialized)
        return Error_t::kNotInitializedError;
    if (!pfIn || !pfSpectrum)
        return Error_t::kFunctionInvalidArgsError;

    // copy data to internal buffer
    CVector::copy(m_pfProcBuff, pfIn, m_iDataLength);
    CVector::setZero(&m_pfProcBuff[m_iDataLength], static_cast<long long>(m_iFftLength) - m_iDataLength);

    // apply window function
    if (m_ePrePostWindowOpt & kPreWindow)
        CVector::mul_I(m_pfProcBuff, m_pfWindowBuff, m_iDataLength);

    // compute fft
    LaszloFft::realfft_split(m_pfProcBuff, m_iFftLength);

    // copy data to output buffer
    CVector::copy(pfSpectrum, m_pfProcBuff, m_iFftLength);

    return Error_t::kNoError;
}

Error_t CFft::compInvFft(float *pfOut, const complex_t *pfSpectrum)
{
    if (!m_bIsInitialized)
        return Error_t::kNotInitializedError;

    // copy data to internal buffer
    CVector::copy(m_pfProcBuff, pfSpectrum, m_iFftLength);

    // compute ifft
    LaszloFft::irealfft_split(m_pfProcBuff, m_iFftLength);

    // apply window function
    if (m_ePrePostWindowOpt & kPostWindow)
        CVector::mul_I(m_pfProcBuff, m_pfWindowBuff, m_iDataLength);

    // copy data to output buffer
    CVector::copy(pfOut, m_pfProcBuff, m_iFftLength);

    return Error_t::kNoError;
}

Error_t CFft::getMagnitude(float *pfMag, const complex_t *pfSpectrum) const
{
    if (!m_bIsInitialized)
        return Error_t::kNotInitializedError;

    // re(0),re(1),re(2),...,re(size/2),im(size/2-1),...,im(1)
    int iNyq = m_iFftLength >> 1;

    // no imaginary part at these bins
    pfMag[0] = std::abs(pfSpectrum[0]);
    pfMag[iNyq] = std::abs(pfSpectrum[iNyq]);

    for (int i = 1; i < iNyq; i++)
    {
        int iImagIdx = m_iFftLength - i;
        pfMag[i] = sqrtf(pfSpectrum[i] * pfSpectrum[i] + pfSpectrum[iImagIdx] * pfSpectrum[iImagIdx]);
    }
    return Error_t::kNoError;
}

Error_t CFft::getPhase(float *pfPhase, const complex_t *pfSpectrum) const
{
    if (!m_bIsInitialized)
        return Error_t::kNotInitializedError;

    // re(0),re(1),re(2),...,re(size/2),im(size/2-1),...,im(1)
    int iNyq = m_iFftLength >> 1;

    pfPhase[0] = m_Pi;
    pfPhase[iNyq] = m_Pi;

    for (int i = 1; i < iNyq; i++)
    {
        int iImagIdx = m_iFftLength - i;
        if (pfSpectrum[i] == .0F && pfSpectrum[iImagIdx] != .0F)
            pfPhase[i] = m_Pi2;
        else
            pfPhase[i] = atan2f(pfSpectrum[iImagIdx], pfSpectrum[i]);
    }
    return Error_t::kNoError;
}

Error_t CFft::splitRealImag(float *pfReal, float *pfImag, const complex_t *pfSpectrum) const
{
    if (!m_bIsInitialized)
        return Error_t::kNotInitializedError;

    // re(0),re(1),re(2),...,re(size/2),im(size/2-1),...,im(1)
    int iNyq = m_iFftLength >> 1;

    CVector::copy(pfReal, pfSpectrum, static_cast<long long>(iNyq) + 1);

    pfImag[0] = 0;
    pfImag[iNyq] = 0;
    for (int i = 1, iImag = m_iFftLength - 1; i < iNyq; i++, iImag--)
    {
        pfImag[i] = pfSpectrum[iImag];
    }

    return Error_t::kNoError;
}

Error_t CFft::mergeRealImag(complex_t *pfSpectrum, const float *pfReal, const float *pfImag) const
{
    if (!m_bIsInitialized)
        return Error_t::kNotInitializedError;

    // re(0),re(1),re(2),...,re(size/2),im(size/2-1),...,im(1)
    int iNyq = m_iFftLength >> 1;

    CVector::copy(pfSpectrum, pfReal, static_cast<long long>(iNyq) + 1);

    for (int i = 1, iImag = m_iFftLength - 1; i < iNyq; i++, iImag--)
    {
        pfSpectrum[iImag] = pfImag[i];
    }

    return Error_t::kNoError;
}

float CFft::freq2bin(float fFreqInHz, float fSampleRateInHz) const
{
    return fFreqInHz / fSampleRateInHz * m_iFftLength;
}

float CFft::bin2freq(int iBinIdx, float fSampleRateInHz) const
{
    return iBinIdx * fSampleRateInHz / m_iFftLength;
}

void CFft::conjugate_I(complex_t *pfFftResult) const
{
    // re(0),re(1),re(2),...,re(size/2),im(size/2-1),...,im(1)
    CVector::mulC_I(&pfFftResult[(m_iFftLength >> 1) + 1], -1.F, static_cast<long long>(m_iFftLength >> 1) - 1);
}

void CFft::multiply_I(complex_t *pfFftSrc1Dest, const complex_t *pfFftSrc2) const
{
    int i = 0,
        j = 0;

    // re(0),re(1),re(2),...,re(size/2),im(size/2-1),...,im(1)
    pfFftSrc1Dest[0] *= pfFftSrc2[0];
    pfFftSrc1Dest[m_iFftLength >> 1] *= pfFftSrc2[m_iFftLength >> 1];

    for (i = 1, j = m_iFftLength - 1; i < m_iFftLength >> 1; i++, j--)
    {
        float fTemp = pfFftSrc1Dest[i];

        // real part of multiplication
        pfFftSrc1Dest[i] *= pfFftSrc2[i];
        pfFftSrc1Dest[i] -= pfFftSrc1Dest[j] * pfFftSrc2[j];

        // imaginary part of multiplication
        pfFftSrc1Dest[j] *= pfFftSrc2[i];
        pfFftSrc1Dest[j] += fTemp * pfFftSrc2[j];
    }

    return;
}

Error_t CFft::allocMemory_()
{
    CVector::alloc(m_pfProcBuff, m_iFftLength);
    CVector::alloc(m_pfWindowBuff, m_iDataLength);

    if (!m_pfProcBuff || !m_pfWindowBuff)
        return Error_t::kMemError;
    else
    {
        return Error_t::kNoError;
    }
}

Error_t CFft::freeMemory_()
{
    CVector::free(m_pfProcBuff);
    CVector::free(m_pfWindowBuff);

    m_pfProcBuff = 0;
    m_pfWindowBuff = 0;

    return Error_t::kNoError;
}

Error_t CFft::computeWindow_(WindowFunction_t eWindow)
{
    int i;

    // note that these windows are periodic, not symmetric
    switch (eWindow)
    {
    case kWindowSine:
    {
        for (i = 0; i < m_iDataLength; i++)
        {
            m_pfWindowBuff[i] = sinf((i * m_Pi) / (m_iDataLength + 1));
        }
        break;
    }
    case kWindowHann:
    {
        for (i = 0; i < m_iDataLength; i++)
        {
            m_pfWindowBuff[i] = .5F * (1.F - cosf((i * 2.F * m_Pi) / (m_iDataLength + 1)));
        }
        break;
    }
    case kWindowHamming:
    {
        for (i = 0; i < m_iDataLength; i++)
        {
            m_pfWindowBuff[i] = .54F - .46F * cosf((i * 2.F * m_Pi) / (m_iDataLength + 1));
        }
        break;
    }
    default:
    case kNumWindows:
    {
        return Error_t::kFunctionInvalidArgsError;
    }
    }

    return Error_t::kNoError;
}

int CFft::getLength(Length_t eLengthIdx) const
{
    switch (eLengthIdx)
    {
    case kLengthFft:
        return m_iFftLength;
    case kLengthData:
        return m_iDataLength;
    case kLengthMagnitude:
    case kLengthPhase:
        return m_iFftLength / 2 + 1;
    default:
    case kNumLengths:
        return -1;
    }
}
