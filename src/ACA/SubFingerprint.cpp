
#include <cmath>

#include "Vector.h"
#include "Matrix.h"

#include "ToolConversion.h"

#include "SubFingerprint.h"

CSubFingerprint::CSubFingerprint(int iMagSpecLength, float fSampleRate) :
    m_iMagSpecLength(iMagSpecLength),
    m_fSampleRate(fSampleRate)
{
    assert(iMagSpecLength > 0);
    assert(fSampleRate > 4000);

    // alloc transfer function memory
    CMatrix::alloc(m_ppfH, m_iNumBands + 1, m_iMagSpecLength);

    for (auto k = 0; k < kNumProcBuffs; k++)
        CVector::alloc(m_apfProcBuff[k], m_iNumBands + static_cast<long long>(1));

    genBands_();
}

CSubFingerprint::~CSubFingerprint()
{
    for (auto k = 0; k < kNumProcBuffs; k++)
        CVector::free(m_apfProcBuff[k]);

    CMatrix::free(m_ppfH, m_iNumBands + 1);
}

uint32_t CSubFingerprint::compSubFingerprint(const float *pfMagSpec)
{
    assert(pfMagSpec);

    CVector::setZero(m_apfProcBuff[kCurr], m_iNumBands + static_cast<long long>(1));

    for (auto p = 0; p < m_iNumBands + 1; p++)
    {
        // we could do this nicer with CVector::mulScalar if we allocated memory; or, we could use the bin boundaries for start and end of loop. or both.
        for (auto k = 0; k < m_iMagSpecLength; k++)
            m_apfProcBuff[kCurr][p] += m_ppfH[p][k] * (pfMagSpec[k] * pfMagSpec[k]);
    }

    // compute diff in freq domain
    for (auto p = 0; p < m_iNumBands; p++)
    {
        m_apfProcBuff[kCurr][p] = m_apfProcBuff[kCurr][p + 1] - m_apfProcBuff[kCurr][p];
        m_apfProcBuff[kTmp][p] = m_apfProcBuff[kCurr][p] - m_apfProcBuff[kPrev][p];
    }

    // update for next block
    CUtil::swap(m_apfProcBuff[kCurr], m_apfProcBuff[kPrev]);

    return CConversion::convertFloat2Word(m_apfProcBuff[kTmp]);
}

Error_t CSubFingerprint::reset()
{
    if (!m_apfProcBuff[kCurr] || !m_apfProcBuff[kTmp] || !m_apfProcBuff[kPrev])
        return Error_t::kMemError;

    for (auto k = 0; k < kNumProcBuffs; k++)
        CVector::setZero(m_apfProcBuff[k], m_iNumBands + static_cast<long long>(1));

    return Error_t::kNoError;
}

void CSubFingerprint::genBands_()
{
    enum FreqBounds_t
    {
        kLow,
        kHigh,

        kNumBounds
    };

    const float afFreqBounds[kNumBounds] = { 300.F, 2000.F };
    int iNumBands = m_iNumBands + 1;  //!< extract 33 bands
    int iFftLength = 2 * (m_iMagSpecLength - 1);

    float fFreqScale = std::log(afFreqBounds[kHigh] / afFreqBounds[kLow]);
    float fLowBound = afFreqBounds[kLow];

    CMatrix::setZero(m_ppfH, m_iNumBands + 1, m_iMagSpecLength);

    for (auto p = 0; p < m_iNumBands; p++)
    {
        float fHighBound = afFreqBounds[kLow] * std::exp((p + 1) * fFreqScale / iNumBands);
        // get indices from freqs
        const int aiBoundIdx[kNumBounds] = { static_cast<int>(CConversion::convertFreq2Bin(fLowBound, iFftLength, m_fSampleRate)) + 1,
            static_cast<int>(CConversion::convertFreq2Bin(fHighBound, iFftLength, m_fSampleRate)) };

        CVector::setValue(&m_ppfH[p][aiBoundIdx[kLow]], 1.F, aiBoundIdx[kHigh] + static_cast<long long>(1) - aiBoundIdx[kLow]);

        // proceed to next band
        fLowBound = fHighBound;
    }
}
