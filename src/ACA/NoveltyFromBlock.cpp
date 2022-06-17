
#include "Vector.h"
#include "Util.h"

#include "NoveltyFromBlock.h"


////////////////////////////////////////////////////////////////////////
// static member functions

float CNoveltyFromBlockIf::compNoveltyFlux(const float *pfMagSpec, const float *pfPrevSpec, int iDataLength, float /*fSampleRate = 1.F*/)
{
    assert(pfMagSpec);
    assert(pfPrevSpec);
    assert(iDataLength > 0);

    float fSum = 0;
    for (auto k = 0; k < iDataLength; k++)
    {
        // spectral flux with HWR
        float fDiff = pfMagSpec[k] - pfPrevSpec[k];
        fSum += (fDiff > 0) ? fDiff * fDiff : 0.F;
    }

    return std::sqrt(fSum) / iDataLength;
}

float CNoveltyFromBlockIf::compNoveltyHainsworth(const float *pfMagSpec, const float *pfPrevSpec, int iDataLength, float /*fSampleRate = 1.F*/)
{
    assert(pfMagSpec);
    assert(pfPrevSpec);
    assert(iDataLength > 0);

    const float fEpsilon = 1e-5F;
    float fSum = 0;

    for (auto k = 0; k < iDataLength; k++)
    {
        float fMag = (pfMagSpec[k] > 0) ? pfMagSpec[k] : fEpsilon;
        float fPrev = (pfPrevSpec[k] > 0) ? pfPrevSpec[k] : fEpsilon;
        fSum += std::log2(fMag / fPrev);
    }

    return fSum / iDataLength;
}

float CNoveltyFromBlockIf::compNoveltyLaroche(const float *pfMagSpec, const float *pfPrevSpec, int iDataLength, float /*fSampleRate = 1.F*/)
{
    assert(pfMagSpec);
    assert(pfPrevSpec);
    assert(iDataLength > 0);

    float fSum = 0;

    for (auto k = 0; k < iDataLength; k++)
    {
        float fDiff = std::sqrt(pfMagSpec[k]) - std::sqrt(pfPrevSpec[k]);
        fSum += (fDiff > 0) ? fDiff : 0.F;
    }

    return fSum / iDataLength;
}


///////////////////////////////////////////////////////////////////
// normal member functions

CNoveltyFromBlockIf::CNoveltyFromBlockIf(CNoveltyIf::Novelty_t eNoveltyIdx, int iDataLength, float fSampleRate) :
    m_eNoveltyIdx(eNoveltyIdx),
    m_iDataLength(iDataLength),
    m_fSampleRate(fSampleRate)
{
    assert(iDataLength > 0);
    assert(fSampleRate > 0);

    CVector::alloc(m_pfPrevSpec, m_iDataLength);
}

CNoveltyFromBlockIf::~CNoveltyFromBlockIf()
{
    CVector::free(m_pfPrevSpec);
}

Error_t CNoveltyFromBlockIf::create(CNoveltyFromBlockIf *&pCInstance, CNoveltyIf::Novelty_t eNoveltyIdx, int iDataLength, float fSampleRate)
{
    if (iDataLength <= 0 || fSampleRate <= 0)
        return Error_t::kFunctionInvalidArgsError;

    pCInstance = new CNoveltyFromBlockIf(eNoveltyIdx, iDataLength, fSampleRate);

    return Error_t::kNoError;
}

Error_t CNoveltyFromBlockIf::destroy(CNoveltyFromBlockIf *&pCInstance)
{
    delete pCInstance;

    pCInstance = 0;

    return Error_t::kNoError;
}

int CNoveltyFromBlockIf::getNoveltyDimension() const
{
    // default: 1 value per block
    return 1;
}

Error_t CNoveltyFromBlockIf::compNovelty(float *pfNovelty, const float *pfIn)
{
    *pfNovelty = m_DispatchMap.at(m_eNoveltyIdx)(pfIn, m_pfPrevSpec, m_iDataLength, m_fSampleRate);

    CVector::copy(m_pfPrevSpec, pfIn, m_iDataLength);

    return Error_t::kNoError;
}
