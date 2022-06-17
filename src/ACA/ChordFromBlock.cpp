
#include "Vector.h"
#include "Matrix.h"

#include "FeatureFromBlock.h"

#include "Chord.h"
#include "ChordFromBlock.h"

Error_t CChordFromBlockIf::create(CChordFromBlockIf *&pCInstance, int iMagSpecLength, float fSampleRate)
{
    if (iMagSpecLength <= 0 || fSampleRate <= 0)
        return Error_t::kFunctionInvalidArgsError;


    pCInstance = new CChordFromBlockIf(iMagSpecLength, fSampleRate);

    return Error_t::kNoError;
}

Error_t CChordFromBlockIf::destroy(CChordFromBlockIf *&pCInstance)
{
    delete pCInstance;

    pCInstance = 0;

    return Error_t::kNoError;
}

Error_t CChordFromBlockIf::compChordProb(float *pfChordProb, const float *pfIn)
{
    // compute pitch chroma
    m_pCFeatureExtractor->compFeature(m_pfPitchChroma, pfIn);

    // check if zero
    if (CVector::getSum(m_pfPitchChroma, kNumPitchClasses) <= 1e-20F)
    {
        CVector::setZero(pfChordProb, CChordIf::kNumChords);
        pfChordProb[CChordIf::kNoChord] = 1.F;
        return Error_t::kNoError;
    }

    // get chord probs with template
    CMatrix::mulMatColvec(pfChordProb, m_ppfTemplateMatrix, m_pfPitchChroma, CChordIf::kNumChords, kNumPitchClasses);

    assert(CVector::getSum(pfChordProb, CChordIf::kNumChords) > 0);

    // normalize to probability of 1
    CVector::mulC_I(pfChordProb, 1.F / CVector::getSum(pfChordProb, CChordIf::kNumChords), CChordIf::kNumChords);

    return Error_t::kNoError;
}

inline CChordFromBlockIf::CChordFromBlockIf(int iMagSpecLength, float fSampleRate) : m_iMagSpecLength(iMagSpecLength), m_fSampleRate(fSampleRate)
{
    CVector::alloc(m_pfPitchChroma, kNumPitchClasses);
    CMatrix::alloc(m_ppfTemplateMatrix, CChordIf::kNumChords, kNumPitchClasses);

    CFeatureFromBlockIf::create(m_pCFeatureExtractor, CFeatureIf::kFeatureSpectralPitchChroma, m_iMagSpecLength, m_fSampleRate);

    genTemplateMatrix_();
}

inline CChordFromBlockIf::~CChordFromBlockIf()
{
    CVector::free(m_pfPitchChroma);
    CMatrix::free(m_ppfTemplateMatrix, CChordIf::kNumChords);

    CFeatureFromBlockIf::destroy(m_pCFeatureExtractor);
}

void CChordFromBlockIf::genTemplateMatrix_()
{
    assert(m_ppfTemplateMatrix);
    assert(m_ppfTemplateMatrix[0]);

    const int iNumChordPitches = 3;
    const int aiMajorIndices[3] = { 0,4,7 };
    const int aiMinorIndices[3] = { 0,3,7 };

    // simple template with equally weighted chord pitches, zero otherwise
    for (auto c = 0; c < kNumPitchClasses; c++)
    {
        for (auto p = 0; p < iNumChordPitches; p++)
        {
            m_ppfTemplateMatrix[c][(c + aiMajorIndices[p]) % 12] = 1.F / iNumChordPitches;
            m_ppfTemplateMatrix[c + kNumPitchClasses][(c + aiMinorIndices[p]) % 12] = 1.F / iNumChordPitches;
        }
    }
    CVector::addC_I(m_ppfTemplateMatrix[CChordIf::kNoChord], 1.F / kNumPitchClasses, kNumPitchClasses);
}
