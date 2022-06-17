
#include "ErrorDef.h"

#include "Vector.h"
#include "RingBuffer.h"

#include "Key.h"

#include "KeyFromChroma.h"

CKeyFromChroma::CKeyFromChroma(void)
{
    // norm template key profiles
    for (auto m = 0; m < kNumModes; m++)
    {
        float fSum = CVector::getSum(m_aafKeyProfiles[m], kNumPitchClasses);

        m_apCRingBuff[m] = new CRingBuffer<float>(kNumPitchClasses);

        for (auto p = 0; p < kNumPitchClasses; p++)
            m_apCRingBuff[m]->putPostInc(m_aafKeyProfiles[m][p] / fSum);
    }
}

CKeyFromChroma::~CKeyFromChroma(void)
{
    for (auto m = 0; m < kNumModes; m++)
        delete m_apCRingBuff[m];
}

int CKeyFromChroma::getKey(const float *pfQuery)
{
    long long iRes = -1;
    float afQueryN[kNumPitchClasses] = { 0 };
    float afDist[CKey::kNumKeys] = { 0 };
    float fNorm = CVector::getSum(pfQuery, kNumPitchClasses);

    if (fNorm <= 0)
        return CKey::kNoKey;

    // normalize input
    CVector::copy(afQueryN, pfQuery, kNumPitchClasses);
    CVector::mulC_I(afQueryN, 1.F / fNorm, kNumPitchClasses);

    // compute manhattan distances for modes (major and minor)
    for (auto m = 0; m < kNumModes; m++)
    {
        for (auto p = 0; p < kNumPitchClasses; p++)
        {
            float afShiftedChroma[kNumPitchClasses] = { 0 };
            m_apCRingBuff[m]->get(afShiftedChroma, kNumPitchClasses);

            afDist[m * kNumPitchClasses + p] = CVector::distManhattan(afQueryN, afShiftedChroma, kNumPitchClasses);

            m_apCRingBuff[m]->setReadIdx(m_apCRingBuff[m]->getReadIdx() - 1);
        }
    }
    // last distance
    CVector::addC_I(afQueryN, -1.F / kNumPitchClasses, kNumPitchClasses);
    afDist[CKey::kNoKey] = CVector::getSum(afQueryN, kNumPitchClasses, true);

    // get key index (reuse afQuery as dummy arg)
    CVector::findMin(afDist, afQueryN[0], iRes, CKey::kNumKeys);

    return static_cast<int>(iRes);
}
