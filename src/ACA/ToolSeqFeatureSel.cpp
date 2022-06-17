#include "Vector.h"
#include "Matrix.h"

#include "ToolLeaveOneOutCrossVal.h"
#include "ToolSimpleKnn.h"

#include "ToolSeqFeatureSel.h"

CSeqFeatureSel::~CSeqFeatureSel(void) { reset(); }

Error_t CSeqFeatureSel::init(int iNumFeatures, int iNumObs)
{
    if (iNumFeatures <= 0 || iNumObs <= 1)
        return Error_t::kFunctionInvalidArgsError;

    reset();

    m_iNumFeatures = iNumFeatures;
    m_iNumObs = iNumObs;

    m_pCClassifier = new CKnn();
    m_pCClassifier->setParamK(1);
    m_pCCv = new CLeaveOneOutCrossVal();

    CVector::alloc(m_piSelFeatures, m_iNumFeatures);
    CVector::alloc(m_pfAccuracy, m_iNumFeatures);

    CMatrix::alloc(m_ppfTrain, m_iNumFeatures, m_iNumObs);

    m_bIsInitialized = true;

    return Error_t::kNoError;
}

Error_t CSeqFeatureSel::process(const float *const *const ppfFeatures, const int *piClassIndices)
{
    if (!m_bIsInitialized)
        return Error_t::kFunctionIllegalCallError;
    if (!ppfFeatures || !piClassIndices)
        return Error_t::kFunctionInvalidArgsError;
    if (!ppfFeatures[0])
        return Error_t::kFunctionInvalidArgsError;

    int iNumFeatures2Select = m_iNumFeatures;
    int iNumSelFeatures = 0;

    CVector::setValue(m_piSelFeatures, -1, m_iNumFeatures);

    // find single best feature
    for (auto v = 0; v < m_iNumFeatures; v++)
    {
        float fAcc = 0;

        m_pCCv->init(1, m_iNumObs, m_pCClassifier);

        fAcc = m_pCCv->process(&ppfFeatures[v], piClassIndices);

        if (fAcc >= m_pfAccuracy[0])
        {
            m_pfAccuracy[0] = fAcc;
            m_piSelFeatures[0] = v;
        }
    }

    // copy best feature value to the first row of our feature matrix
    CVector::copy(m_ppfTrain[0], ppfFeatures[m_piSelFeatures[0]], m_iNumObs);
    iNumSelFeatures++;

    // iterate until target number of features is reached
    for (auto f = iNumSelFeatures; f < iNumFeatures2Select; f++)
    {
        // iterate over all features not yet selected
        for (auto v = 0; v < m_iNumFeatures; v++)
        {
            float fAcc = 0;

            if (isFeatureAlreadySelected_(v))
                continue;


            // add current feature v to internal feature matrix
            CVector::copy(m_ppfTrain[iNumSelFeatures], ppfFeatures[v], m_iNumObs);

            m_pCCv->init(iNumSelFeatures + 1, m_iNumObs, m_pCClassifier);

            //accuracy of selected features plus current feature f
            fAcc = m_pCCv->process(m_ppfTrain, piClassIndices);

            if (fAcc >= m_pfAccuracy[iNumSelFeatures])
            {
                m_pfAccuracy[iNumSelFeatures] = fAcc;
                m_piSelFeatures[iNumSelFeatures] = v;
            }
        }
        assert(m_piSelFeatures[iNumSelFeatures] >= 0 && m_piSelFeatures[iNumSelFeatures] < m_iNumFeatures);
        CVector::copy(m_ppfTrain[iNumSelFeatures], ppfFeatures[m_piSelFeatures[iNumSelFeatures]], m_iNumObs);
        iNumSelFeatures++;
    }

    m_bWasProcessed = true;

    return Error_t::kNoError;
}

Error_t CSeqFeatureSel::getResult(int *piFeatureIndices, float *pfAccuracyPerStep /*= 0*/)
{
    if (!m_bWasProcessed)
        return Error_t::kFunctionIllegalCallError;
    if (!piFeatureIndices)
        return Error_t::kFunctionInvalidArgsError;

    CVector::copy(piFeatureIndices, m_piSelFeatures, m_iNumFeatures);
    if (pfAccuracyPerStep)
        CVector::copy(pfAccuracyPerStep, m_pfAccuracy, m_iNumFeatures);

    return Error_t::kNoError;
}

Error_t CSeqFeatureSel::reset()
{
    m_bIsInitialized = false;
    m_bWasProcessed = false;

    CVector::free(m_piSelFeatures);
    CVector::free(m_pfAccuracy);
    CMatrix::free(m_ppfTrain, m_iNumFeatures);

    delete m_pCClassifier;
    m_pCClassifier = 0;

    delete m_pCCv;
    m_pCCv = 0;

    m_iNumFeatures = 0;
    m_iNumObs = 0;

    return Error_t::kNoError;
}

