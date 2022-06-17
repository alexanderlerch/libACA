#include "Vector.h"
#include "Matrix.h"

#include "ClassifierBase.h"

#include "ToolLeaveOneOutCrossVal.h"

CLeaveOneOutCrossVal::~CLeaveOneOutCrossVal(void) { reset(); }

Error_t CLeaveOneOutCrossVal::init(int iNumFeatures, int iNumObs, CClassifierBase *pCClassifier)
{
    if (iNumFeatures <= 0 || iNumObs <= 1)
        return Error_t::kFunctionInvalidArgsError;
    if (!pCClassifier)
        return Error_t::kFunctionInvalidArgsError;

    if (m_iNumFeatures == iNumFeatures && m_iNumObs == iNumObs && m_pCClassifier == pCClassifier)
    {
        m_pCClassifier->reset();
        CMatrix::setZero(m_ppfTrain, m_iNumFeatures, m_iNumObs - static_cast<long long>(1));
        CVector::setZero(m_piClassLabels, m_iNumObs - static_cast<long long>(1));
        CVector::setZero(m_pfQuery, m_iNumFeatures);
    }
    else
    {
        reset();


        // set internal member variables
        m_iNumFeatures = iNumFeatures;
        m_iNumObs = iNumObs;
        m_pCClassifier = pCClassifier;

        // allocate memory for feature and ground truth data, and query
        CMatrix::alloc(m_ppfTrain, m_iNumFeatures, m_iNumObs - static_cast<long long>(1));
        CVector::alloc(m_piClassLabels, m_iNumObs - static_cast<long long>(1));
        CVector::alloc(m_pfQuery, m_iNumFeatures);

        m_bIsInitialized = true;
    }

    return Error_t::kNoError;
}

Error_t CLeaveOneOutCrossVal::reset()
{
    m_bIsInitialized = false;

    // allocate memory for feature and ground truth data, and query
    CMatrix::free(m_ppfTrain, m_iNumFeatures);
    CVector::free(m_piClassLabels);
    CVector::free(m_pfQuery);

    m_iNumFeatures = 0;
    m_iNumObs = 0;
    m_pCClassifier = 0;

    return Error_t::kNoError;
}

float CLeaveOneOutCrossVal::process(const float *const *const ppfTrainFeatures, const int *piTrainClassIndices)
{
    if (!m_bIsInitialized)
        return -1.F;
    if (!ppfTrainFeatures || !piTrainClassIndices)
        return -1.F;
    if (!ppfTrainFeatures[0])
        return -1.F;

    assert(m_iNumObs > 1);
    assert(m_iNumFeatures > 0);

    int iGtLabel = piTrainClassIndices[m_iNumObs - 1];
    int iTruePositives = 0;

    // initialize classifier
    m_pCClassifier->init(m_iNumFeatures, m_iNumObs - 1);

    // copy feature data and ground truth data
    CMatrix::copy(m_ppfTrain, ppfTrainFeatures, m_iNumFeatures, m_iNumObs - static_cast<long long>(1));
    CVector::copy(m_piClassLabels, piTrainClassIndices, m_iNumObs - static_cast<long long>(1));

    for (int n = 0; n < m_iNumObs - 1; n++)
    {
        CMatrix::getCol(m_pfQuery, ppfTrainFeatures, m_iNumObs - 1 - n, m_iNumFeatures);
        iGtLabel = piTrainClassIndices[m_iNumObs - 1 - n];

        // train (check reset and memory alloc)
        m_pCClassifier->train(m_ppfTrain, m_piClassLabels);

        // classify
        if (iGtLabel == m_pCClassifier->classify(m_pfQuery))
            iTruePositives++;

        // prep for next round
        CMatrix::setCol(m_ppfTrain, m_pfQuery, m_iNumObs - 2 - n, m_iNumFeatures);
        m_piClassLabels[m_iNumObs - 2 - n] = iGtLabel;
    }

    // last observation (avoid if in loop)
    CMatrix::getCol(m_pfQuery, ppfTrainFeatures, 0, m_iNumFeatures);
    iGtLabel = piTrainClassIndices[0];

    // train (check reset and memory alloc)
    m_pCClassifier->train(m_ppfTrain, m_piClassLabels);

    // classify
    if (iGtLabel == m_pCClassifier->classify(m_pfQuery))
        iTruePositives++;

    return iTruePositives * 1.F / m_iNumObs;
}

