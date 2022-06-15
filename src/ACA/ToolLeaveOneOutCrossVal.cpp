#include "Vector.h"
#include "Matrix.h"

#include "ClassifierBase.h"

#include "ToolLeaveOneOutCrossVal.h"

CLeaveOneOutCrossVal::~CLeaveOneOutCrossVal(void) { reset(); }

/*! initializes LeaveOneOutCrossVal instance
\param iNumFeatures number of features (rows in the input matrix)
\param iNumObservations number of observations (columns in the feature matrix)
\param pCClassifier classifier to be used internally
\return Error_t
*/

Error_t CLeaveOneOutCrossVal::init(int iNumFeatures, int iNumObservations, CClassifierBase* pCClassifier)
{
    if (iNumFeatures <= 0 || iNumObservations <= 1)
        return Error_t::kFunctionInvalidArgsError;
    if (!pCClassifier)
        return Error_t::kFunctionInvalidArgsError;

    reset();

    // set internal member variables
    m_iNumFeatures = iNumFeatures;
    m_iNumObs = iNumObservations;
    m_pCClassifier = pCClassifier;

    // allocate memory for feature and ground truth data, and query
    CMatrix::alloc(m_ppfTrain, m_iNumFeatures, m_iNumObs - static_cast<long long>(1));
    CVector::alloc(m_piClassLabels, m_iNumObs - static_cast<long long>(1));

    m_bIsInitialized = true;

    return Error_t::kNoError;
}

/*! initializes LeaveOneOutCrossVal instance
\param ppfTrainFeatures feature data for 'training' (dimensions iNumFeatures X iNumObservations)
\param piTrainClassIndices ground truth class index for each observation
\return Error_t
*/

float CLeaveOneOutCrossVal::process(float** ppfTrainFeatures, const int* piTrainClassIndices)
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
    CMatrix::getCol(m_pfQuery, ppfTrainFeatures, m_iNumObs - 1, m_iNumFeatures);
    CVector::copy(m_piClassLabels, piTrainClassIndices, m_iNumObs - static_cast<long long>(1));

    for (int n = 0; n < m_iNumObs; n++)
    {
        // train (check reset and memory alloc)
        m_pCClassifier->train(m_ppfTrain, m_piClassLabels);

        // classify
        if (iGtLabel == m_pCClassifier->classify(m_pfQuery))
            iTruePositives++;

        // prep for next round
        CMatrix::setCol(m_ppfTrain, m_pfQuery, m_iNumObs - 2 - n, m_iNumFeatures);
        CMatrix::getCol(m_pfQuery, ppfTrainFeatures, m_iNumObs - 2 - n, m_iNumFeatures);
        m_piClassLabels[m_iNumObs - 2 - n] = iGtLabel;
        iGtLabel = piTrainClassIndices[m_iNumObs - 2 - n];
    }

    return iTruePositives * 1.F / m_iNumObs;
}

/*! resets LeaveOneOutCrossVal instance
\return Error_t
*/

Error_t CLeaveOneOutCrossVal::reset()
{
    m_bIsInitialized = false;

    // allocate memory for feature and ground truth data, and query
    CMatrix::free(m_ppfTrain, m_iNumFeatures);
    CVector::free(m_piClassLabels);

    m_iNumFeatures = 0;
    m_iNumObs = 0;
    m_pCClassifier = 0;

    return Error_t::kNoError;
}
