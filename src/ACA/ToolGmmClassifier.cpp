#include "Vector.h"
#include "Matrix.h"
#include "ToolGmm.h"

#include "ToolGmmClassifier.h"

CGmmClassifier::~CGmmClassifier(void) { reset(); }

Error_t CGmmClassifier::init(int iNumFeatures, int iNumObs)
{
    if (iNumFeatures <= 0 || iNumObs < 1)
        return Error_t::kFunctionInvalidArgsError;

    reset();

    m_iNumFeatures = iNumFeatures;
    m_iNumObs = iNumObs;

    // worst case: number of classes equals number of observations
    CVector::alloc(m_piClassLabels, m_iNumObs);

    CVector::alloc(m_pfNormScale, m_iNumFeatures);
    CVector::alloc(m_pfNormSub, m_iNumFeatures);

    CVector::alloc(m_pfQuery, m_iNumFeatures);

    m_bIsInitialized = true;

    return Error_t::kNoError;
}

Error_t CGmmClassifier::train(const float *const *const ppfTrainFeatures, const int *piTrainClassIndices, CClassifierBase::Normalization_t eNorm)
{
    if (!m_bIsInitialized)
        return Error_t::kFunctionIllegalCallError;

    if (!ppfTrainFeatures || !piTrainClassIndices)
        return Error_t::kFunctionInvalidArgsError;

    if (!ppfTrainFeatures[0])
        return Error_t::kFunctionInvalidArgsError;

    float **ppfTrain = 0;
    CGmm Gmm;

    // alloc temp train data matrix
    CMatrix::alloc(ppfTrain, m_iNumFeatures, m_iNumObs);

    // get number of classes
    m_iNumClasses = countClasses(piTrainClassIndices);

    // allocate GMM stuff but keep result for later (avoid mem leaks when repeatedly calling train)
    if (m_ppCGmmResult)
    {
        for (auto c = 0; c < m_iNumClasses; c++)
            delete m_ppCGmmResult[c];
        CVector::free(m_ppCGmmResult);
    }
    CVector::alloc(m_ppCGmmResult, m_iNumClasses);


    // normalize features and train GMM per class
    compNormConstants(ppfTrainFeatures, m_iNumFeatures, m_iNumObs, eNorm);
    for (auto c = 0; c < m_iNumClasses; c++)
    {
        Gmm.reset();
        int iNumClassObs = 0;
        for (auto n = 0; n < m_iNumObs; n++)
        {
            // add class-specific observations to the train matrix
            if (piTrainClassIndices[n] == m_piClassLabels[c])
            {
                CMatrix::getCol(m_pfQuery, ppfTrainFeatures, n, m_iNumFeatures);
                normalizeVector(m_pfQuery, m_iNumFeatures);
                CMatrix::setCol(ppfTrain, m_pfQuery, iNumClassObs, m_iNumFeatures);
                iNumClassObs++;
            }
        }

        // train
        m_ppCGmmResult[c] = new CGmmResult();
        Gmm.init(m_ppCGmmResult[c], m_iK, m_iNumFeatures, iNumClassObs);
        Gmm.compGmm(m_ppCGmmResult[c], ppfTrain);
    }

    // clean up
    CMatrix::free(ppfTrain, m_iNumFeatures);

    return Error_t::kNoError;
}

Error_t CGmmClassifier::reset()
{
    m_bIsInitialized = false;

    CVector::free(m_piClassLabels);

    CVector::free(m_pfQuery);

    for (auto c = 0; c < m_iNumClasses; c++)
        delete m_ppCGmmResult[c];
    CVector::free(m_ppCGmmResult);

    CVector::free(m_pfNormScale);
    CVector::free(m_pfNormSub);

    m_iNumClasses = 0;
    m_iNumFeatures = 0;
    m_iNumObs = 0;

    return Error_t::kNoError;
}

Error_t CGmmClassifier::setNumMixtures(int iK)
{
    if (iK < 1)
        return Error_t::kFunctionInvalidArgsError;

    if (m_iK != iK)
    {
        reset();
        m_iK = iK;
    }

    return Error_t::kNoError;
}


int CGmmClassifier::getNumMixtures() const
{
    return m_iK;
}


int CGmmClassifier::classify(const float *pfQuery)
{
    if (!pfQuery || !m_bIsInitialized)
        return kIllegalClassLabel;

    // normalize
    CVector::copy(m_pfQuery, pfQuery, m_iNumFeatures);
    normalizeVector(m_pfQuery, m_iNumFeatures);

    // check which Gmm gives us the highest value
    float fMax = -1;
    int iIdx = kIllegalClassLabel;
    for (auto c = 0; c < m_iNumClasses; c++)
    {
        float fProb = m_ppCGmmResult[c]->getProb(pfQuery);

        if (fProb > fMax)
        {
            fMax = fProb;
            iIdx = c;
        }
    }

    if (iIdx == kIllegalClassLabel)
        return kIllegalClassLabel;
    else
        return iIdx;
}

int CGmmClassifier::countClasses(const int *piClassLabels)
{
    m_iNumClasses = 1;
    m_piClassLabels[0] = piClassLabels[0];

    for (auto n = 1; n < m_iNumObs; n++)
    {
        int i = 0;
        while (i < m_iNumClasses)
        {
            // if it's in list go to next labels
            if (piClassLabels[n] == m_piClassLabels[i])
                break;
            i++;
        }
        // if we haven't found it, add it to the class list
        if (i == m_iNumClasses)
        {
            m_piClassLabels[m_iNumClasses] = piClassLabels[n];
            m_iNumClasses++;
        }
    }

    return m_iNumClasses;
}
