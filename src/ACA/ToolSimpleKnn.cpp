
#include "Matrix.h"
#include "Vector.h"

#include "ToolSimpleKnn.h"

const int CClassifierBase::kIllegalClassLabel = -1111111111;


CKnn::~CKnn(void) 
{
    this->reset();
}

Error_t CKnn::init(float** ppfTrainFeatures, const int* piTrainClassIndices, int iNumFeatures, int iNumObservations, CClassifierBase::Normalization_t eNorm)
{
    if (!ppfTrainFeatures || !piTrainClassIndices)
        return Error_t::kFunctionInvalidArgsError;
    if (!ppfTrainFeatures[0])
        return Error_t::kFunctionInvalidArgsError;
    if (iNumFeatures <= 0 || iNumObservations <= 1)
        return Error_t::kFunctionInvalidArgsError;

    m_iNumObs = iNumObservations;
    m_iNumFeatures = iNumFeatures;

    CVector::alloc(m_piClassLabels, m_iNumObs);
    CVector::alloc(m_piSortIdx, m_iNumObs);
    CVector::alloc(m_pfDist, m_iNumObs);
    CMatrix::alloc(m_ppfTrain, m_iNumObs, m_iNumFeatures);
    CVector::alloc(m_pfQuery, m_iNumFeatures);

    CVector::alloc(m_pfHist, m_iK);
    CVector::alloc(m_piHistLabel, m_iK);

    CVector::alloc(m_pfNormScale, m_iNumFeatures);
    CVector::alloc(m_pfNormSub, m_iNumFeatures);

    CVector::copy(m_piClassLabels, piTrainClassIndices, m_iNumObs);

    // note we store this transposed - that's easier for the distance computation
    CMatrix::transpose(m_ppfTrain, ppfTrainFeatures, m_iNumFeatures, m_iNumObs);

    // normaization constants
    if (eNorm == kZscoreNormalization)
    {
        for (auto f = 0; f < m_iNumFeatures; f++)
        {
            m_pfNormSub[f] = CVector::getMean(ppfTrainFeatures[f], m_iNumObs);
            m_pfNormScale[f] = CVector::getStd(ppfTrainFeatures[f], m_iNumObs, m_pfNormSub[f]);
            if (m_pfNormScale[f] > 0)
                m_pfNormScale[f] = 1.F / m_pfNormScale[f];
        }
    }
    else if (eNorm == kMinmaxNormalization)
    {
        for (auto f = 0; f < m_iNumFeatures; f++)
        {
            m_pfNormSub[f] = CVector::getMin(ppfTrainFeatures[f], m_iNumObs);
            m_pfNormScale[f] = CVector::getMax(ppfTrainFeatures[f], m_iNumObs) - m_pfNormSub[f];
            if (m_pfNormScale[f] != 0)
                m_pfNormScale[f] = 1.F / m_pfNormScale[f];
        }
    }
    else
    {
        CVector::setValue(m_pfNormScale, 1.F, m_iNumFeatures);
        CVector::setZero(m_pfNormSub, m_iNumFeatures);
    }

    // normalize features
    for (auto n = 0; n < m_iNumObs; n++)
    {
        CVector::sub_I(m_ppfTrain[n], m_pfNormSub, m_iNumFeatures);
        CVector::mul_I(m_ppfTrain[n], m_pfNormScale, m_iNumFeatures);
    }

    m_bIsInitialized = true;

    return Error_t::kNoError;
}

/*! resets Knn instance
\return Error_t
*/

Error_t CKnn::reset()
{
    CVector::free(m_piClassLabels);
    CVector::free(m_piSortIdx);
    CVector::free(m_pfDist);
    CMatrix::free(m_ppfTrain, m_iNumObs);

    CVector::free(m_pfQuery);

    CVector::free(m_pfHist);
    CVector::free(m_piHistLabel);

    CVector::free(m_pfNormScale);
    CVector::free(m_pfNormSub);

    m_bIsInitialized = false;

    return Error_t::kNoError;
}

Error_t CKnn::setParamK(int iK)
{
    if (iK <= 0)
        return Error_t::kFunctionInvalidArgsError;

    m_iK = iK;
    CVector::free(m_pfHist);
    CVector::free(m_piHistLabel);

    CVector::alloc(m_pfHist, m_iK);
    CVector::alloc(m_piHistLabel, m_iK);

    return Error_t::kNoError;
}
int CKnn::getParamK() const
{
    return m_iK;
}

int CKnn::classify(const float* pfQuery)
{
    if (!pfQuery || !m_bIsInitialized)
        return kIllegalClassLabel;

    // normalize
    CVector::copy(m_pfQuery, pfQuery, m_iNumFeatures);
    CVector::sub_I(m_pfQuery, m_pfNormSub, m_iNumFeatures);
    CVector::mul_I(m_pfQuery, m_pfNormScale, m_iNumFeatures);

    // compute distance to all training observations
    for (auto n = 0; n < m_iNumObs; n++)
        m_pfDist[n] = CVector::distEuclidean(pfQuery, m_ppfTrain[n], m_iNumFeatures);

    // sort distances
    CVector::sort_I(m_pfDist, m_piSortIdx, m_iNumObs);

    // take care of weird scenarios
    if (m_iK > m_iNumObs)
        m_iK = m_iNumObs;

    // create histogram
    buildHistogram_(false);

    // handle multiple maxima (first, use distances, then, reduce K)
    int iK = m_iK;  //!< remember m_iK
    while (countMaxima_() > 1)
    {
        buildHistogram_(true);
        m_iK--;
    }
    m_iK = iK;

    return m_piHistLabel[0];
}

void CKnn::buildHistogram_(bool bUseDistance)
{
    int iNumLabelsInK = 0;
    CVector::setValue(m_piHistLabel, kIllegalClassLabel, m_iK);
    m_piHistLabel[0] = m_piClassLabels[m_piSortIdx[0]];
    m_pfHist[0] += 1;
    iNumLabelsInK++;
    for (auto k = 1; k < m_iK; k++)
    {
        bool bContinue = false;
        for (auto l = 0; l < iNumLabelsInK; l++)
        {
            if (m_piHistLabel[l] == m_piClassLabels[m_piSortIdx[k]])
            {
                m_pfHist[l] += bUseDistance ? m_pfDist[m_piSortIdx[k]] : 1.F;
                bContinue = true;
                break;
            }
        }
        if (bContinue)
            continue;
        m_piHistLabel[iNumLabelsInK] = m_piClassLabels[m_piSortIdx[k]];
        m_pfHist[iNumLabelsInK] += 1;
        iNumLabelsInK++;
    }
}

int CKnn::countMaxima_()
{
    int iNumMax = 0;
    float fMax = 0;
    long long iMax = 0;
    CVector::findMax(m_pfHist, fMax, iMax, m_iK);
    CUtil::swap(m_pfHist[iNumMax], m_pfHist[iMax]);
    CUtil::swap(m_piHistLabel[iNumMax], m_piHistLabel[iMax]);
    iNumMax++;
    for (auto k = 1; k < m_iK; k++)
    {
        if (m_pfHist[k] >= fMax)
        {
            CUtil::swap(m_pfHist[iNumMax], m_pfHist[k]);
            CUtil::swap(m_piHistLabel[iNumMax], m_piHistLabel[k]);

            iNumMax++;
        }
    }
    return iNumMax;
}
