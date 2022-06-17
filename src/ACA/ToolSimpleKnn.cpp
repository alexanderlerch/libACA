
#include "Matrix.h"
#include "Vector.h"

#include "ToolSimpleKnn.h"

const int CClassifierBase::kIllegalClassLabel = -1111111111;


CKnn::~CKnn(void)
{
    reset();
}

Error_t CKnn::init(int iNumFeatures, int iNumObs)
{
    if (iNumFeatures <= 0 || iNumObs <= 1)
        return Error_t::kFunctionInvalidArgsError;

    reset();

    m_iNumObs = iNumObs;
    m_iNumFeatures = iNumFeatures;

    CVector::alloc(m_piClassLabels, m_iNumObs);
    CVector::alloc(m_piSortIdx, m_iNumObs);
    CVector::alloc(m_pfSortDist, m_iNumObs);
    CVector::alloc(m_pfQuery, m_iNumFeatures);

    CVector::alloc(m_pfNormScale, m_iNumFeatures);
    CVector::alloc(m_pfNormSub, m_iNumFeatures);

    CMatrix::alloc(m_ppfTrain, m_iNumObs, m_iNumFeatures);

    CVector::alloc(m_pfHist, m_iK);
    CVector::alloc(m_piHistLabel, m_iK);
    CVector::alloc(m_piHistCount, m_iK);

    m_bIsInitialized = true;

    return Error_t::kNoError;
}

Error_t CKnn::reset()
{
    CVector::free(m_piClassLabels);
    CVector::free(m_piSortIdx);
    CVector::free(m_pfSortDist);
    CVector::free(m_pfQuery);
    CMatrix::free(m_ppfTrain, m_iNumObs);

    CVector::free(m_pfHist);
    CVector::free(m_piHistLabel);
    CVector::free(m_piHistCount);

    CVector::free(m_pfNormScale);
    CVector::free(m_pfNormSub);

    m_iNumObs = 0;
    m_iNumFeatures = 0;

    m_bIsInitialized = false;

    return Error_t::kNoError;
}
Error_t CKnn::train(const float *const *const ppfTrainFeatures, const int *piTrainClassIndices, CClassifierBase::Normalization_t eNorm)
{
    if (!ppfTrainFeatures || !piTrainClassIndices)
        return Error_t::kFunctionInvalidArgsError;
    if (!ppfTrainFeatures[0])
        return Error_t::kFunctionInvalidArgsError;
    if (!m_bIsInitialized)
        return Error_t::kFunctionIllegalCallError;

    CVector::copy(m_piClassLabels, piTrainClassIndices, m_iNumObs);

    // note we store this transposed - that's easier for the distance computation
    CMatrix::transpose(m_ppfTrain, ppfTrainFeatures, m_iNumFeatures, m_iNumObs);

    // normalize features
    compNormConstants(ppfTrainFeatures, m_iNumFeatures, m_iNumObs, eNorm);
    for (auto n = 0; n < m_iNumObs; n++)
        normalizeVector(m_ppfTrain[n], m_iNumFeatures);

    return Error_t::kNoError;
}


Error_t CKnn::setParamK(int iK)
{
    if (iK <= 0)
        return Error_t::kFunctionInvalidArgsError;

    if (m_iK == iK)
        return Error_t::kNoError;

    CVector::free(m_pfHist);
    CVector::free(m_piHistLabel);
    CVector::free(m_piHistCount);

    m_iK = iK;
    CVector::alloc(m_pfHist, m_iK);
    CVector::alloc(m_piHistLabel, m_iK);
    CVector::alloc(m_piHistCount, m_iK);

    return Error_t::kNoError;
}
int CKnn::getParamK() const
{
    return m_iK;
}

int CKnn::classify(const float *pfQuery)
{
    if (!pfQuery || !m_bIsInitialized)
        return kIllegalClassLabel;

    // normalize
    CVector::copy(m_pfQuery, pfQuery, m_iNumFeatures);
    normalizeVector(m_pfQuery, m_iNumFeatures);

    // compute distance to all training observations
    for (auto n = 0; n < m_iNumObs; n++)
        m_pfSortDist[n] = CVector::distEuclidean(pfQuery, m_ppfTrain[n], m_iNumFeatures);

    // sort distances
    CVector::sort_I(m_pfSortDist, m_piSortIdx, m_iNumObs);

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

    // init
    CVector::setZero(m_pfHist, m_iK);
    CVector::setZero(m_piHistCount, m_iK);
    CVector::setValue(m_piHistLabel, kIllegalClassLabel, m_iK);

    // set first candidate (lowest distance)
    m_piHistLabel[0] = m_piClassLabels[m_piSortIdx[0]];
    m_pfHist[0] += bUseDistance ? m_pfSortDist[0] : 1.F;
    m_piHistCount[0]++;
    iNumLabelsInK++;
    for (auto k = 1; k < m_iK; k++)
    {
        bool bContinue = false;
        for (auto l = 0; l < iNumLabelsInK; l++)
        {
            // check if current label k belongs to existing class
            if (m_piHistLabel[l] == m_piClassLabels[m_piSortIdx[k]])
            {
                m_pfHist[l] += bUseDistance ? m_pfSortDist[k] : 1.F;
                m_piHistCount[l]++;
                bContinue = true;
                break;
            }
        }
        if (bContinue)
            continue;

        // create new hist class
        m_piHistLabel[iNumLabelsInK] = m_piClassLabels[m_piSortIdx[k]];
        m_pfHist[iNumLabelsInK] += bUseDistance ? m_pfSortDist[k] : 1.F;
        m_piHistCount[iNumLabelsInK]++;
        iNumLabelsInK++;
    }

    // distance-based metrics
    if (bUseDistance)
    {
        // scale by number of entries in histogram
        for (auto k = 0; k < iNumLabelsInK; k++)
            if (m_piHistCount[k] > 0) m_pfHist[k] /= m_piHistCount[k];

        // invert so that distance becomes similarity
        float fMax = CVector::getMax(m_pfHist, iNumLabelsInK);
        if (fMax > 0)
            CVector::mulC_I(m_pfHist, -1.F / fMax, iNumLabelsInK);
        CVector::addC_I(m_pfHist, 1.F, iNumLabelsInK);
    }
}

int CKnn::countMaxima_()
{
    int iNumMax = 0;
    float fMax = 0;
    long long iMax = 0;

    // find first max
    CVector::findMax(m_pfHist, fMax, iMax, m_iK);
    CUtil::swap(m_pfHist[iNumMax], m_pfHist[iMax]);
    CUtil::swap(m_piHistLabel[iNumMax], m_piHistLabel[iMax]);
    iNumMax++;
    for (auto k = 1; k < m_iK; k++)
    {
        // check for others
        if (m_pfHist[k] >= fMax)
        {
            CUtil::swap(m_pfHist[iNumMax], m_pfHist[k]);
            CUtil::swap(m_piHistLabel[iNumMax], m_piHistLabel[k]);

            iNumMax++;
        }
    }
    return iNumMax;
}
