#if !defined(__Knn_HEADER_INCLUDED__)
#define __Knn_HEADER_INCLUDED__

#pragma once

#include "ErrorDef.h"

#include "Matrix.h"
#include "Vector.h"

class CClassifierBase
{
public:
    enum Normalization_t
    {
        kNoNormalization,
        kZscoreNormalization,
        kMinmaxNormalization,

        kNumNormModes
    };
    virtual Error_t init(float** ppfTrainFeatures, const int* piTrainClassIndices, int iNumFeatures, int iNumObservations, Normalization_t eNorm) = 0;

    virtual Error_t reset() = 0;

    virtual int classify(const float* pfQuery) = 0;

};

/*! \brief computation of k nearest neighbor
*/
class CKnn : public CClassifierBase
{
public:
    CKnn(void) {};
    virtual ~CKnn(void);;


    /*! initializes Knn instance
    \param iNumFeatures number of rows in the input and output matrices
    \param iNumObservations number of columns in the input and output matrices
    \return Error_t
    */
    Error_t init(float** ppfTrainFeatures, const int* piTrainClassIndices, int iNumFeatures, int iNumObservations, CClassifierBase::Normalization_t eNorm = CClassifierBase::kNoNormalization) override
    {
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
                m_pfNormSub[f] = CVectorFloat::getMean(ppfTrainFeatures[f], m_iNumObs);
                m_pfNormScale[f] = CVectorFloat::getStd(ppfTrainFeatures[f], m_iNumObs, m_pfNormSub[f]);
                if (m_pfNormScale[f] > 0)
                    m_pfNormScale[f] = 1.F / m_pfNormScale[f];
            }
        }
        else if (eNorm == kMinmaxNormalization)
        {
            for (auto f = 0; f < m_iNumFeatures; f++)
            {
                m_pfNormSub[f] = CVectorFloat::getMin(ppfTrainFeatures[f], m_iNumObs);
                m_pfNormScale[f] = CVectorFloat::getMax(ppfTrainFeatures[f], m_iNumObs) - m_pfNormSub[f];
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
            CVectorFloat::sub_I(m_ppfTrain[n], m_pfNormSub, m_iNumFeatures);
            CVectorFloat::mul_I(m_ppfTrain[n], m_pfNormScale, m_iNumFeatures);
        }
    }

    /*! resets Knn instance
    \return Error_t
    */
    Error_t reset() override
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
    }

    Error_t setParam(int iK = 3) 
    {
        m_iK = iK;
        CVector::free(m_pfHist);
        CVector::free(m_piHistLabel);

        CVector::alloc(m_pfHist, m_iK);
        CVector::alloc(m_piHistLabel, m_iK);

        return Error_t::kNoError;
    }

    /*! classifies a new query vector
    \param ppfRes matrix with principal components (user-allocated, to be written, dimensions equal input matrix)
    \param pfEigenValues eigenvalues of principle components (user-allocated, to be written, length equals number of features)
    \param ppfIn input matrix (dimensions iNumFeatures X iNumObservations)
    \return int
    */
    int classify(const float* pfQuery) override
    {
        // normalize
        CVector::copy(m_pfQuery, pfQuery, m_iNumFeatures);
        CVectorFloat::sub_I(m_pfQuery, m_pfNormSub, m_iNumFeatures);
        CVectorFloat::mul_I(m_pfQuery, m_pfNormScale, m_iNumFeatures);

        // compute distance to all training observations
        for (auto n = 0; n < m_iNumObs; n++)
            m_pfDist[n] = CVectorFloat::distEuclidean(pfQuery, m_ppfTrain[n], m_iNumFeatures);

        // sort distances
        CVectorFloat::sort_I(m_pfDist, m_piSortIdx, m_iNumObs);

        // create histogram
        buildHist_(false);

        // handle multiple maxima (first, use distances, then, reduce K
        while (countMaxima_() > 1)
        {
            buildHist_(true);
            m_iK--;
        }

        return m_piHistLabel[0];
    }


private:
    CKnn(const CKnn& that);     //!< disallow copy construction   
    CKnn& operator=(const CKnn& c);

    void buildHist_(bool bUseDistance)
    {
        int iNumLabelsInK = 0;
        CVector::setValue(m_piHistLabel, -1000001, m_iK);
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

    int countMaxima_()
    {
        int iNumMax = 0;
        float fMax = 0;
        long long iMax = 0;
        CVectorFloat::findMax(m_pfHist, fMax, iMax, m_iK);
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

    int m_iNumFeatures = 0,
        m_iNumObs = 0,
        
        m_iK = 3;

    float** m_ppfTrain = 0;
    float *m_pfDist = 0;
    float* m_pfQuery = 0;
    int* m_piClassLabels = 0;

    int* m_piSortIdx = 0;

    float* m_pfHist = 0;
    int* m_piHistLabel = 0;

    float *m_pfNormScale = 0;
    float *m_pfNormSub = 0;

};


#endif
