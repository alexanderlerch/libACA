#if !defined(__ACA_ClassifierBase_HEADER_INCLUDED__)
#define __ACA_ClassifierBase_HEADER_INCLUDED__

#pragma once


/*! \brief abstract base class for traditional classifiers
*/
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
    static const int kIllegalClassLabel;

    virtual Error_t init(int iNumFeatures, int iNumObservations) = 0;

    virtual Error_t train(float** ppfTrainFeatures, const int* piTrainClassIndices, Normalization_t eNorm) = 0;

    virtual Error_t reset() = 0;

    virtual int classify(const float* pfQuery) = 0;

protected:
    virtual ~CClassifierBase()
    {
        CVector::free(m_pfNormScale);
        CVector::free(m_pfNormSub);
    }

    virtual void compNormConstants(float** ppfTrainFeatures, int iNumFeatures, int iNumObs, Normalization_t eNorm)
    {
        CVector::alloc(m_pfNormScale, iNumFeatures);
        CVector::alloc(m_pfNormSub, iNumFeatures);

        // normaization constants
        if (eNorm == kZscoreNormalization)
        {
            for (auto f = 0; f < iNumFeatures; f++)
            {
                m_pfNormSub[f] = CVector::getMean(ppfTrainFeatures[f], iNumObs);
                m_pfNormScale[f] = CVector::getStd(ppfTrainFeatures[f], iNumObs, m_pfNormSub[f]);
                if (m_pfNormScale[f] > 0)
                    m_pfNormScale[f] = 1.F / m_pfNormScale[f];
            }
        }
        else if (eNorm == kMinmaxNormalization)
        {
            for (auto f = 0; f < iNumFeatures; f++)
            {
                m_pfNormSub[f] = CVector::getMin(ppfTrainFeatures[f], iNumObs);
                m_pfNormScale[f] = CVector::getMax(ppfTrainFeatures[f], iNumObs) - m_pfNormSub[f];
                if (m_pfNormScale[f] != 0)
                    m_pfNormScale[f] = 1.F / m_pfNormScale[f];
            }
        }
        else
        {
            CVector::setValue(m_pfNormScale, 1.F, iNumFeatures);
            CVector::setZero(m_pfNormSub, iNumFeatures);
        }
    }
    virtual void normalizeVector(float* pfQuery, int iNumFeatures)
    {
        CVector::sub_I(pfQuery, m_pfNormSub, iNumFeatures);
        CVector::mul_I(pfQuery, m_pfNormScale, iNumFeatures);
    }

    float* m_pfNormScale = 0; //!< scaling constant for normalization (length m_iNumFeatures)
    float* m_pfNormSub = 0; //!< offset for normalization (length m_iNumFeatures)
};

#endif //__ACA_ClassifierBase_HEADER_INCLUDED__