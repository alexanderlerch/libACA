#if !defined(__ACA_ClassifierBase_HEADER_INCLUDED__)
#define __ACA_ClassifierBase_HEADER_INCLUDED__

#pragma once

#include "Vector.h"

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
    static const int kIllegalClassLabel; //!< indicates something went wrong (this number should not be used as a class label)

    /*! initializes instance
    \param iNumFeatures number of features (rows in the input matrix)
    \param iNumObs number of observations (columns in the feature matrix)
    \return Error_t
    */
    virtual Error_t init(int iNumFeatures, int iNumObs) = 0;

    /*! trains a classifier instance
    \param ppfTrainFeatures feature data for 'training' (dimensions iNumFeatures X iNumObs)
    \param piTrainClassIndices ground truth class index for each observation
    \param eNorm specification of what normalization should be applied to the feature data
    \return Error_t
    */
    virtual Error_t train(const float* const* const ppfTrainFeatures, const int* piTrainClassIndices, Normalization_t eNorm = kNoNormalization) = 0;

    /*! resets classifier instance
    \return Error_t
    */
    virtual Error_t reset() = 0;

    /*! classifies a new query vector
    \param pfQuery vector of length iNumFeatures to classify
    \return int class label of most likely class (returns CClassifierBase::kIllegalClassLabel in case of error)
    */
    virtual int classify(const float* pfQuery) = 0;

protected:
    virtual ~CClassifierBase()
    {
        CVector::free(m_pfNormScale);
        CVector::free(m_pfNormSub);
    }

    /*! estimates the constants for data normalization
    \param ppfTrainFeatures training feature matrix (iNumFeatures X iNumObs)
    \param iNumFeatures number of features (rows in the input matrix)
    \param iNumObs number of observations (columns in the feature matrix)
    \param eNorm method for normalization
    */
    virtual void compNormConstants(const float* const* const ppfTrainFeatures, int iNumFeatures, int iNumObs, Normalization_t eNorm)
    {
        assert(m_pfNormScale);
        assert(m_pfNormSub);

        // normalization constants
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

    /*! normalizes a new query vector according to previously extracted constants
    \param pfQuery vector of length iNumFeatures to classify
    \param iNumFeatures number of features (rows in the input matrix)
    */
    virtual void normalizeVector(float* pfQuery, int iNumFeatures)
    {
        CVector::sub_I(pfQuery, m_pfNormSub, iNumFeatures);
        CVector::mul_I(pfQuery, m_pfNormScale, iNumFeatures);
    }

    float* m_pfNormScale = 0; //!< scaling constant for normalization (length m_iNumFeatures)
    float* m_pfNormSub = 0; //!< offset for normalization (length m_iNumFeatures)
};

#endif //__ACA_ClassifierBase_HEADER_INCLUDED__
