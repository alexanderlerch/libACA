#if !defined(__ACA_GmmClassifier_HEADER_INCLUDED__)
#define __ACA_GmmClassifier_HEADER_INCLUDED__

#pragma once

#include "ErrorDef.h"

#include "Vector.h"
#include "Matrix.h"
#include "ClassifierBase.h"
#include "ToolGmm.h"


/*! \brief computation of k nearest neighbor
*/
class CGmmClassifier : public CClassifierBase
{
public:
    CGmmClassifier(void) {};
    virtual ~CGmmClassifier(void) { reset(); };


    /*! initializes GmmClassifier instance
    \param ppfTrainFeatures feature data for 'training' (dimensions iNumFeatures X iNumObservations)
    \param piTrainClassIndices ground truth class index for each observation
    \param iNumFeatures number of features (rows in the input matrix)
    \param iNumObservations number of observations (columns in the feature matrix)
    \param eNorm specification of what normalization should be applied to the feature data
    \return Error_t
    */
    Error_t init(int iNumFeatures, int iNumObservations) override
    {
        if (iNumFeatures <= 0 || iNumObservations < 1)
            return Error_t::kFunctionInvalidArgsError;
       
        reset();

        m_iNumFeatures = iNumFeatures;
        m_iNumObs = iNumObservations;

        // worst case: number of classes equals number of observations
        CVector::alloc(m_piClassLabels, m_iNumObs); 

        CVector::alloc(m_pfQuery, m_iNumFeatures);

        m_bIsInitialized = true;

        return Error_t::kNoError;
    }
    Error_t train(float** ppfTrainFeatures, const int* piTrainClassIndices, CClassifierBase::Normalization_t eNorm = CClassifierBase::kNoNormalization) override
    {
        if (!m_bIsInitialized)
            return Error_t::kFunctionIllegalCallError;

        if (!ppfTrainFeatures || !piTrainClassIndices)
            return Error_t::kFunctionInvalidArgsError;
        
        if (!ppfTrainFeatures[0])
            return Error_t::kFunctionInvalidArgsError;

        float** ppfTrain = 0;
        CGmm Gmm;

        // alloc temp train data matrix
        CMatrix::alloc(ppfTrain, m_iNumFeatures, m_iNumObs);

        // get number of classes
        m_iNumClasses = countClasses(piTrainClassIndices);

        // allocate GMM stuff but keep result for later
        CVector::alloc(m_ppCGmmResult, m_iNumClasses);

        
        // normalize features and train GMM per class
        compNormConstants(ppfTrainFeatures, m_iNumFeatures, m_iNumObs, eNorm);
        for (auto c = 0; c < m_iNumClasses; c++)
        {
            Gmm.reset();
            int iNumClassObs = 0;
            for (auto n = 0; n < m_iNumObs; n++)
            {
                // append class-specific observations
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

    /*! resets GmmClassifier instance
    \return Error_t
    */
    Error_t reset() override
    {
        m_bIsInitialized = false;

        CVector::free(m_piClassLabels);

        CVector::free(m_pfQuery);

        for (auto c = 0; c < m_iNumClasses; c++)
            delete m_ppCGmmResult[c];
        CVector::free(m_ppCGmmResult);

        m_iNumClasses = 0;
        m_iNumFeatures = 0;
        m_iNumObs = 0;

        return Error_t::kNoError;
    }

    /*! sets the number of neighbors K
    \param iK number of nearest neighbors taken into consideration
    \return Error_t
    */
    Error_t setNumMixtures(int iK = 3)
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


    /*! returns the parameter K
    \return int iK
    */
    int getNumMixtures() const
    {
        return m_iK;
    }

    /*! classifies a new query vector
    \param pfQuery vector of length iNumFeatures to classify
    \return int class label of most likely class (returns CClassifierBase::kIllegalClassLabel in case of error)
    */
    int classify(const float* pfQuery) override
    {
        if (!pfQuery || !m_bIsInitialized)
            return kIllegalClassLabel;

        // normalize
        CVector::copy(m_pfQuery, pfQuery, m_iNumFeatures);
        normalizeVector(m_pfQuery, m_iNumFeatures);

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


private:
    CGmmClassifier(const CGmmClassifier& that);     //!< disallow copy construction   
    CGmmClassifier& operator=(const CGmmClassifier& c);

    int countClasses(const int* piClassLabels)
    {
        m_iNumClasses = 1;
        m_piClassLabels[0] = piClassLabels[0];

        for (auto n = 1; n < m_iNumObs; n++)
        {
            int i = 0;
            while (i < m_iNumClasses)
            {
                if (piClassLabels[n] == m_piClassLabels[i])
                    break;
                i++;
            }
            if (i == m_iNumClasses)
            {
                m_piClassLabels[m_iNumClasses] = piClassLabels[n];
                m_iNumClasses++;
            }
        }

        return m_iNumClasses;
    }

    int m_iNumFeatures = 0, //!< number of features
        m_iNumObs = 0, //!< number of training observations
        m_iNumClasses = 0,
        m_iK = 3; //!< number of neighbors for classification

    int* m_piClassLabels = 0; 
    float* m_pfQuery = 0; //!< preallocated vector for the normalized query

    bool m_bIsInitialized = false; //!< indicates if instance has been properly initialized

    CGmmResult** m_ppCGmmResult = 0;

};


#endif // __ACA_GmmClassifier_HEADER_INCLUDED__
