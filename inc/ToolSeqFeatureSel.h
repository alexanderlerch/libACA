#if !defined(__ACA_SeqFeatureSel_HEADER_INCLUDED__)
#define __ACA_SeqFeatureSel_HEADER_INCLUDED__

#pragma once

#include "ErrorDef.h"

// forward declarations
class CKnn;
class CLeaveOneOutCrossVal;

/*! \brief computation of sequential feature forward selection
*/
class CSeqFeatureSel
{
public:
    CSeqFeatureSel(void) {};
    virtual ~CSeqFeatureSel(void);

    /*! initializes SeqFeatureSel instance
    \param iNumFeatures number of features (rows in the input matrix)
    \param iNumObs number of observations (columns in the feature matrix)
    \return Error_t
    */
    Error_t init(int iNumFeatures, int iNumObs);

    /*! selects the featyres
    \param ppfFeatures feature data for 'training' (dimensions iNumFeatures X iNumObs)
    \param piClassIndices ground truth class index for each observation
    \return Error_t
    */
    Error_t process(const float *const *const ppfFeatures, const int *piClassIndices);

    /*! writes results after processing
    \param piFeatureIndices sorted feature indices, best first (dimensions iNumFeatures)
    \param pfAccuracyPerStep best accuracy for each combination (1 feature, 2 features, 3 features...)
    \return Error_t
    */
    Error_t getResult(int *piFeatureIndices, float *pfAccuracyPerStep = 0);

    /*! resets SeqFeatureSel instance
    \return Error_t
    */
    Error_t reset();



private:
    CSeqFeatureSel(const CSeqFeatureSel &that);     //!< disallow copy construction   
    CSeqFeatureSel &operator=(const CSeqFeatureSel &c);

    /*! return true if feature index is already in the list of selected features
    \param iFeatureIdx index of the feature
    \return bool
    */
    bool isFeatureAlreadySelected_(int iFeatureIdx) const
    {
        for (auto v = 0; v < m_iNumFeatures; v++)
        {
            if (iFeatureIdx == m_piSelFeatures[v])
                return true;
            if (m_piSelFeatures[v] < 0)
                return false;
        }

        return false;
    }

    int m_iNumFeatures = 0, //!< number of features
        m_iNumObs = 0; //!< number of neighbors for classification

    float **m_ppfTrain = 0; //!< train data (m_iNumObs X m_iNumFeatures)

    float *m_pfAccuracy = 0; //!< resulting best accuracy over number of features

    int *m_piSelFeatures = 0; //!< resulting selected feature indices

    CKnn *m_pCClassifier = 0; //!< handle to wrapped classifier
    CLeaveOneOutCrossVal *m_pCCv = 0; //!< cross validation instance


    bool m_bIsInitialized = false; //!< indicates if instance has been properly initialized
    bool m_bWasProcessed = false; //!< indicates if process function had been successfully executed

};

#endif // __ACA_SeqFeatureSel_HEADER_INCLUDED__
