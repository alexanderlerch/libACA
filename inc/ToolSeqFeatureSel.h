#if !defined(__ACA_SeqFeatureSel_HEADER_INCLUDED__)
#define __ACA_SeqFeatureSel_HEADER_INCLUDED__

#pragma once

#include "ErrorDef.h"

// forward declarations
class CKnn;
class CLeaveOneOutCrossVal;

/*! \brief computation of leav one out cross validation
*/
class CSeqFeatureSel
{
public:
    CSeqFeatureSel(void) {};
    virtual ~CSeqFeatureSel(void);

    /*! initializes SeqFeatureSel instance
    \param iNumFeatures number of features (rows in the input matrix)
    \param iNumObservations number of observations (columns in the feature matrix)
    \param pCClassifier classifier to be used internally
    \return Error_t
    */
    Error_t init(int iNumFeatures, int iNumObservations);

    /*! initializes SeqFeatureSel instance
    \param ppfFeatures feature data for 'training' (dimensions iNumFeatures X iNumObservations)
    \param piClassIndices ground truth class index for each observation
    \return Error_t
    */
    Error_t process(float** ppfFeatures, const int* piClassIndices);

    /*! resets SeqFeatureSel instance
    \return Error_t
    */
    Error_t getResult(int *piFeatureIndices, float *pfAccuracyPerStep = 0);

    /*! resets SeqFeatureSel instance
    \return Error_t
    */
    Error_t reset();



private:
    CSeqFeatureSel(const CSeqFeatureSel& that);     //!< disallow copy construction   
    CSeqFeatureSel& operator=(const CSeqFeatureSel& c);

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

    float** m_ppfTrain = 0; //!< train data (m_iNumObs X m_iNumFeatures)
    
    float* m_pfAccuracy = 0; 

    int* m_piSelFeatures = 0;

    CKnn* m_pCClassifier = 0;
    CLeaveOneOutCrossVal* m_pCCv = 0;


    bool m_bIsInitialized = false; //!< indicates if instance has been properly initialized
    bool m_bWasProcessed = false;

};


#endif // __ACA_SeqFeatureSel_HEADER_INCLUDED__
