#if !defined(__ACA_LeaveOneOutCrossVal_HEADER_INCLUDED__)
#define __ACA_LeaveOneOutCrossVal_HEADER_INCLUDED__

#pragma once

#include "ErrorDef.h"

class ClassifierBase;

/*! \brief computation of k nearest neighbor
*/
class CLeaveOneOutCrossVal
{
public:
    CLeaveOneOutCrossVal(void) {};
    virtual ~CLeaveOneOutCrossVal(void);

    /*! initializes LeaveOneOutCrossVal instance
    \param iNumFeatures number of features (rows in the input matrix)
    \param iNumObservations number of observations (columns in the feature matrix)
    \return Error_t
    */
    Error_t init(int iNumFeatures, int iNumObservations, CClassifierBase* pCClassifier)
    {

        // set internal member variables
        m_iNumFeatures = iNumFeatures;
        m_iNumObs = iNumObservations;
        m_pCClassifier = pCClassifier;

        // allocate memory for feature and ground truth data, and query
        CMatrix::alloc(m_ppfTrain, m_iNumFeatures, m_iNumObs - 1);
        CVector::alloc(m_piClassLabels, m_iNumObs - 1),
    }

    /*! initializes LeaveOneOutCrossVal instance
    \param ppfTrainFeatures feature data for 'training' (dimensions iNumFeatures X iNumObservations)
    \param piTrainClassIndices ground truth class index for each observation
    \param eNorm specification of what normalization should be applied to the feature data
    \return Error_t
    */
    float process(float** ppfTrainFeatures, const int* piTrainClassIndices)
    {
        int iGtLabel = piTrainClassIndices[m_iNumObs - 1];
        int iTruePositives = 0;

        // initialize classifier
        m_pCClassifier->init(m_iNumFeatures, m_iNumObs - 1);

        // copy feature data and ground truth data
        CMatrix::copy(m_ppfTrain, ppfTrainFeatures, m_iNumFeatures, m_iNumObs - 1);
        CMatrix::getCol(m_pfQuery, ppfTrainFeatures, m_iNumObs - 1, m_iNumFeatures);
        CVector::copy(m_piClassLabels, piTrainClassIndices, m_iNumObs - 1);

        for (int n = 0; n < m_iNumObs; n++)
        {
            // train (check reset and memory alloc)
            m_pCClassifier->train(m_ppfTrain, m_piClassLabels)
            
            // classify
            if (iGtLabel == m_pCClassifier->classify(m_pfQuery))
                iTruePositives++;

            // prep for next round
            CMatrix::setCol(m_ppfTrain, m_pfQuery, m_iNumObs - 2 - n, m_iNumFeatures);
            CMatrix::getCol(m_pfQuery, ppfTrainFeatures, m_iNumObs - 2 - n, m_iNumFeatures);
            m_piClassLabels[m_iNumObs - 2 - n] = iGtLabel;
            iGtLabel = piTrainClassIndices[m_iNumObs - 2 - n];
        }
    }

    /*! resets LeaveOneOutCrossVal instance
    \return Error_t
    */
    Error_t reset() override;



private:
    CLeaveOneOutCrossVal(const CLeaveOneOutCrossVal& that);     //!< disallow copy construction   
    CLeaveOneOutCrossVal& operator=(const CLeaveOneOutCrossVal& c);

    int m_iNumFeatures = 0, //!< number of features
        m_iNumObs = 0; //!< number of neighbors for classification

    float** m_ppfTrain = 0; //!< train data (m_iNumObs X m_iNumFeatures)
    
    float* m_pfQuery = 0; //!< preallocated vector for the normalized query

    int m_piClassLabels = 0;

    CClassifierBase* m_pCClassifier = 0;

    bool m_bIsInitialized = false; //!< indicates if instance has been properly initialized

};


#endif // __ACA_LeaveOneOutCrossVal_HEADER_INCLUDED__
