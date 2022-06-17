#if !defined(__ACA_LeaveOneOutCrossVal_HEADER_INCLUDED__)
#define __ACA_LeaveOneOutCrossVal_HEADER_INCLUDED__

#pragma once

#include "ErrorDef.h"

// forward declaration
class CClassifierBase;

/*! \brief computation of leave one out cross validation
*/
class CLeaveOneOutCrossVal
{
public:
    CLeaveOneOutCrossVal(void) {};
    virtual ~CLeaveOneOutCrossVal(void);

    /*! initializes LeaveOneOutCrossVal instance
    \param iNumFeatures number of features (rows in the input matrix)
    \param iNumObs number of observations (columns in the feature matrix)
    \param pCClassifier classifier to be used internally
    \return Error_t
    */
    Error_t init(int iNumFeatures, int iNumObs, CClassifierBase* pCClassifier);

    /*! initializes LeaveOneOutCrossVal instance
    \param ppfTrainFeatures feature data for 'training' (dimensions iNumFeatures X iNumObs)
    \param piTrainClassIndices ground truth class index for each observation
    \return Error_t
    */
    float process(const float* const* const ppfTrainFeatures, const int* piTrainClassIndices);

    /*! resets LeaveOneOutCrossVal instance
    \return Error_t
    */
    Error_t reset();



private:
    CLeaveOneOutCrossVal(const CLeaveOneOutCrossVal& that); //!< disallow copy construction   
    CLeaveOneOutCrossVal& operator=(const CLeaveOneOutCrossVal& c);

    int m_iNumFeatures = 0, //!< number of features
        m_iNumObs = 0; //!< number of observations

    float** m_ppfTrain = 0; //!< temporary train data (m_iNumObs X m_iNumFeatures)

    float* m_pfQuery = 0; //!< preallocated vector for the normalized query (test vector)

    int* m_piClassLabels = 0; //!< vector holding the temporary train set class labels

    CClassifierBase* m_pCClassifier = 0; //!< pointer to classifier to use

    bool m_bIsInitialized = false; //!< indicates if instance has been properly initialized

};

#endif // __ACA_LeaveOneOutCrossVal_HEADER_INCLUDED__
