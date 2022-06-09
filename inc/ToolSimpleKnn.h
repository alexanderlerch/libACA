#if !defined(__Knn_HEADER_INCLUDED__)
#define __Knn_HEADER_INCLUDED__

#pragma once

#include "ErrorDef.h"


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
    virtual ~CKnn(void);


    /*! initializes Knn instance
    \param ppfTrainFeatures feature data for 'training' (dimensions iNumFeatures X iNumObservations)
    \param piTrainClassIndices ground truth class index for each observation
    \param iNumFeatures number of features (rows in the input matrix)
    \param iNumObservations number of observations (columns in the feature matrix)
    \param eNorm specification of what normalization should be applied to the feature data
    \return Error_t
    */
    Error_t init(float** ppfTrainFeatures, const int* piTrainClassIndices, int iNumFeatures, int iNumObservations, CClassifierBase::Normalization_t eNorm = CClassifierBase::kNoNormalization) override;

    /*! resets Knn instance
    \return Error_t
    */
    Error_t reset() override;

    /*! set the number of neighbors K
    \param iK number of nearest neighbors taken into consideration
    \return Error_t
    */
    Error_t setParam(int iK = 3);

    /*! classifies a new query vector
    \param pfQuery vector of length iNumFeatures to classify
    \return int class label of most likely class (returns CClassifierBase::kIllegalClassLabel in case of error)
    */
    int classify(const float* pfQuery) override;


private:
    CKnn(const CKnn& that);     //!< disallow copy construction   
    CKnn& operator=(const CKnn& c);

    void buildHistogram_(bool bUseDistance);

    int countMaxima_();

    int m_iNumFeatures = 0, //!< number of features
        m_iNumObs = 0, //!< number of training observations
        
        m_iK = 3; //!< number of neighbors for classification

    float** m_ppfTrain = 0; //!< train data (m_iNumObs X m_iNumFeatures)
    int* m_piClassLabels = 0; //!< ground truth train labels
    
    float *m_pfDist = 0; //!< preallocated vector for distance of query to all data points
    float* m_pfQuery = 0; //!< preallocated vector for the normalized query
    int* m_piSortIdx = 0; //!< preallocated vector for the resorted class indices

    float* m_pfHist = 0; //!< preallocated vector for the histogram of nearest neighbors
    int* m_piHistLabel = 0; //!< preallocated vector holding the histogram class labels

    float *m_pfNormScale = 0; //!< scaling constant for normalization (length m_iNumFeatures)
    float *m_pfNormSub = 0; //!< offset for normalization (length m_iNumFeatures)

    bool m_bIsInitialized = false; //!< indicates if instance has been properly initialized

};


#endif
