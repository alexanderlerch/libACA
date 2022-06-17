#if !defined(__ACA_GmmClassifier_HEADER_INCLUDED__)
#define __ACA_GmmClassifier_HEADER_INCLUDED__

#pragma once

#include "ErrorDef.h"

#include "ClassifierBase.h"


/*! \brief computation of k nearest neighbor
*/
class CGmmClassifier : public CClassifierBase
{
public:
    CGmmClassifier(void) {};
    virtual ~CGmmClassifier(void);


    /*! initializes GmmClassifier instance
    \param iNumFeatures number of features (rows in the input matrix)
    \param iNumObservations number of observations (columns in the feature matrix)
    \return Error_t
    */
    Error_t init(int iNumFeatures, int iNumObservations) override;

    /*! trains a GmmClassifier instance
    \param ppfTrainFeatures feature data for 'training' (dimensions iNumFeatures X iNumObservations)
    \param piTrainClassIndices ground truth class index for each observation
    \param eNorm specification of what normalization should be applied to the feature data
    \return Error_t
    */
    Error_t train(const float *const *const ppfTrainFeatures, const int *piTrainClassIndices, CClassifierBase::Normalization_t eNorm = CClassifierBase::kNoNormalization) override;

    /*! resets GmmClassifier instance
    \return Error_t
    */
    Error_t reset() override;

    /*! sets the number of Gaussians per class
    \param iK number of Gaussians
    \return Error_t
    */
    Error_t setNumMixtures(int iK = 3);


    /*! returns the number of Gaussians per class
    \return int
    */
    int getNumMixtures() const;

    /*! classifies a new query vector
    \param pfQuery vector of length iNumFeatures to classify
    \return int class label of most likely class (returns CClassifierBase::kIllegalClassLabel in case of error)
    */
    int classify(const float *pfQuery) override;


private:
    CGmmClassifier(const CGmmClassifier &that); //!< disallow copy construction   
    CGmmClassifier &operator=(const CGmmClassifier &c);

    /*! counts the number of unique classes in piClassLabels
    \param piClassLabels vector containing int class labels
    \return int number of classes
    */
    int countClasses(const int *piClassLabels);

    int m_iNumFeatures = 0, //!< number of features
        m_iNumObs = 0, //!< number of training observations
        m_iNumClasses = 0, //!< number of classes in the data
        m_iK = 3; //!< number of neighbors for classification

    int *m_piClassLabels = 0;  //!< preallocated vector containing the unique set of classes
    float *m_pfQuery = 0; //!< preallocated vector for the normalized query

    bool m_bIsInitialized = false; //!< indicates if instance has been properly initialized

    CGmmResult **m_ppCGmmResult = 0; //!< array holding the individual Gmm models (length m_iNumClasses)

};

#endif // __ACA_GmmClassifier_HEADER_INCLUDED__
