#if !defined(__ACA_Kmeans_HEADER_INCLUDED__)
#define __ACA_Kmeans_HEADER_INCLUDED__

#pragma once

#include "ErrorDef.h"


/*! \brief implements kmeans clustering
*/
class CKmeans
{
public:

    CKmeans(void) {};
    virtual ~CKmeans(void);

    /*! initializes the class
    \param iK target number of clusters
    \param iNumFeatures number of rows of matrix to be clustered
    \param iNumObs number of columns of matrix to be clustered
    \param iMaxIter maximum number of iterations
    \return Error_t
    */
    Error_t init(int iK, int iNumFeatures, int iNumObs, int iMaxIter = 300);

    /*! resets all internal class members
    \return Error_t
    */
    Error_t reset();


    /*! clusters the data
    \param piResult resulting cluster indices, starting with 0 (length iNumObs, user-allocated, to be written)
    \param ppfFeatures input matrix of dimesions iNumFeatures X iNumObs
    \return Error_t
    */
    Error_t compKmeans(int *piResult, const float *const *const ppfFeatures);


private:
    enum States_t
    {
        kPrev,
        kCurr,

        kNumStates
    };
    CKmeans(const CKmeans &that);     //!< disallow copy construction   
    CKmeans &operator=(const CKmeans &c);

    /*! deals with empty clusters through new random initialization
    \param ppfFeatures input matrix of dimesions iNumFeatures X iNumObs
    */
    void reinitClusterMeans_(const float *const *const ppfFeatures);


    /*! randomly initializes the initial cluster means
    \param ppfFeatures input matrix of dimesions iNumFeatures X iNumObs
    */
    void initClusterMeans_(const float *const *const ppfFeatures);


    /*! computes the current cluster means
    \param ppfFeatures input matrix of dimesions iNumFeatures X iNumObs
    \param piResult current cluster assignments
    */
    void compClusterMeans_(const float *const *const ppfFeatures, const int *piResult);


    /*! check if the means have changed during the last iteration
    \return bool true if unchanged
    */
    bool checkConverged_();


    /*! assigns cluster labels given the means
    \param piResult new cluster assignments
    \param ppfFeatures input matrix of dimesions iNumFeatures X iNumObs
    */
    void assignClusterLabels_(int *piResult, const float *const *const ppfFeatures);

    float **m_appfClusterMeans[kNumStates] = { 0 }; //!< contains the current and previous cluster means

    float *m_pfProc = 0, //!< temporary pre-allocated memory buffer 
        *m_pfDist = 0; //!< buffer for distance calculation

    int *m_piClusterSize = 0; //!< number of observations per cluster

    int m_iNumFeatures = 0, //!< number of feature dimension (matrix rows)
        m_iNumObs = 0, //!< number of observations (matrix cols)
        m_iK = 0, //!< number of clusters
        m_iMaxIter = 0; //!< maximum number of iterations

    bool m_bIsInitialized = false; //!< true if instance has been initialized
};

#endif // __ACA_Kmeans_HEADER_INCLUDED__
