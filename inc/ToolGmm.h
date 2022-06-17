#if !defined(__ACA_Gmm_HEADER_INCLUDED__)
#define __ACA_Gmm_HEADER_INCLUDED__

#pragma once

#include "ErrorDef.h"


/*! \brief class holding the result/details of a Gaussian Mixture Model created with ::CGmm
*/
class CGmmResult
{
    friend class CGmm;

public:
    CGmmResult() {};
    virtual ~CGmmResult(void);
    CGmmResult(const CGmmResult &that);

    /*! returns the number of Gaussians in the mixture
    \return int
    */
    int getNumGaussians() const;

    /*! returns the dimensionality of each Gaussian, i.e., the number of features
    \return int
    */
    int getNumDimensions() const;

    /*! returns mean for one Gaussian and one feature
    \param iGaussianIdx index of Gaussian
    \param iFeatureIdx index of feature
    \return float
    */
    float getMu(int iGaussianIdx, int iFeatureIdx) const;

    /*! returns the prior for one Gaussian
    \param iGaussianIdx index of Gaussian
    \return float
    */
    float getPrior(int iGaussianIdx) const;

    /*! returns entry of sigma matrix for one Gaussian,
    \param iGaussianIdx index of Gaussian
    \param iRowIdx row index (< iNumFeatures)
    \param iColIdx col index (< iNumFeatures)
    \return float
    */
    float getSigma(int iGaussianIdx, int iRowIdx, int iColIdx) const;

    /*! writes sigma matrix for one Gaussian
    \param ppfSigma to be written (user allocated, dimensions iNumFeatures X iNumFeatures)
    \param iGaussianIdx index of Gaussian
    */
    void getSigma(float **ppfSigma, int iGaussianIdx) const;

    float getProb(const float *pfQuery);

    /*! returns initialization state
    \return bool true if initialized
    */
    bool isInitialized() const;

    /*! assignment operator
    */
    CGmmResult &operator=(const CGmmResult &that);

protected:
    // these are called by our friend
    Error_t init(int iK, int iNumFeatures);
    Error_t reset();
    Error_t setMu(int iGaussianIdx, int iFeatureIdx, float fParamValue);
    Error_t setPrior(int iGaussianIdx, float fParamValue);
    Error_t setSigma(int iGaussianIdx, float **ppfSigma);
private:
    enum Sigma_t
    {
        kNormal,
        kInv,

        kSigma
    };
    float **m_ppfMu = 0; //!< means, dim: cluster x feature
    float *m_pfPrior = 0; //!< priors, dim: cluster
    float ***m_apppfSigma[kSigma] = { 0 }; //!< sigma, cluster x feature x feature
    float *m_apfProc[2] = { 0 }; //!< pre-allocated temporary memory buffer 

    int m_iK = 0; //!< number of Gaussians
    int m_iNumFeatures = 0; //!< number of features

    bool m_bIsInitialized = false; //!< indicates whether instance is initialized
};


/*! \brief implements Gaussian mixture model
*/
class CGmm
{
public:

    CGmm(void) {};
    virtual ~CGmm(void);

    /*! initializes the class
    \param pCResult class holding resulting mean, sigma, and priors, see class ::CGmmResult (user-allocated)
    \param iK target number of Gaussians
    \param iNumFeatures number of rows (features) of input matrix
    \param iNumObs number of columns (observations) of input matrix
    \param iMaxIter maximum number of iterations
    \return Error_t
    */
    Error_t init(CGmmResult *pCResult, int iK, int iNumFeatures, int iNumObs, int iMaxIter = 300);

    /*! resets all internal class members
    \return Error_t
    */
    Error_t reset();


    /*! computes the mixture model
    \param pCResult class holding resulting mean, sigma, and priors, see class ::CGmmResult
    \param ppfFeatures input matrix of dimensions iNumFeatures X iNumObs
    \return Error_t
    */
    Error_t compGmm(CGmmResult *pCResult, const float *const *const ppfFeatures);


private:
    enum States_t
    {
        kPrev,
        kCurr,

        kNumStates
    };
    CGmm(const CGmm &that); //!< disallow copy construction   
    CGmm &operator=(const CGmm &c);


    /*! randomly initializes the state variables
    \param ppfFeatures input matrix of dimensions iNumFeatures X iNumObs
    \param pCCurrState class holding the current state variables
    */
    void initState_(const float *const *const  ppfFeatures, CGmmResult *pCCurrState);


    /*! computes probabilities given the current state
    \param ppfFeatures input matrix of dimensions iNumFeatures X iNumObs
    \param pCCurrState class holding the current state variables
    */
    void compProbabilities_(const float *const *const ppfFeatures, CGmmResult *pCCurrState);

    /*! update mean, sigma, and prior
    \param ppfFeatures input matrix of dimensions iNumFeatures X iNumObs
    \param pCCurrState class holding the current state variables
    */
    void updateState_(const float *const *const ppfFeatures, CGmmResult *pCCurrState);


    /*! returns true if the means are identical compared to previous iteration
    \return bool true if identical
    */
    bool checkConverged_(CGmmResult *pCCurrState);


    CGmmResult PrevState; //!< previous state for convergence

    float **m_appfClusterMeans[kNumStates] = { 0 }; //!< contains the current and previous cluster means

    float *m_apfProc[2] = { 0 }, //!< temporary pre-allocated memory buffer 
        **m_appfSigma[2] = { 0 }, //!< temporary sigma matrices (m_iNumFeatures x m_iNumFeatures)
        **m_ppfProb = 0;  //!< probabilities (temp, dims m_iK x m_iNumObs)

    int *m_piClusterSize = 0; //!< number of observations per cluster

    int m_iNumFeatures = 0, //!< number of feature dimension (matrix rows)
        m_iNumObs = 0, //!< number of observations (matrix cols)
        m_iK = 0, //!< number of clusters
        m_iMaxIter = 300; //!< maximum number of iterations

    bool m_bisInitialized = false; //!< indicates whether instance is initialized
};

#endif // __ACA_Gmm_HEADER_INCLUDED__
