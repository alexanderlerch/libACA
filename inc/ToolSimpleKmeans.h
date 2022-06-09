#if !defined(__Kmeans_HEADER_INCLUDED__)
#define __Kmeans_HEADER_INCLUDED__

#pragma once

#include "ErrorDef.h"

#include "Matrix.h"
#include "Vector.h"
#include "Synthesis.h"


/*! \brief implements kmeans clustering
*/
class CKmeans
{
public:

    CKmeans(void);
    virtual ~CKmeans(void);

    /*! initializes the class with the size of the distance matrix
    \param pCKmeansResult class holding the result to be initialized (user allocated)
    \param iRank Kmeans rank
    \param iNumRows number of rows of matrix to be factorized
    \param iNumCols number of columns of matrix to be factorized
    \param iMaxIter maximum number of iterations
    \param fSparsity sparsity weight
    \return Error_t
    */
    Error_t init(int iK, int iNumFeatures, int iNumObservations, int iMaxIter = 300)
    {
        if (iK <= 0 || iNumFeatures <= 0 || iNumObservations <= iK || iMaxIter < 1)
            return Error_t::kFunctionInvalidArgsError;

        m_iK = iK;
        m_iNumFeatures = iNumFeatures;
        m_iNumObs = iNumObservations;
        m_iMaxIter = iMaxIter;

        CMatrix::alloc(m_appfClusterMeans[kPrev], m_iK, m_iNumFeatures);
        CMatrix::alloc(m_appfClusterMeans[kCurr], m_iK, m_iNumFeatures);

        CVector::alloc(m_pfProc, std::max(m_iK, m_iNumFeatures));
        CVector::alloc(m_pfDist, m_iK);
        CVector::alloc(m_piClusterSize, m_iK);

        m_bIsInitialized = true;
    }
    
    /*! resets all internal class members
    \return Error_t
    */
    Error_t reset()
    {
        CMatrix::free(m_appfClusterMeans[kPrev], m_iK);
        CMatrix::free(m_appfClusterMeans[kCurr], m_iK);
        CVector::free(m_pfProc);
        CVector::free(m_pfDist);
        CVector::free(m_piClusterSize);

        m_iK = 0;
        m_iNumFeatures = 0;
        m_iNumObs = 0;
        m_iMaxIter = 0;

        m_bIsInitialized = false;
    }

    Error_t compKmeans(int *piResult, float** ppfFeatures)
    {
        // init: randomly selected data points as cluster means
        initClusterMeans_(ppfFeatures);

        // assign observations to clusters
        assignClusterLabels_(piResult, ppfFeatures);

        // iterate
        for (auto i = 0; i < m_iMaxIter; i++)
        {
            // copy state to prev state
            CMatrix::copy(m_appfClusterMeans[kPrev], m_appfClusterMeans[kCurr], m_iK, m_iNumFeatures);

            // update cluster means
            computeClusterMeans_(ppfFeatures, piResult);

            // re-init empty clusters

            // assign observations to clusters
            assignClusterLabels_(piResult, ppfFeatures);

            // check for change
            if (checkConverged_()) 
                break;
        }
    }

    void reinitClusterMeans_(float** ppfFeatures)
    {
        if (CVector::getMin(m_piClusterSize, m_iK) > 0)
            return;
        CSynthesis::genNoise(m_pfProc, m_iK);
        CVector::addC_I(m_pfProc, 1.F, m_iK);
        CVector::mulC_I(m_pfProc, (m_iNumObs - 1) / 2.F, m_iK);
        for (auto k = 0; k < m_iK; k++)
        {
            if (m_piClusterSize[k] > 0)
                continue;
            int iIdx = CUtil::float2int<int>(m_pfProc[k]);
            assert(iIdx > 0 && iIdx < m_iNumObs);

            for (auto v = 0; v < m_iNumFeatures; v++)
                m_appfClusterMeans[kCurr][k][v] = ppfFeatures[v][iIdx];
        }
    }

    void initClusterMeans_(float** ppfFeatures)
    {
        CSynthesis::genNoise(m_pfProc, m_iK);
        CVector::addC_I(m_pfProc, 1.F, m_iK);
        CVector::mulC_I(m_pfProc, (m_iNumObs - 1) / 2.F, m_iK);
        for (auto k = 0; k < m_iK; k++)
        {
            int iIdx = CUtil::float2int<int>(m_pfProc[k]);
            assert(iIdx > 0 && iIdx < m_iNumObs);

            for (auto v = 0; v < m_iNumFeatures; v++)
                m_appfClusterMeans[kCurr][k][v] = ppfFeatures[v][iIdx];
        }
    }

    void computeClusterMeans_(float** ppfFeatures, const int* piResult)
    {
        CMatrix::setZero(m_appfClusterMeans[kCurr], m_iK, m_iNumFeatures);
        CVector::setZero(m_piClusterSize, m_iK);
        for (auto n = 0; n < m_iNumObs; n++)
        {
            for (auto v = 0; v < m_iNumFeatures; v++)
                m_appfClusterMeans[kCurr][piResult[n]][v] += ppfFeatures[v][n];
        }
        for (auto k = 0; k < m_iK; k++)
            CVector::mulC_I(m_appfClusterMeans[kCurr][k], 1.F / m_piClusterSize[k], m_iNumFeatures);
    }

    bool checkConverged_()
    {
        float fSum = 0;
        int k = 0;
        CVector::copy(m_pfProc, m_appfClusterMeans[kCurr][k], m_iNumFeatures);
        CVector::sub_I(m_pfProc, m_appfClusterMeans[kPrev][k], m_iNumFeatures);
        while ((fSum += CVector::getSum(m_pfProc, m_iNumFeatures, true)) <= 0 && k < m_iK - 1)
        {
            k++;
            CVector::copy(m_pfProc, m_appfClusterMeans[kCurr][k], m_iNumFeatures);
            CVector::sub_I(m_pfProc, m_appfClusterMeans[kPrev][k], m_iNumFeatures);
        }
        if (fSum <= 0)
            return true;
        
        return false;
    }

    void assignClusterLabels_(int* piResult, float** ppfFeatures)
    {
        CVector::setZero(m_piClusterSize, m_iK);
        for (auto n = 0; n < m_iNumObs; n++)
        {
            float fMin = 0;
            long long iMin = -1;

            CMatrix::getCol(m_pfProc, ppfFeatures, n, m_iNumFeatures, m_iNumObs);
            // compute distance to all training observations
            for (auto k = 0; k < m_iK; k++)
                m_pfDist[k] = CVector::distEuclidean(m_appfClusterMeans[kCurr][k], m_pfProc, m_iNumFeatures);

            CVector::findMin(m_pfDist, fMin, iMin, m_iK);
            piResult[n] = iMin;
            m_piClusterSize[iMin]++;
        }
    }
 
 
private:
    enum States_t
    {
        kPrev,
        kCurr,

        kNumStates
    };
    CKmeans(const CKmeans& that);     //!< disallow copy construction   
    CKmeans& operator=(const CKmeans& c);

    float** m_appfClusterMeans[kNumStates] = { 0 };

    float* m_pfProc = 0,
        *m_pfDist = 0;

    int* m_piClusterSize = 0;

    int m_iNumFeatures = 0,
        m_iNumObs = 0,
        m_iK = 0,
        m_iMaxIter = 0;

    bool m_bIsInitialized = false;
};


#endif
