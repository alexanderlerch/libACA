#if !defined(__VectorFloat_hdr__)
#define __VectorFloat_hdr__

#define _USE_MATH_DEFINES
#include <cassert>
#include <cstring>
#include <limits>
#include <cmath>

#include "Synthesis.h"

/*! \brief class with static functions for buffer operations with type T
*/
class CVector
{
public:

    /*! allocates a float buffer and inits it with zeros
    \param ptVec (empty pointer, to be allocated)
    \param iLength number of floats
    \return
    */
    template<typename T>
    static void alloc(T*& ptVec, long long iLength)
    {
        assert(iLength > 0);

        ptVec = new T[iLength];

        assert(ptVec);
        setZero(ptVec, iLength);
    }

    /*! frees a float buffer and sets it to zero
    \param ptVec (empty pointer, to be allocated)
    \return
    */
    template<typename T>
    static void free(T*& ptVec)
    {
        delete[] ptVec;
        ptVec = 0;
    }

    /*! sets a buffer to zero
    \param ptSrcDest pointer to memory to be modified
    \param iLength  buffer length
    \return void
    */
    template<typename T>
    static void setZero (T *ptSrcDest, long long iLength)
    {
        assert (iLength >= 0);
        assert (ptSrcDest);

        if (iLength > 0)
            memset (ptSrcDest, 0, sizeof(T)*iLength);
    }

    /*! initializes the buffer to a specific value
    \param ptDest pointer to memory to be set
    \param tValue value to use
    \param iLength number of elements to be set
    \return void
    */
    template<typename T>
    static void setValue(T* ptDest, T tValue, long long iLength)
    {
        assert(iLength >= 0);
        assert(ptDest);

        for (auto i = 0; i < iLength; i++)
            ptDest[i] = tValue;
    }

    /*! initializes the buffer with random noise
    \param pfDest pointer to memory to be initialized
    \param iLength number of elements to be set
    \return void
    */
    template<typename T>
    static inline void setRand(T* pfDest, long long iLength)
    {
        assert(iLength >= 0);
        assert(pfDest);

        CSynthesis::genNoise(pfDest, iLength);
    }

    /*! sets all values smaller than a threshold to 0
    \param ptSrcDest pointer to memory to be modified
    \param iLength  buffer length
    \param tThresh threshold value
    \return void
    */
    template<typename T>
    static void setZeroBelowThresh (T *ptSrcDest, long long int iLength, T tThresh)
    {
        assert (iLength >= 0);
        assert (ptSrcDest);

        for (auto i = 0; i < iLength; i++)
            if (ptSrcDest[i] < tThresh)
                ptSrcDest[i] = 0;
    }
    /*! copies buffer of type T
    \param ptDest pointer to destination memory
    \param ptSrc pointer to source memory
    \param iLength length of buffer
    \return void
    */
    template<typename T>
    static void copy(T *ptDest, const T *ptSrc, long long int iLength)
    {
        assert(iLength >= 0);

        if (iLength > 0)
        {
            assert(ptDest);
            assert(ptSrc);
            memcpy(ptDest, ptSrc, sizeof(T)*iLength);
        }
    }
    /*! reverses buffer (last to first element)
    \param ptSrcDest pointer to memory to be flipped
    \param iLength number of elements
    \return void
    */
    template<typename T>
    static void flip_I(T *ptSrcDest, long long int iLength)
    {
        assert(iLength >= 0);

        if (iLength > 0)
        {
            assert(ptSrcDest);

            auto iLoopLength = iLength / 2; // integer division!
            for (auto i = 0; i < iLoopLength; i++)
            {
                T Tmp = ptSrcDest[i];
                ptSrcDest[i] = ptSrcDest[iLength - 1 - i];
                ptSrcDest[iLength - 1 - i] = Tmp;
            }
        }
    }
    /*! moves a subset of the current buffer
    \param ptSrcDest source and destination
    \param iDestIdx destination index
    \param iSrcIdx source index
    \param iLength number of elements to be moved
    \return void
    */
    template<typename T>
    static void moveInMem (T *ptSrcDest, int iDestIdx, int iSrcIdx, long long int iLength)
    {
        assert (iLength >= 0);
        assert (ptSrcDest);

        if (iLength > 0)
            memmove (&ptSrcDest[iDestIdx], &ptSrcDest[iSrcIdx], sizeof(T)*iLength);
    }
};

/*! \brief class with static functions for buffer operations with type float
*/
class CVectorFloat
{
public:

    /*! sets a buffer to zero
    \param pfSrcDest pointer to memory to be modified
    \param iLength  buffer length
    \return void
    */
    static inline void setZero (float *pfSrcDest, long long int iLength)
    {
        CVector::setZero(pfSrcDest, iLength);
    }
    
    /*! sets all values smaller than a threshold to 0
    \param pfSrcDest pointer to memory to be modified
    \param iLength  buffer length
    \param fThresh threshold value
    \return void
    */
    static inline void setZeroBelowThresh (float *pfSrcDest, long long int iLength, float fThresh = 0.F)
    {
        CVector::setZeroBelowThresh(pfSrcDest, iLength, fThresh);
    }

    /*! copies buffer of type float
    \param pfDest pointer to destination memory
    \param pfSrc pointer to source memory
    \param iLength length of buffer
    \return void
    */
    static inline void copy(float *pfDest, const float *pfSrc, long long int iLength)
    {
        CVector::copy(pfDest, pfSrc, iLength);
    }

    /*! reverses buffer (last to first element)
    \param pfSrcDest pointer to memory to be flipped
    \param iLength number of elements
    \return void
    */
    static inline void flip_I(float *pfSrcDest, long long int iLength)
    {
        CVector::flip_I(pfSrcDest, iLength);
    }

    /*! moves a subset of the current buffer
    \param pfSrcDest source and destination
    \param iDestIdx destination index
    \param iSrcIdx source index
    \param iLength number of elements to be moved
    \return void
    */
    static inline void moveInMem (float *pfSrcDest, int iDestIdx, int iSrcIdx, long long int iLength)
    {
        CVector::moveInMem(pfSrcDest, iDestIdx, iSrcIdx, iLength);
    }

    /*! initializes the buffer to a specific value
    \param pfDest pointer to memory to be initialized
    \param fValue value to use
    \param iLength number of elements to be set
    \return void
    */
    static inline void setValue(float* pfDest, float fValue, long long int iLength)
    {
        assert(iLength >= 0);
        assert(pfDest);

        for (auto i = 0; i < iLength; i++)
            pfDest[i] = fValue;
    }

    /*! initializes the buffer with random noise
    \param pfDest pointer to memory to be initialized
    \param iLength number of elements to be set
    \return void
    */
    static inline void setRand(float* pfDest, long long int iLength)
    {
        CVector::setRand(pfDest, iLength);
    }

    /*! multiplies a buffer with a scalar
    \param pfSrcDest buffer to be multiplied
    \param fScale scalar
    \param iLength number of element to be multiplied
    \return void
    */
    static inline void mulC_I (float *pfSrcDest, float fScale, long long int iLength)
    {
        assert (iLength >= 0);
        assert (pfSrcDest);
        
        for (auto i = 0; i < iLength; i++)
            pfSrcDest[i] *= fScale;
    }

    /*! element-wise vector multiplication
    \param pfSrcDest one input and output buffer
    \param pfSrc second input buffer
    \param iLength number of element to be multiplied
    \return void
    */
    static inline void mul_I (float *pfSrcDest, const float *pfSrc, long long int iLength)
    {
        assert (iLength >= 0);
        assert (pfSrcDest);
        assert (pfSrc);

        for (auto i = 0; i < iLength; i++)
            pfSrcDest[i] *= pfSrc[i];
    }

    /*! computes the scalar product between two vectors
    \param pfSrc1 vector one
    \param pfSrc2 vector two
    \param iLength number of dimenions
    \return float
    */
    static inline float mulScalar (const float *pfSrc1, const float *pfSrc2, long long int iLength)
    {
        assert (iLength >= 0);
        assert (pfSrc1);
        assert (pfSrc2);
        float  fResult = 0;

        for (auto i = 0; i < iLength; i++)
            fResult += pfSrc1[i] * pfSrc2[i];

        return fResult;
    }

    /*! element-wise vector division
    \param pfSrcDest one input and output buffer
    \param pfSrc second input buffer
    \param iLength number of element to be divided
    \return void
    */
    static inline void div_I (float *pfSrcDest, const float *pfSrc, long long int iLength)
    {
        assert (iLength >= 0);
        assert (pfSrcDest);
        assert (pfSrc);

        for (auto i = 0; i < iLength; i++)
        {
            assert(pfSrc[i] != 0);
            pfSrcDest[i] /= pfSrc[i];
        }
    }

    /*! element-wise vector addition
    \param pfSrcDest one input and output buffer
    \param pfSrc second input buffer
    \param iLength number of element to be added
    \return void
    */
    static inline void add_I (float *pfSrcDest, const float *pfSrc, long long int iLength)
    {
        assert (iLength >= 0);
        assert (pfSrcDest);
        assert (pfSrc);

        for (auto i = 0; i < iLength; i++)
            pfSrcDest[i] += pfSrc[i];
    }

    /*! adds a buffer to a scalar
    \param pfSrcDest buffer to be added
    \param fConst scalar
    \param iLength number of element to be added
    \return void
    */
    static inline void addC_I (float *pfSrcDest, float fConst, long long int iLength)
    {
        assert (iLength >= 0);
        assert (pfSrcDest);

        for (auto i = 0; i < iLength; i++)
            pfSrcDest[i] += fConst;
    }


    /*! sorts values in a buffer 
    \param pfSrcDest buffer to be sorted
    \param piIndices new indices (optional, can be left 0)
    \param iLength number of elements to be sorted
    \param bAscending sort order (descending -> false)
    \return void
    */
    static inline void sort_I(float* pfSrcDest, int* piIndices,  int iLength, bool bAscending = true)
    {
        // go bubble sort! ( should be replaced some time with something faster)

        bool bDone = false; // this flag will be used to check whether we have to continue the algorithm

        // initialize
        if (piIndices)
            for (auto i = 0; i < iLength; i++)
                piIndices[i] = i;

        if (bAscending)
        {
            while (!bDone)
            {
                bDone = true;

                for (auto i = 0; i < iLength-1; i++)
                {
                    if (pfSrcDest[i] > pfSrcDest[i + 1]) // compare the current element with the following one
                    {
                        //swap them
                        CUtil::swap(pfSrcDest[i], pfSrcDest[i + 1]);
                        if (piIndices)
                            CUtil::swap(piIndices[i], piIndices[i + 1]);

                        bDone = false; // let's recheck the array
                    }
                }
            }
        }
        else // descending
        {
            while (!bDone)
            {
                bDone = true;

                for (auto i = 0; i < iLength-1; i++)
                {
                    if (pfSrcDest[i] < pfSrcDest[i + 1]) // compare the current element with the following one
                    {
                        //swap them
                        CUtil::swap(pfSrcDest[i], pfSrcDest[i + 1]);
                        if (piIndices)
                            CUtil::swap(piIndices[i], piIndices[i + 1]);

                        bDone = false; // let's recheck the array
                    }
                }
            }
        }
    }


    /*! element-wise vector subtraction
    \param pfSrcDest one input and output buffer
    \param pfSrc second input buffer
    \param iLength number of element to be subtracted
    \return void
    */
    static inline void sub_I(float* pfSrcDest, const float* pfSrc, long long int iLength)
    {
        assert(iLength >= 0);
        assert(pfSrcDest);
        assert(pfSrc);

        for (auto i = 0; i < iLength; i++)
            pfSrcDest[i] -= pfSrc[i];
    }

    /*! manhattan distance
    \param pfSrc1 first input vector
    \param pfSrc2 second input vector
    \param iLength length of vectors
    \return float
    */
    static inline float distManhattan(const float* pfSrc1, const float* pfSrc2, long long iLength)
    {
        assert(iLength >= 0);
        assert(pfSrc1);
        assert(pfSrc2);

        float fDist = std::abs(pfSrc1[0] - pfSrc2[0]);
        for (auto i = 1; i < iLength; i++)
            fDist += std::abs(pfSrc1[i] - pfSrc2[i]);
        
        return fDist;
    }

    /*! Euclidean distance
    \param pfSrc1 first input vector
    \param pfSrc2 second input vector
    \param iLength length of vectors
    \return float
    */
    static inline float distEuclidean(const float* pfSrc1, const float* pfSrc2, long long iLength)
    {
        assert(iLength >= 0);
        assert(pfSrc1);
        assert(pfSrc2);

        float fDist = -2.F * mulScalar(pfSrc1, pfSrc2, iLength);
        fDist += mulScalar(pfSrc1, pfSrc1, iLength);
        fDist += mulScalar(pfSrc2, pfSrc2, iLength);
        //for (auto i = 0; i < iLength; i++)
        //    fDist += (pfSrc1[i] - pfSrc2[i]) * (pfSrc1[i] - pfSrc2[i]);

        return std::sqrt(fDist);
    }

    /*! Cosine distance
    \param pfSrc1 first input vector
    \param pfSrc2 second input vector
    \param iLength length of vectors
    \return float
    */
    static inline float distCosine(const float* pfSrc1, const float* pfSrc2, long long iLength)
    {
        assert(iLength >= 0);
        assert(pfSrc1);
        assert(pfSrc2);

        float fDist = 0;
        for (auto i = 0; i < iLength; i++)
            fDist += pfSrc1[i] * pfSrc2[i];

        return 1.F - fDist / std::sqrt(mulScalar(pfSrc1, pfSrc1, iLength) * mulScalar(pfSrc2, pfSrc2, iLength));
    }


    /*! weighted element-wise vector subtraction
    \param pfSrcDest one input and output buffer
    \param pfSrc second input buffer
    \param fWeight weight to be applied to pfSrc entries
    \param iLength number of element to be subtracted
    \return void
    */
    static inline void subW_I(float* pfSrcDest, const float* pfSrc, float fWeight, long long int iLength)
    {
        assert(iLength >= 0);
        assert(pfSrcDest);
        assert(pfSrc);

        for (auto i = 0; i < iLength; i++)
            pfSrcDest[i] -= fWeight*pfSrc[i];
    }

    /*! computes the sum of a vector
    \param pfSrc vector
    \param iLength length of vector
    \param bAbs specifies whether it is the sum of absolute values or not
    \return float
    */
    static inline float getSum (const float *pfSrc, long long int iLength, bool bAbs = false)
    {
        assert (iLength >= 0);
        assert (pfSrc);

        float fResult = 0;
        if (bAbs)
        {
            for (auto i = 0; i < iLength; i++)
                fResult += std::abs(pfSrc[i]);
        }
        else
        {
            for (auto i = 0; i < iLength; i++)
                fResult += pfSrc[i];
        }
        return fResult;
    }

    /*! checks to buffer for equality (no floating point tolerance)
    \param pfSrc1 buffer 1
    \param pfSrc2 buffer 2
    \param iLength number of dimensions
    \return bool
    */
    static inline bool isEqual (const float *pfSrc1, const float *pfSrc2, long long int iLength)
    {
        assert (iLength >= 0);
        assert (pfSrc1);
        assert (pfSrc2);

        return (memcmp (pfSrc1, pfSrc2, iLength * sizeof(float)) == 0);
    }

    /*! extracts the mean value
    \param pfSrc input buffer
    \param iLength number of elements in buffer
    \return float
    */
    static inline float getMean (const float *pfSrc, long long int iLength)
    {
        assert (iLength >= 0);

        float fMean = getSum(pfSrc, iLength);

        if (iLength > 0)
        {
            fMean  /= iLength;
        }

        return fMean;
    }

    /*! extracts the standard deviation (biased) from a buffer
    \param pfSrc input buffer
    \param iLength number of elements in buffer
    \param fMean mean value if it has already been computed, otherwise it will be extracted in function
    \return float
    */
    static inline float getStd (const float *pfSrc, long long int iLength, float fMean = std::numeric_limits<float>::max())
    {
        assert (iLength >= 0);

        float  fStd = 0;

        if (fMean == std::numeric_limits<float>::max())
        {
            fMean   = getMean(pfSrc, iLength);
        }

        for (auto i=0; i < iLength; i++)
        {
            fStd   += (pfSrc[i] - fMean) * (pfSrc[i] - fMean);
        }

        if (iLength > 1)
        {
            //dStd   /= (iLength - 1);
            fStd   /= iLength;
        }

        return std::sqrt(fStd);
    }

    /*! extracts the root mean square from a buffer
    \param pfSrc input buffer
    \param iLength number of elements in buffer
    \return float
    */
    static inline float getRms (const float *pfSrc, long long int iLength)
    {
        assert (iLength >= 0);

        float fRms = 0;


        for (auto i=0; i < iLength; i++)
        {
            fRms   += pfSrc[i] * pfSrc[i];
        }

        if (iLength > 0)
        {
            fRms   /= iLength;
        }

        return std::sqrt(fRms);
    }

    /*! finds the maximum (absolute) value in the buffer
    \param pfSrc input buffer
    \param iLength number of elements in buffer
    \param bAbs bool to specify whether we search absolute values
    \return float
    */
    static inline float getMax(const float* pfSrc, long long int iLength, bool bAbs = false)
    {
        float fMax;
        long long iMax;

        findMax(pfSrc, fMax, iMax, iLength, bAbs);

        return fMax;
    }

    /*! finds the local maxima in the buffer
    \param pbIsLocalMax result buffer
    \param pfSrc input buffer
    \param iLength number of elements in buffer
    \param fThresh only detect maxima above this threshold
    \return int number of local maxima
    */
    static inline int findPeaks(bool *pbIsLocalMax, const float* pfSrc, long long int iLength, float fThresh = -std::numeric_limits<float>::max())
    {
        assert(iLength >= 0);
        assert(pfSrc);
        assert(pbIsLocalMax);

        int iNumPeaks = 0;

        CVector::setValue(pbIsLocalMax, false, iLength);

        for (auto k = 1; k < iLength - 1; k++)
        {
            // search for local maxima
            if (pfSrc[k] <= pfSrc[k - 1] || pfSrc[k] <= pfSrc[k + 1] || pfSrc[k] <= fThresh)
                continue;
            else
            {
                pbIsLocalMax[k] = true;
                iNumPeaks++;

                // increment because the next bin cannot be a local max
                k++;
            }
        }

        return iNumPeaks;
    }

    /*! finds the minimum (absolute) value in the buffer
    \param pfSrc input buffer
    \param iLength number of elements in buffer
    \param bAbs bool to specify whether we search absolute values
    \return float
    */
    static inline float getMin (const float *pfSrc, long long int iLength, bool bAbs = false)
    {
        float fMin;
        long long iMin;

        findMin(pfSrc, fMin, iMin, iLength, bAbs);

        return fMin;
    }

    /*! finds the maximum (absolute) value in the buffer
    \param pfSrc input buffer
    \param fMax resulting output value
    \param iMax index of the resulting output value
    \param iLength number of elements in buffer
    \param bAbs bool to specify whether we search absolute values
    \return float
    */
    static inline void findMax (const float *pfSrc, float &fMax, long long &iMax, long long int iLength, bool bAbs = false)
    {
        assert (iLength >= 0);
        assert (pfSrc);

        fMax    = -std::numeric_limits<float>::max();
        iMax    = -1;

        for (auto i = 0; i < iLength; i++)
        {
            float fCurr   = (bAbs)? std::abs(pfSrc[i]) : pfSrc[i];

            if (fCurr > fMax)
            {
                fMax = fCurr;
                iMax = i;
            }
        }
    }

    /*! finds the minimum (absolute) value in the buffer
    \param pfSrc input buffer
    \param fMin resulting output value
    \param iMin index of the resulting output value
    \param iLength number of elements in buffer
    \param bAbs bool to specify whether we search absolute values
    \return float
    */
    static inline void findMin (const float *pfSrc, float &fMin, long long &iMin, long long int iLength, bool bAbs = false)
    {
        assert (iLength >= 0);
        assert (pfSrc);

        fMin    = std::numeric_limits<float>::max();
        iMin    = -1;

        for (auto i = 0; i < iLength; i++)
        {
            float fCurr   = (bAbs)? std::abs(pfSrc[i]) : pfSrc[i];

            if (fCurr < fMin)
            {
                fMin    = fCurr;
                iMin    = i;
            }
        }
    }
};
#endif // __VectorFloat_hdr__