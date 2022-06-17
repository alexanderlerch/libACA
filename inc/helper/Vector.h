#if !defined(__ACA_Vector_HEADER_INCLUDED__)
#define __ACA_Vector_HEADER_INCLUDED__


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

    //////////////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////////////
    /*! allocates a T buffer and inits it with zeros
    \param ptVec (empty pointer, to be allocated)
    \param iLength number of Ts
    */
    template<typename T>
    static inline void alloc(T *&ptVec, long long iLength)
    {
        assert(iLength > 0);

        ptVec = new T[iLength];

        assert(ptVec);
        setZero(ptVec, iLength);
    }

    /*! copies buffer of type T
    \param ptDest pointer to destination memory
    \param ptSrc pointer to source memory
    \param iLength length of buffer
    */
    template<typename T>
    static inline void copy(T *ptDest, const T *ptSrc, long long int iLength)
    {
        assert(iLength >= 0);

        if (iLength > 0)
        {
            assert(ptDest);
            assert(ptSrc);
            memcpy(ptDest, ptSrc, sizeof(T) * iLength);
        }
    }

    /*! frees a T buffer and sets it to zero
    \param ptVec (empty pointer, to be allocated)
    */
    template<typename T>
    static inline void free(T *&ptVec)
    {
        delete[] ptVec;
        ptVec = 0;
    }

    /*! moves a subset of the current buffer
    \param ptSrcDest source and destination
    \param iDestIdx destination index
    \param isrcIdx source index
    \param iLength number of elements to be moved
    */
    template<typename T>
    static inline void moveInMem(T *ptSrcDest, int iDestIdx, int isrcIdx, long long int iLength)
    {
        assert(iLength >= 0);
        assert(ptSrcDest);

        if (iLength > 0)
            memmove(&ptSrcDest[iDestIdx], &ptSrcDest[isrcIdx], sizeof(T) * iLength);
    }

    //////////////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////////////
    /*! initializes the buffer with random noise
    \param ptDest pointer to memory to be initialized
    \param iLength number of elements to be set
    */
    template<typename T>
    static inline void setRand(T *ptDest, long long iLength)
    {
        assert(iLength >= 0);
        assert(ptDest);

        CSynthesis::genNoise(ptDest, iLength);
    }

    /*! initializes the buffer to a specific value
    \param ptDest pointer to memory to be set
    \param tValue value to use
    \param iLength number of elements to be set
    */
    template<typename T>
    static inline void setValue(T *ptDest, T tValue, long long iLength)
    {
        assert(iLength >= 0);
        assert(ptDest);

        for (auto i = 0; i < iLength; i++)
            ptDest[i] = tValue;
    }

    /*! sets a buffer to zero
    \param ptSrcDest pointer to memory to be modified
    \param iLength  buffer length
    */
    template<typename T>
    static inline void setZero(T *ptSrcDest, long long iLength)
    {
        assert(iLength >= 0);
        assert(ptSrcDest);

        if (iLength > 0)
            memset(ptSrcDest, 0, sizeof(T) * iLength);
    }

    /*! sets all values smaller than a threshold to 0
    \param ptSrcDest pointer to memory to be modified
    \param iLength  buffer length
    \param tThresh threshold value
    */
    template<typename T>
    static inline void setZeroBelowThresh(T *ptSrcDest, long long int iLength, T tThresh)
    {
        assert(iLength >= 0);
        assert(ptSrcDest);

        for (auto i = 0; i < iLength; i++)
            if (ptSrcDest[i] < tThresh)
                ptSrcDest[i] = 0;
    }

    //////////////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////////////
    /*! element-wise vector addition
    \param ptSrcDest one input and output buffer
    \param ptSrc second input buffer
    \param iLength number of element to be added
    */
    template<typename T>
    static inline void add_I(T *ptSrcDest, const T *ptSrc, long long int iLength)
    {
        assert(iLength >= 0);
        assert(ptSrcDest);
        assert(ptSrc);

        for (auto i = 0; i < iLength; i++)
            ptSrcDest[i] += ptSrc[i];
    }

    /*! adds a buffer to a scalar
    \param ptSrcDest buffer to be added
    \param fConst scalar
    \param iLength number of element to be added
    */
    template<typename T>
    static inline void addC_I(T *ptSrcDest, T fConst, long long int iLength)
    {
        assert(iLength >= 0);
        assert(ptSrcDest);

        for (auto i = 0; i < iLength; i++)
            ptSrcDest[i] += fConst;
    }

    /*! weighted element-wise vector addition
    \param ptSrcDest one input and output buffer
    \param ptSrc second input buffer
    \param fWeight weight to be applied to ptSrc entries
    \param iLength number of elements to be added
    */
    template<typename T>
    static inline void addW_I(T *ptSrcDest, const T *ptSrc, T fWeight, long long int iLength)
    {
        assert(iLength >= 0);
        assert(ptSrcDest);
        assert(ptSrc);

        for (auto i = 0; i < iLength; i++)
            ptSrcDest[i] += fWeight * ptSrc[i];
    }

    /*! element-wise vector division
    \param ptSrcDest one input and output buffer
    \param ptSrc second input buffer
    \param iLength number of element to be divided
    */
    template<typename T>
    static inline void div_I(T *ptSrcDest, const T *ptSrc, long long int iLength)
    {
        assert(iLength >= 0);
        assert(ptSrcDest);
        assert(ptSrc);

        for (auto i = 0; i < iLength; i++)
        {
            assert(ptSrc[i] != 0);
            ptSrcDest[i] /= ptSrc[i];
        }
    }

    /*! multiplies a buffer with a scalar
    \param ptSrcDest buffer to be multiplied
    \param fScale scalar
    \param iLength number of element to be multiplied
    */
    template<typename T>
    static inline void mulC_I(T *ptSrcDest, T fScale, long long int iLength)
    {
        assert(iLength >= 0);
        assert(ptSrcDest);

        for (auto i = 0; i < iLength; i++)
            ptSrcDest[i] *= fScale;
    }

    /*! element-wise vector multiplication
    \param ptSrcDest one input and output buffer
    \param ptSrc second input buffer
    \param iLength number of element to be multiplied
    */
    template<typename T>
    static inline void mul_I(T *ptSrcDest, const T *ptSrc, long long int iLength)
    {
        assert(iLength >= 0);
        assert(ptSrcDest);
        assert(ptSrc);

        for (auto i = 0; i < iLength; i++)
            ptSrcDest[i] *= ptSrc[i];
    }

    /*! computes the scalar product between two vectors
    \param ptSrc1 vector one
    \param ptSrc2 vector two
    \param iLength number of dimenions
    \return T
    */
    template<typename T>
    static inline T mulScalar(const T *ptSrc1, const T *ptSrc2, long long int iLength)
    {
        assert(iLength >= 0);
        assert(ptSrc1);
        assert(ptSrc2);
        T  fResult = 0;

        for (auto i = 0; i < iLength; i++)
            fResult += ptSrc1[i] * ptSrc2[i];

        return fResult;
    }

    /*! element-wise vector subtraction
    \param ptSrcDest one input and output buffer
    \param ptSrc second input buffer
    \param iLength number of element to be subtracted
    */
    template<typename T>
    static inline void sub_I(T *ptSrcDest, const T *ptSrc, long long int iLength)
    {
        assert(iLength >= 0);
        assert(ptSrcDest);
        assert(ptSrc);

        for (auto i = 0; i < iLength; i++)
            ptSrcDest[i] -= ptSrc[i];
    }

    /*! weighted element-wise vector subtraction
    \param ptSrcDest one input and output buffer
    \param ptSrc second input buffer
    \param fWeight weight to be applied to ptSrc entries
    \param iLength number of element to be subtracted
    */
    template<typename T>
    static inline void subW_I(T *ptSrcDest, const T *ptSrc, T fWeight, long long int iLength)
    {
        addW_I(ptSrcDest, ptSrc, -fWeight, iLength);
    }

    //////////////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////////////
    /*! reverses buffer (last to first element)
    \param ptSrcDest pointer to memory to be flipped
    \param iLength number of elements
    */
    template<typename T>
    static inline void flip_I(T *ptSrcDest, long long int iLength)
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

    /*! sorts values in a buffer
    \param ptSrcDest buffer to be sorted
    \param piIndices new indices (optional, can be left 0)
    \param iLength number of elements to be sorted
    \param bAscending sort order (descending -> false)
    */
    template<typename T>
    static inline void sort_I(T *ptSrcDest, int *piIndices, int iLength, bool bAscending = true)
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

                for (auto i = 0; i < iLength - 1; i++)
                {
                    if (ptSrcDest[i] > ptSrcDest[i + 1]) // compare the current element with the following one
                    {
                        //swap them
                        CUtil::swap(ptSrcDest[i], ptSrcDest[i + 1]);
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

                for (auto i = 0; i < iLength - 1; i++)
                {
                    if (ptSrcDest[i] < ptSrcDest[i + 1]) // compare the current element with the following one
                    {
                        //swap them
                        CUtil::swap(ptSrcDest[i], ptSrcDest[i + 1]);
                        if (piIndices)
                            CUtil::swap(piIndices[i], piIndices[i + 1]);

                        bDone = false; // let's recheck the array
                    }
                }
            }
        }
    }

    //////////////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////////////
    /*! Cosine distance
    \param ptSrc1 first input vector
    \param ptSrc2 second input vector
    \param iLength length of vectors
    \return T
    */
    template<typename T>
    static inline T distCosine(const T *ptSrc1, const T *ptSrc2, long long iLength)
    {
        assert(iLength >= 0);
        assert(ptSrc1);
        assert(ptSrc2);

        T fDist = 0;
        for (auto i = 0; i < iLength; i++)
            fDist += ptSrc1[i] * ptSrc2[i];

        return 1.F - fDist / std::sqrt(mulScalar(ptSrc1, ptSrc1, iLength) * mulScalar(ptSrc2, ptSrc2, iLength));
    }


    /*! Euclidean distance
    \param ptSrc1 first input vector
    \param ptSrc2 second input vector
    \param iLength length of vectors
    \return T
    */
    template<typename T>
    static inline T distEuclidean(const T *ptSrc1, const T *ptSrc2, long long iLength)
    {
        assert(iLength >= 0);
        assert(ptSrc1);
        assert(ptSrc2);

        T fDist = -2.F * mulScalar(ptSrc1, ptSrc2, iLength);
        fDist += mulScalar(ptSrc1, ptSrc1, iLength);
        fDist += mulScalar(ptSrc2, ptSrc2, iLength);

        return std::sqrt(fDist);
    }

    /*! manhattan distance
    \param ptSrc1 first input vector
    \param ptSrc2 second input vector
    \param iLength length of vectors
    \return T
    */
    template<typename T>
    static inline T distManhattan(const T *ptSrc1, const T *ptSrc2, long long iLength)
    {
        assert(iLength >= 0);
        assert(ptSrc1);
        assert(ptSrc2);

        T fDist = std::abs(ptSrc1[0] - ptSrc2[0]);
        for (auto i = 1; i < iLength; i++)
            fDist += std::abs(ptSrc1[i] - ptSrc2[i]);

        return fDist;
    }

    //////////////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////////////
    /*! finds the maximum (absolute) value in the buffer
    \param ptSrc input buffer
    \param iLength number of elements in buffer
    \param bAbs bool to specify whether we search absolute values
    \return T
    */
    template<typename T>
    static inline T getMax(const T *ptSrc, long long int iLength, bool bAbs = false)
    {
        T fMax = 0;
        long long iMax = -1;

        findMax(ptSrc, fMax, iMax, iLength, bAbs);

        return fMax;
    }

    /*! extracts the mean value
    \param ptSrc input buffer
    \param iLength number of elements in buffer
    \return T
    */
    template<typename T>
    static inline T getMean(const T *ptSrc, long long int iLength)
    {
        assert(iLength >= 0);

        T fMean = getSum(ptSrc, iLength);

        if (iLength > 0)
        {
            fMean /= iLength;
        }

        return fMean;
    }

    /*! finds the minimum (absolute) value in the buffer
    \param ptSrc input buffer
    \param iLength number of elements in buffer
    \param bAbs bool to specify whether we search absolute values
    \return T
    */
    template<typename T>
    static inline T getMin(const T *ptSrc, long long int iLength, bool bAbs = false)
    {
        T fMin = 0;
        long long iMin = -1;

        findMin(ptSrc, fMin, iMin, iLength, bAbs);

        return fMin;
    }

    /*! extracts the root mean square from a buffer
    \param ptSrc input buffer
    \param iLength number of elements in buffer
    \return T
    */
    template<typename T>
    static inline T getRms(const T *ptSrc, long long int iLength)
    {
        assert(iLength >= 0);

        T fRms = 0;


        for (auto i = 0; i < iLength; i++)
        {
            fRms += ptSrc[i] * ptSrc[i];
        }

        if (iLength > 0)
        {
            fRms /= iLength;
        }

        return std::sqrt(fRms);
    }

    /*! extracts the standard deviation (biased) from a buffer
    \param ptSrc input buffer
    \param iLength number of elements in buffer
    \param fMean mean value if it has already been computed, otherwise it will be extracted in function
    \return T
    */
    template<typename T>
    static inline T getStd(const T *ptSrc, long long int iLength, T fMean = std::numeric_limits<T>::max())
    {
        assert(iLength >= 0);

        T  fStd = 0;

        if (fMean == std::numeric_limits<T>::max())
        {
            fMean = getMean(ptSrc, iLength);
        }

        for (auto i = 0; i < iLength; i++)
        {
            fStd += (ptSrc[i] - fMean) * (ptSrc[i] - fMean);
        }

        if (iLength > 1)
        {
            //dStd   /= (iLength - 1);
            fStd /= iLength;
        }

        return std::sqrt(fStd);
    }

    /*! computes the sum of a vector
    \param ptSrc vector
    \param iLength length of vector
    \param bAbs specifies whether it is the sum of absolute values or not
    \return T
    */
    template<typename T>
    static inline T getSum(const T *ptSrc, long long int iLength, bool bAbs = false)
    {
        assert(iLength >= 0);
        assert(ptSrc);

        T fResult = 0;
        if (bAbs)
        {
            for (auto i = 0; i < iLength; i++)
                fResult += std::abs(ptSrc[i]);
        }
        else
        {
            for (auto i = 0; i < iLength; i++)
                fResult += ptSrc[i];
        }
        return fResult;
    }

    //////////////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////////////
    /*! finds the maximum (absolute) value in the buffer
    \param ptSrc input buffer
    \param fMax resulting output value
    \param iMax index of the resulting output value
    \param iLength number of elements in buffer
    \param bAbs bool to specify whether we search absolute values
    */
    template<typename T>
    static inline void findMax(const T *ptSrc, T &fMax, long long &iMax, long long int iLength, bool bAbs = false)
    {
        assert(iLength >= 0);
        assert(ptSrc);

        fMax = -std::numeric_limits<T>::max();
        iMax = -1;

        for (auto i = 0; i < iLength; i++)
        {
            T fCurr = (bAbs) ? std::abs(ptSrc[i]) : ptSrc[i];

            if (fCurr > fMax)
            {
                fMax = fCurr;
                iMax = i;
            }
        }
    }

    /*! finds the minimum (absolute) value in the buffer
    \param ptSrc input buffer
    \param fMin resulting output value
    \param iMin index of the resulting output value
    \param iLength number of elements in buffer
    \param bAbs bool to specify whether we search absolute values
    */
    template<typename T>
    static inline void findMin(const T *ptSrc, T &fMin, long long &iMin, long long int iLength, bool bAbs = false)
    {
        assert(iLength >= 0);
        assert(ptSrc);

        fMin = std::numeric_limits<T>::max();
        iMin = -1;

        for (auto i = 0; i < iLength; i++)
        {
            T fCurr = (bAbs) ? std::abs(ptSrc[i]) : ptSrc[i];

            if (fCurr < fMin)
            {
                fMin = fCurr;
                iMin = i;
            }
        }
    }
    /*! finds the local maxima in the buffer
     \param pbisLocalMax result buffer
     \param ptSrc input buffer
     \param iLength number of elements in buffer
     \param fThresh only detect maxima above this threshold
     \return int number of local maxima
     */
    template<typename T>
    static inline int findPeaks(bool *pbisLocalMax, const T *ptSrc, long long int iLength, T fThresh = -std::numeric_limits<T>::max())
    {
        assert(iLength >= 0);
        assert(ptSrc);
        assert(pbisLocalMax);

        int iNumPeaks = 0;

        CVector::setValue(pbisLocalMax, false, iLength);

        for (auto k = 1; k < iLength - 1; k++)
        {
            // search for local maxima
            if (ptSrc[k] <= ptSrc[k - 1] || ptSrc[k] <= ptSrc[k + 1] || ptSrc[k] <= fThresh)
                continue;
            else
            {
                pbisLocalMax[k] = true;
                iNumPeaks++;

                // increment because the next bin cannot be a local max
                k++;
            }
        }

        return iNumPeaks;
    }

    //////////////////////////////////////////////////////////////////////////////
    /*! checks to buffer for equality (no floating point tolerance)
    \param ptSrc1 buffer 1
    \param ptSrc2 buffer 2
    \param iLength number of dimensions
    \return bool
    */
    template<typename T>
    static inline bool isEqual(const T *ptSrc1, const T *ptSrc2, long long int iLength)
    {
        assert(iLength >= 0);
        assert(ptSrc1);
        assert(ptSrc2);

        return (memcmp(ptSrc1, ptSrc2, iLength * sizeof(T)) == 0);
    }

};

#endif // __ACA_Vector_HEADER_INCLUDED__
