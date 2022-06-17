#if !defined(__ACA_Util_HEADER_INCLUDED__)
#define __ACA_Util_HEADER_INCLUDED__

#include <cassert>
#include <cstring>
#include <limits>

/*! \brief class with static utility functions
*/
class CUtil
{
public:
    /*! converts a float to an int
    \param fIn float value
    \return T
    */
    template<typename T>
    static T float2int(float fIn)
    {
        if (fIn >= 0.F)
            return static_cast<T>(fIn + .5);
        else
            return static_cast<T>(fIn - .5);
    }
    /*! converts a double to an int
    \param fIn double value
    \return T
    */
    template<typename T>
    static T double2int(double fIn)
    {
        if (fIn >= 0)
            return static_cast<T>(fIn + .5);
        else
            return static_cast<T>(fIn - .5);
    }

    /*! checks if the input is a power of 2
    \param n integer value
    \return bool
    */
    static bool isPowOf2(int n)
    {
        return !(n & (n - 1));
    }

    /*! converts an arbitrary integer (positive) to the next larger power of two
    \param n integer value
    \return int
    */
    static int nextPowOf2(int n)
    {
        int iOrder = 0;

        if (n <= 0)
            return 0;

        while (n >> iOrder)
            iOrder++;

        if (!(n % (1 << (iOrder - 1))))
            iOrder--;

        return (1 << (iOrder));
    }

    /*! swaps two values
    \param tValue1 first value
    \param tValue2 second value
    */
    template<typename T>
    static void swap(T &tValue1, T &tValue2)
    {
        T tTmp = tValue1;

        tValue1 = tValue2;
        tValue2 = tTmp;
    }

    /*! returns the sign of a value
    \param fValue value
    \return float -1, 0, or 1
    */
    static float sign(float fValue)
    {
        if (fValue > 0) return 1.F;
        if (fValue < 0) return -1.F;

        return 0.F;
    }

    /*! returns the sign of a value
    \param iValue value
    \return int -1, 0, or 1
    */
    static int sign(int iValue)
    {
        if (iValue > 0) return 1;
        if (iValue < 0) return -1;

        return 0;
    }
};

#endif // __ACA_Util_HEADER_INCLUDED__
