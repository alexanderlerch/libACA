#if !defined(__Util_hdr__)
#define __Util_hdr__

#include <cassert>
#include <cstring>
#include <limits>

/*! \brief class with static utility functions 
*/
class CUtil
{
public:
    /*! converts a float to an int
    \param fInput float value
    \return T
    */
    template<typename T>
    static T float2int (float fInput)
    {
        if (fInput >= 0.F)
            return static_cast<T>(fInput + .5F);
        else
            return static_cast<T>(fInput - .5F);
    }
    /*! converts a double to an int
    \param fInput double value
    \return T
    */
    template<typename T>
    static T double2int (double fInput)
    {
        if (fInput >= 0)
            return static_cast<T>(fInput + .5);
        else
            return static_cast<T>(fInput - .5);
    }

    /*! checks if the input is a power of 2
    \param n integer value
    \return bool
    */
    static bool isPowOf2 (int n) 
    {
        return !(n & (n-1));
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

        while (n>>iOrder)
            iOrder++;

        if (!(n%(1<<(iOrder-1))))
            iOrder--;

        return (1<<(iOrder));
    }

    template<typename T>
    static void swap (T &tValue1, T &tValue2)
    {
        T tTmp = tValue1;

        tValue1 = tValue2;
        tValue2 = tTmp;
    }
};
#endif // __Util_hdr__