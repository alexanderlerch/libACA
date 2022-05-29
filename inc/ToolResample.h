#if !defined(__Resample_hdr__)
#define __Resample_hdr__

#include "ErrorDef.h"


/*! \brief class for simple interpolation/sample rate conversion
*/
class CResample
{
public:

    static Error_t interp1d(float* pfNewIdx, float* pfOut, const float* pfIn, long long iNumOutSamples, long long iNumInSamples)
    {
        if (!pfNewIdx || !pfOut || !pfIn || iNumInSamples <= 0 || iNumOutSamples <= 0)
            return Error_t::kFunctionInvalidArgsError;

        long long iStart = 0,
            iEnd = iNumOutSamples - 1;

        // treat first samples
        while ((pfNewIdx[iStart] < 0) && (iStart < iNumOutSamples))
        {
            if (pfNewIdx[iStart] < -1)
                pfOut[iStart] = 0;
            else
            {
                int     iIdx = static_cast<int>(std::floor(pfNewIdx[iStart]));
                float   fFrac = pfNewIdx[iStart] - iIdx;
                pfOut[iStart] = fFrac * pfIn[0];
            }
            iStart++;
        }

        // treat last samples
        while ((pfNewIdx[iEnd] > iNumInSamples - 1) && (iEnd >= 0))
        {
            if (pfNewIdx[iEnd] > iNumInSamples)
                pfOut[iEnd] = 0;
            else
            {
                int     iIdx = static_cast<int>(std::floor(pfNewIdx[iEnd]));
                float   fFrac = pfNewIdx[iEnd] - iIdx;
                pfOut[iEnd] = (1 - fFrac) * pfIn[iNumInSamples - 1];
            }
            iEnd--;
        }

        // now go through the buffer
        for (auto i = iStart; i <= iEnd; i++)
        {
            int     iIdx = static_cast<int>(std::floor(pfNewIdx[i]));
            float   fFrac = pfNewIdx[i] - iIdx;

            pfOut[i] = (1 - fFrac) * pfIn[iIdx] + fFrac * pfIn[iIdx + 1];
        }

        return Error_t::kNoError;
    }

    explicit CResample(float fInSampleRate, float fOutSampleRate) :
        m_fInSampleRate(fInSampleRate),
        m_fOutSampleRate(fOutSampleRate)
    {
    }

    virtual ~CResample() 
    {
    }

    Error_t process(float *pfOut, const float *pfIn)
    {
        if (!pfOut || !pfIn)
            return Error_t::kFunctionInvalidArgsError;

        return Error_t::kNoError;
    }

private:
    CResample(const CResample& that);
    CResample& operator=(const CResample& c);

    float m_fInSampleRate = 0.F;
    float m_fOutSampleRate = 0.F;
};




#endif // #if !defined(__Resample_hdr__)



