#if !defined(__Synthesis_hdr__)
#define __Synthesis_hdr__

#define _USE_MATH_DEFINES
#include <cmath>
#include <cstdlib>

#include "ErrorDef.h"
#include "Util.h"

/*! \brief class with static functions for signal generation
*/
class CSynthesis
{
public:
    static Error_t generateSine (float *pfOutBuf, float fFreqInHz, float fSampleFreqInHz, int iLength, float fAmplitude = 1.F, float fStartPhaseInRad = 0.F)
    {
        if (!pfOutBuf)
            return Error_t::kFunctionInvalidArgsError;

        for (int i = 0; i < iLength; i++)
        {
            pfOutBuf[i] = fAmplitude * static_cast<float>(sin (2*M_PI*fFreqInHz * i/fSampleFreqInHz + fStartPhaseInRad));
        }

        return Error_t::kNoError;
    }
    static Error_t generateRect (float *pfOutBuf, float fFreqInHz, float fSampleFreqInHz, int iLength, float fAmplitude = 1.F)
    {
        if (!pfOutBuf)
            return Error_t::kFunctionInvalidArgsError;

        float fPeriodLength = fSampleFreqInHz / fFreqInHz;
        for (int i = 0; i < iLength; i++)
        {
            // this seems super inefficient
            if (i%CUtil::float2int<int>(fPeriodLength) <= .5*fPeriodLength)
            {            
                pfOutBuf[i] = fAmplitude;
            }
            else
            {
                pfOutBuf[i] = -fAmplitude;
            }
        }

        return Error_t::kNoError;
    }
    static Error_t generateSaw (float *pfOutBuf, float fFreqInHz, float fSampleFreqInHz, int iLength, float fAmplitude = 1.F)
    {
        if (!pfOutBuf)
            return Error_t::kFunctionInvalidArgsError;

        float fIncr = 2*fAmplitude / fSampleFreqInHz * fFreqInHz;
        pfOutBuf[0] = 0;
        for (int i = 1; i < iLength; i++)
        {
            pfOutBuf[i] = fmodf(pfOutBuf[i-1] + fIncr + fAmplitude, 2*fAmplitude) - fAmplitude;
        }

        return Error_t::kNoError;
    }
    static Error_t generateDc (float *pfOutBuf, int iLength, float fAmplitude = 1.F)
    {
        if (!pfOutBuf)
            return Error_t::kFunctionInvalidArgsError;

        for (int i = 0; i < iLength; i++)
        {
            pfOutBuf[i] = fAmplitude;
        }

        return Error_t::kNoError;
    }
    static Error_t generateNoise (float *pfOutBuf, int iLength, float fAmplitude = 1.F)
    {
        if (!pfOutBuf)
            return Error_t::kFunctionInvalidArgsError;

        for (int i = 0; i < iLength; i++)
        {
            pfOutBuf[i] = rand()*fAmplitude/RAND_MAX;
        }

        return Error_t::kNoError;
    }
};
#endif // __Synthesis_hdr__