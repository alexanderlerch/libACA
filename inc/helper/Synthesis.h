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
    /*! generates a sinusoidal
    \param pfOutBuf output memory buffer (to be written)
    \param fFreqInHz frequency of sinusoidal
    \param fSampleRateInHz sample rate 
    \param iLength number of frames to be generated
    \param fAmplitude amplitude of signal
    \param fStartPhaseInRad starting phase in radiants
    \return Error_t
    */
    static Error_t genSine (float *pfOutBuf, float fFreqInHz, float fSampleRateInHz, int iLength, float fAmplitude = 1.F, float fStartPhaseInRad = 0.F)
    {
        if (!pfOutBuf)
            return Error_t::kFunctionInvalidArgsError;

        for (int i = 0; i < iLength; i++)
        {
            pfOutBuf[i] = fAmplitude * static_cast<float>(sin (2*M_PI*fFreqInHz * i/fSampleRateInHz + fStartPhaseInRad));
        }

        return Error_t::kNoError;
    }

    /*! generates a square wave
    \param pfOutBuf output memory buffer (to be written)
    \param fFreqInHz frequency of sinusoidal
    \param fSampleRateInHz sample rate
    \param iLength number of frames to be generated
    \param fAmplitude amplitude of signal
    \return Error_t
    */
    static Error_t genRect (float *pfOutBuf, float fFreqInHz, float fSampleRateInHz, int iLength, float fAmplitude = 1.F)
    {
        if (!pfOutBuf)
            return Error_t::kFunctionInvalidArgsError;

        float fPeriodLength = fSampleRateInHz / fFreqInHz;
        for (int i = 0; i < iLength; i++)
        {
            // this seems super inefficient
            if (i%CUtil::float2int<int>(fPeriodLength) < .5F*fPeriodLength)
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

    /*! generates a sawtooth
   \param pfOutBuf output memory buffer (to be written)
   \param fFreqInHz frequency of sinusoidal
   \param fSampleRateInHz sample rate
   \param iLength number of frames to be generated
    \param fAmplitude amplitude of signal
   \return Error_t
   */
    static Error_t genSaw (float *pfOutBuf, float fFreqInHz, float fSampleRateInHz, int iLength, float fAmplitude = 1.F)
    {
        if (!pfOutBuf)
            return Error_t::kFunctionInvalidArgsError;

        float fIncr = 2*fAmplitude / fSampleRateInHz * fFreqInHz;
        pfOutBuf[0] = 0;
        for (int i = 1; i < iLength; i++)
        {
            pfOutBuf[i] = fmodf(pfOutBuf[i-1] + fIncr + fAmplitude, 2*fAmplitude) - fAmplitude;
        }

        return Error_t::kNoError;
    }

    /*! generates a dc signal
    \param pfOutBuf output memory buffer (to be written)
    \param iLength number of frames to be generated
    \param fAmplitude amplitude of signal
    \return Error_t
    */
    static Error_t genDc (float *pfOutBuf, int iLength, float fAmplitude = 1.F)
    {
        if (!pfOutBuf)
            return Error_t::kFunctionInvalidArgsError;

        for (int i = 0; i < iLength; i++)
        {
            pfOutBuf[i] = fAmplitude;
        }

        return Error_t::kNoError;
    }

    /*! generates a uniform noise
    \param pfOutBuf output memory buffer (to be written)
    \param iLength number of frames to be generated
    \param fAmplitude max amplitude of noise
    \return Error_t
    */
    static Error_t genNoise (float *pfOutBuf, int iLength, float fAmplitude = 1.F)
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