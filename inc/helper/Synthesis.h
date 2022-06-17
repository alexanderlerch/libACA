#if !defined(__ACA_Synthesis_HEADER_INCLUDED__)
#define __ACA_Synthesis_HEADER_INCLUDED__


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
    \param pfOutBuff output memory buffer (to be written)
    \param fFreqInHz frequency of sinusoidal
    \param fSampleRateInHz sample rate
    \param iLen number of samples to be generated
    \param fAmplitude amplitude of signal
    \param fStartPhaseInRad starting phase in radiants
    \return Error_t
    */
    template <class T>
    static Error_t genSine(T *pfOutBuff, T fFreqInHz, T fSampleRateInHz, long long iLen, T fAmplitude = 1.F, T fStartPhaseInRad = 0.)
    {
        if (!pfOutBuff)
            return Error_t::kFunctionInvalidArgsError;

        for (int i = 0; i < iLen; i++)
        {
            pfOutBuff[i] = fAmplitude * static_cast<T>(sin(2 * M_PI * fFreqInHz * i / fSampleRateInHz + fStartPhaseInRad));
        }

        return Error_t::kNoError;
    }

    /*! generates a square wave
    \param pfOutBuff output memory buffer (to be written)
    \param fFreqInHz frequency of sinusoidal
    \param fSampleRateInHz sample rate
    \param iLen number of samples to be generated
    \param fAmplitude amplitude of signal
    \return Error_t
    */
    template <class T>
    static Error_t genRect(T *pfOutBuff, T fFreqInHz, T fSampleRateInHz, long long iLen, T fAmplitude = 1.)
    {
        if (!pfOutBuff)
            return Error_t::kFunctionInvalidArgsError;

        T fPeriodLength = fSampleRateInHz / fFreqInHz;
        for (int i = 0; i < iLen; i++)
        {
            // this seems super inefficient
            if (i % static_cast<int>(fPeriodLength) < .5 * fPeriodLength)
            {
                pfOutBuff[i] = fAmplitude;
            }
            else
            {
                pfOutBuff[i] = -fAmplitude;
            }
        }

        return Error_t::kNoError;
    }

    /*! generates a sawtooth
   \param pfOutBuff output memory buffer (to be written)
   \param fFreqInHz frequency of sinusoidal
   \param fSampleRateInHz sample rate
   \param iLen number of samples to be generated
    \param fAmplitude amplitude of signal
   \return Error_t
   */
    template <class T>
    static Error_t genSaw(T *pfOutBuff, T fFreqInHz, T fSampleRateInHz, long long iLen, T fAmplitude = 1.)
    {
        if (!pfOutBuff)
            return Error_t::kFunctionInvalidArgsError;

        float fIncr = 2 * fAmplitude / fSampleRateInHz * fFreqInHz;
        pfOutBuff[0] = 0;
        for (int i = 1; i < iLen; i++)
        {
            pfOutBuff[i] = fmodf(pfOutBuff[i - 1] + fIncr + fAmplitude, 2 * fAmplitude) - fAmplitude;
        }

        return Error_t::kNoError;
    }

    /*! generates a dc signal
    \param pfOutBuff output memory buffer (to be written)
    \param iLen number of samples to be generated
    \param fAmplitude amplitude of signal
    \return Error_t
    */
    template <class T>
    static Error_t genDc(T *pfOutBuff, long long iLen, T fAmplitude = 1.)
    {
        if (!pfOutBuff)
            return Error_t::kFunctionInvalidArgsError;

        for (int i = 0; i < iLen; i++)
        {
            pfOutBuff[i] = fAmplitude;
        }

        return Error_t::kNoError;
    }

    /*! generates a uniform noise
    \param pfOutBuff output memory buffer (to be written)
    \param iLen number of samples to be generated
    \param fAmplitude max amplitude of noise
    \param bOnlyPositive create only positive values
    \return Error_t
    */
    template <class T>
    static Error_t genNoise(T *pfOutBuff, long long iLen, T fAmplitude = 1., bool bOnlyPositive = true)
    {
        if (!pfOutBuff)
            return Error_t::kFunctionInvalidArgsError;

        if (bOnlyPositive)
        {
            for (int i = 0; i < iLen; i++)
                pfOutBuff[i] = rand() * fAmplitude / static_cast<float>(RAND_MAX);
        }
        else
        {
            for (int i = 0; i < iLen; i++)
                pfOutBuff[i] = rand() * 2 * fAmplitude / static_cast<float>(RAND_MAX) - 1;
        }
        return Error_t::kNoError;
    }
};

#endif // __ACA_Synthesis_HEADER_INCLUDED__
