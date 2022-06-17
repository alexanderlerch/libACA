#if !defined(__ACA_Resample_HEADER_INCLUDED__)
#define __ACA_Resample_HEADER_INCLUDED__

#include "ErrorDef.h"

#include "Filter.h"
#include "Util.h"

/*! \brief class for simple interpolation/sample rate conversion
*/
class CResample
{
public:


    /*! simple linear interpolation, static function can be used without instantiation
    \param pfOut buffer with interpolated output samples (user allocated of length iNumOutSamples)
    \param pfNewIdx indication of location of outputs on the input index scale, e.g., if the output sample rate is 3x the input sample rate, this vector will start with 0, 0.33, 0.66, 1, 1.33, ...
    \param pfIn original samples
    \param iNumOutSamples length of pfOut and pfNewIdx
    \param iNumInSamples length of pfIn
    \return Error_t
    */
    static Error_t interp1d(float *pfOut, const float *pfNewIdx, const float *pfIn, long long iNumOutSamples, long long iNumInSamples)
    {
        if (!pfNewIdx || !pfOut || !pfIn || iNumInSamples <= 0 || iNumOutSamples <= 0)
            return Error_t::kFunctionInvalidArgsError;

        long long istart = 0,
            iEnd = iNumOutSamples - 1;

        // treat first samples
        while ((istart < iNumOutSamples) && (pfNewIdx[istart] < 0))
        {
            if (pfNewIdx[istart] < -1)
                pfOut[istart] = 0;
            else
            {
                int     iIdx = static_cast<int>(std::floor(pfNewIdx[istart]));
                float   fFrac = pfNewIdx[istart] - iIdx;
                pfOut[istart] = fFrac * pfIn[0];
            }
            istart++;
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
        for (auto i = istart; i <= iEnd; i++)
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
        assert(m_fInSampleRate > 0);
        assert(m_fOutSampleRate > 0);

        m_pCFilter = new CFilter<float>();
    }

    virtual ~CResample()
    {
        delete m_pCFilter;
    }


    /*! returns the length of the output in case of iNumInSamples as input
    \return long long
    */
    long long getOutputLength(long long iNumInSamples) const
    {
        return CUtil::float2int<long long>(iNumInSamples / m_fInSampleRate * m_fOutSampleRate);
    }

    /*! does the sample rate conversion (filtered linear interpolation)
    \param pfOut output buffer (to be written, user allocated, length see CResample::getOutputLength)
    \param pfIn input buffer of length iNumInSamples
    \param iNumInSamples length of input buffer
    \return Error_t
    */
    Error_t process(float *pfOut, const float *pfIn, long long iNumInSamples)
    {
        if (!pfOut || !pfIn || iNumInSamples <= 0)
            return Error_t::kFunctionInvalidArgsError;

        const int iOrder = 4;
        float fOmegaCutoff = (m_fOutSampleRate > m_fInSampleRate) ? m_fInSampleRate / m_fOutSampleRate : m_fOutSampleRate / m_fInSampleRate;

        // compute index axis
        long long iNumOutSamples = getOutputLength(iNumInSamples);
        float *pfOutIdx = 0;
        CVector::alloc(pfOutIdx, iNumOutSamples);
        for (auto i = 0; i < iNumOutSamples; i++)
            pfOutIdx[i] = i * m_fInSampleRate / m_fOutSampleRate;

        // compute butterworth low pass filter coefficients
        float aafCoeffs[2][iOrder + 1] = { { 0 } };
        CButterLp::calcCoeffs(aafCoeffs[0], aafCoeffs[1], iOrder, .9F * fOmegaCutoff);
        m_pCFilter->init(aafCoeffs[0], aafCoeffs[1], iOrder + 1);

        if (m_fOutSampleRate > m_fInSampleRate) // upsample
        {
            // interpolate
            interp1d(pfOut, pfOutIdx, pfIn, iNumOutSamples, iNumInSamples);

            // apply zero phase filter
            m_pCFilter->filtfilt(pfOut, pfOut, iNumOutSamples);
        }
        else // downsample
        {
            float *pfFiltered = 0;
            CVector::alloc(pfFiltered, iNumInSamples);

            // apply zero phase filter
            m_pCFilter->filtfilt(pfFiltered, pfIn, iNumInSamples);

            // interpolate
            interp1d(pfOut, pfOutIdx, pfFiltered, iNumOutSamples, iNumInSamples);

            CVector::free(pfFiltered);
        }

        // free internal memory
        CVector::free(pfOutIdx);

        return Error_t::kNoError;
    }

private:
    CResample(const CResample &that);
    CResample &operator=(const CResample &c);

    float m_fInSampleRate = 0.F; //!< sample rate of input
    float m_fOutSampleRate = 0.F; //!< sample rate of output

    CFilter<float> *m_pCFilter = 0; //!< low pass filter processing
};

#endif // #if !defined(__ACA_Resample_HEADER_INCLUDED__)
