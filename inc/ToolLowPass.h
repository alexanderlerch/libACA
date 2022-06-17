#if !defined(__ACA_LowPass_HEADER_INCLUDED__)
#define __ACA_LowPass_HEADER_INCLUDED__

#include <cmath>

#include "RingBuffer.h"
#include "ErrorDef.h"


/*! \brief class for single-pole low pass filtering
*/
class CSinglePoleLp
{
public:

    /*! initializes a SinglePoleLp instance with file reading
    \param pCInstance pointer to instance to be written
    \return Error_t
    */
    static Error_t create(CSinglePoleLp *&pCInstance)
    {
        pCInstance = new CSinglePoleLp();

        return Error_t::kNoError;
    }

    /*! destroys a SinglePoleLp instance
    \param pCInstance pointer to instance to be destroyed
    \return Error_t
    */
    static Error_t destroy(CSinglePoleLp *&pCInstance)
    {
        delete pCInstance;
        pCInstance = 0;

        return Error_t::kNoError;
    }

    /*! set the filter parameter
    \param fAlpha filter coeff (0 <= fAlpha < 1, the higher the more lowpass)
    \return Error_t
    */
    Error_t setFilterParam(float fAlpha = .9F)
    {
        if (fAlpha < 0 || fAlpha >= 1)
            return Error_t::kFunctionInvalidArgsError;

        m_fAlpha = fAlpha;

        return Error_t::kNoError;
    }

    /*! returns the filter parameter alpha
    \return float
    */
    float getFilterParam() const
    {
        return m_fAlpha;
    }

    /*! performs the SinglePoleLp computation
    \param pfOut filter result (user-allocated, to be written, length iNumSamples)
    \param pfIn input data of length iNumSamples
    \param iNumSamples length of buffers
    return Error_t
    */
    Error_t process(float *pfOut, const float *pfIn, long long iNumSamples)
    {
        if (!pfOut || !pfIn || iNumSamples <= 0)
            return Error_t::kFunctionInvalidArgsError;

        pfOut[0] = (1.F - m_fAlpha) * pfIn[0] + m_fAlpha * m_fPrevOut;

        for (auto i = 1; i < iNumSamples; i++)
            pfOut[i] = (1 - m_fAlpha) * pfIn[0] + m_fAlpha * pfOut[i - 1];

        m_fPrevOut = pfOut[iNumSamples - 1];

        return Error_t::kNoError;
    }

    /*! clears internal buffers and sets parameters to default
    \return Error_t
    */
    Error_t reset()
    {
        m_fAlpha = .9F;
        m_fPrevOut = 0.F;

        return Error_t::kNoError;
    }

    /*! compute the filter coeff from an "integration time"
    \param fIntegrationTimeInS integration time in seconds (the higher, the more low pass)
    \param fSampleRate sample rate in Hz
    return float alpha
    */
    static float calcFilterParam(float fIntegrationTimeInS, float fSampleRate)
    {
        assert(fSampleRate > 0);
        assert(fIntegrationTimeInS >= 0);
        if (fIntegrationTimeInS == 0)
            return 0.F;

        return std::exp(-2.2F / fSampleRate / fIntegrationTimeInS);
    }
private:
    CSinglePoleLp() {};
    virtual ~CSinglePoleLp() {};
    CSinglePoleLp(const CSinglePoleLp &that);
    CSinglePoleLp &operator=(const CSinglePoleLp &c);

    float m_fAlpha = 0.9F; //!< filter coeff

    float m_fPrevOut = 0.F; //!< filter buffer
};


/*! \brief class for moving average low pass filtering
*/
class CMovingAverage
{
public:

    /*! initializes a MovingAverage instance with file reading
    \param pCInstance pointer to instance to be written
    \return Error_t
    */
    static Error_t create(CMovingAverage *&pCInstance)
    {
        pCInstance = new CMovingAverage();

        return Error_t::kNoError;
    }


    /*! destroys a MovingAverage instance
    \param pCInstance pointer to instance to be destroyed
    \return Error_t
    */
    static Error_t destroy(CMovingAverage *&pCInstance)
    {
        delete pCInstance;
        pCInstance = 0;

        return Error_t::kNoError;
    }

    /*! sets the length of the filter
    \param iFilterLength length of the moving average filter (the longer the more lowpass)
    \return int
    */
    Error_t setFilterParam(int iFilterLength = 64)
    {
        if (iFilterLength <= 0)
            return Error_t::kFunctionInvalidArgsError;

        if (m_pCRingBuff->getLength() < iFilterLength)
        {
            delete m_pCRingBuff;
            m_pCRingBuff = new CRingBuffer<float>(iFilterLength + 1);
        }
        m_pCRingBuff->setReadIdx(-iFilterLength);

        return Error_t::kNoError;
    }

    /*! returns the filter length
    \return int
    */
    int getFilterParam() const
    {
        return m_pCRingBuff->getNumValuesInBuffer();
    }

    /*! compute the filter length from an "integration time"
    \param fIntegrationTimeInS integration time in seconds (the higher, the more low pass)
    \param fSampleRate sample rate in Hz
    return int number of coeffs
    */
    static int calcFilterLength(float fIntegrationTimeInS, float fSampleRate)
    {
        assert(fSampleRate > 0);
        assert(fIntegrationTimeInS >= 0);
        if (fIntegrationTimeInS <= 1.5F / fSampleRate)
            return 1;

        return static_cast<int>(std::ceil(fIntegrationTimeInS * fSampleRate));
    }

    /*! performs the MovingAverage computation
    \param pfOut filter result (user-allocated, to be written, length iLenBuff)
    \param pfIn input data of length iLenBuff
    \param iLenBuff length of buffer in samples
    \return Error_t
    */
    Error_t process(float *pfOut, const float *pfIn, long long iLenBuff)
    {
        if (!pfOut || !pfIn || iLenBuff <= 0)
            return Error_t::kFunctionInvalidArgsError;

        int iLenFilter = m_pCRingBuff->getNumValuesInBuffer();

        // recursive implementation - beware of potential error propagation due to numerical precision
        for (auto i = 0; i < iLenBuff; i++)
        {
            m_fPrevOut += (pfIn[i] - m_pCRingBuff->getPostInc()) / iLenFilter;
            pfOut[i] = m_fPrevOut;
            m_pCRingBuff->putPostInc(pfIn[i]);
        }

        return Error_t::kNoError;
    }

    /*! clears internal buffers and sets parameters to default
    \return Error_t
    */
    Error_t reset()
    {
        m_pCRingBuff->reset();
        m_pCRingBuff->setReadIdx(-64);
        m_fPrevOut = 0;

        return Error_t::kNoError;
    }

    /*! performs zero-phase filtering with the MA
    \param pfOut filter result (user-allocated, to be written, length iLenBuff)
    \param pfIn input data of length iLenBuff
    \param iLenBuff length of buffer in samples
    */
    void filtfilt(float *pfOut, const float *pfIn, long long iLenBuff)
    {

        int iLenFilter = this->getFilterParam();
        this->reset();
        this->setFilterParam(iLenFilter);

        float *pfTmpBuff = 0;
        CVector::alloc(pfTmpBuff, iLenBuff + 2 * static_cast<long long>(iLenFilter));

        this->process(&pfTmpBuff[iLenFilter], pfIn, iLenBuff);

        // tail
        for (auto i = 0; i < iLenFilter; i++)
        {
            float fZero = 0.F;
            this->process(&pfTmpBuff[iLenBuff + iLenFilter + i], &fZero, 1);
        }

        // reverse tail
        for (auto i = iLenBuff + 2 * static_cast<long long>(iLenFilter) - 1; i >= iLenBuff + iLenFilter; i--)
        {
            float fZero = 0.F;
            this->process(&fZero, &pfTmpBuff[i], 1);
        }

        for (auto i = iLenBuff + iLenFilter - 1; i >= iLenFilter; i--)
            this->process(&pfOut[i - iLenFilter], &pfTmpBuff[i], 1);

        this->reset();
        this->setFilterParam(iLenFilter);

        CVector::free(pfTmpBuff);
    }


private:
    CMovingAverage()
    {
        reset();
    };

    virtual ~CMovingAverage()
    {
        delete m_pCRingBuff;
    };
    CMovingAverage(const CMovingAverage &that);
    CMovingAverage &operator=(const CMovingAverage &c);

    CRingBuffer<float> *m_pCRingBuff = new CRingBuffer<float>(65); //!< filter buffer

    float m_fPrevOut = 0; //!< previous output value
};

#endif // #if !defined(__ACA_LowPass_HEADER_INCLUDED__)
