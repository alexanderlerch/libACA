#if !defined(__LowPass_hdr__)
#define __LowPass_hdr__

#define _USE_MATH_DEFINES
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
    static Error_t create(CSinglePoleLp*& pCInstance)
    {
        pCInstance = new CSinglePoleLp();

        return Error_t::kNoError;
    }

    /*! destroys a SinglePoleLp instance
    \param pCInstance pointer to instance to be destroyed
    \return Error_t
    */
    static Error_t destroy(CSinglePoleLp*& pCInstance)
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
    \param pfOutput filter result (user-allocated, to be written, length iLengthOfBuffer)
    \param pfInput input data of length iLengthOfBuffer
    return Error_t
    */
    Error_t process(float* pfOutput, const float* pfInput, int iLengthOfBuffer)
    {
        if (!pfOutput || !pfInput || iLengthOfBuffer <= 0)
            return Error_t::kFunctionInvalidArgsError;

        pfOutput[0] = (1.F - m_fAlpha) * pfInput[0] + m_fAlpha * m_fPrevOut;
        
        for (auto i = 1; i < iLengthOfBuffer; i++)
            pfOutput[i] = (1 - m_fAlpha) * pfInput[0] + m_fAlpha * pfOutput[i-1];

        m_fPrevOut = pfOutput[iLengthOfBuffer - 1];

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
    CSinglePoleLp(const CSinglePoleLp& that);

    float m_fAlpha = 0.9F;      //!< filter coeff

    float m_fPrevOut = 0.F;     //!< filter buffer
};


/*! \brief class for single-pole low pass filtering
*/
class CMovingAverage
{
public:

    /*! initializes a MovingAverage instance with file reading
    \param pCInstance pointer to instance to be written
    \return Error_t
    */
    static Error_t create(CMovingAverage*& pCInstance)
    {
        pCInstance = new CMovingAverage();

        return Error_t::kNoError;
    }


    /*! destroys a MovingAverage instance
    \param pCInstance pointer to instance to be destroyed
    \return Error_t
    */
    static Error_t destroy(CMovingAverage*& pCInstance)
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
            m_pCRingBuff = new CRingBuffer<float>(iFilterLength+1);
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

    /*! performs the MovingAverage computation
    \param pfOutput filter result (user-allocated, to be written, length iLengthOfBuffer)
    \param pfInput input data of length iLengthOfBuffer
    \return Error_t
    */
    Error_t process(float* pfOutput, const float* pfInput, int iLengthOfBuffer)
    {
        if (!pfOutput || !pfInput || iLengthOfBuffer <= 0)
            return Error_t::kFunctionInvalidArgsError;
        
        int iFilterLength = m_pCRingBuff->getNumValuesInBuffer();

        // recursive implementation - beware of potential error propagation due to numerical precision
        for (auto i = 0; i < iLengthOfBuffer; i++)
        {
            m_fPrevOut += (pfInput[i] - m_pCRingBuff->getPostInc()) / iFilterLength;
            pfOutput[i] = m_fPrevOut;
            m_pCRingBuff->putPostInc(pfInput[i]);
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


private:
    CMovingAverage() 
    {
        reset();
    };
    virtual ~CMovingAverage() 
    {
        delete m_pCRingBuff;
    };
    CMovingAverage(const CMovingAverage& that);

    CRingBuffer<float>* m_pCRingBuff = new CRingBuffer<float>(65);

    float m_fPrevOut = 0;
};


#endif // #if !defined(__LowPass_hdr__)



