#if !defined(__Filter_hdr__)
#define __Filter_hdr__

#include "ErrorDef.h"

#include "Vector.h"
#include "RingBuffer.h"

/*! \brief class for Filter computation with windowing
*/
template <class T>
class CFilter
{
public:
 
    CFilter() {};
    virtual ~CFilter() 
    {
        reset();
    };

    Error_t init(const T* pfB, const T* pfA, int iNumCoeffs)
    {
        if (!pfA || !pfB || iNumCoeffs <= 0)
            return Error_t::kFunctionInvalidArgsError;

        reset();

        // coefficients
        for (auto i = 0; i < kNumFilterDims; i++)
        {
            m_apfCoeff[i] = new T[iNumCoeffs];
            CVector::setZero(m_apfCoeff[i], iNumCoeffs);
        }

        CVector::copy(m_apfCoeff[kFir], pfB, iNumCoeffs);
        CVector::copy(m_apfCoeff[kIir], pfA, iNumCoeffs);

        // internal memory
        m_pCFilterBuffer = new CRingBuffer<T>(iNumCoeffs);
        m_pCFilterBuffer->setWriteIdx(iNumCoeffs - 1);

        m_pfProcBuffer = new T[iNumCoeffs-1];

        m_iNumFilterCoeffs = iNumCoeffs;

        m_bIsInitialized = true;

        return Error_t::kNoError;
    }
    
    /*! resets a Filter instance
    \return Error_t
    */
    Error_t reset()
    {
        if (!m_bIsInitialized)
            return Error_t::kNoError;

        for (auto i = 0; i < kNumFilterDims; i++)
        {
            delete m_apfCoeff[i];
            m_apfCoeff[i] = 0;
        }

        m_iNumFilterCoeffs = 0;

        delete m_pCFilterBuffer;
        m_pCFilterBuffer = 0;

        delete[] m_pfProcBuffer;
        m_pfProcBuffer = 0;

        m_bIsInitialized = false;

        return Error_t::kNoError;
    }
 
    Error_t process(T* pfOut, const T* pfIn, int iNumSamples)
    {
        if (!m_bIsInitialized)
            return Error_t::kFunctionIllegalCallError;
        if (!pfOut || !pfIn || iNumSamples <= 0)
            return Error_t::kFunctionInvalidArgsError;

        for (auto i = 0; i < iNumSamples; i++)
        {
            T fTmp = pfIn[i];

            // get buffer values
            m_pCFilterBuffer->get(m_pfProcBuffer, m_iNumFilterCoeffs - 1);

            // IIR part
            for (auto j = 1; j < m_iNumFilterCoeffs; j++)
                fTmp -= m_apfCoeff[kIir][j] * m_pfProcBuffer[j - 1];

            //put new value into buffer
            m_pCFilterBuffer->putPostInc(fTmp);

            // FIR part
            pfOut[i] = m_apfCoeff[kFir][0] * fTmp;
            for (auto j = 1; j < m_iNumFilterCoeffs; j++)
                pfOut[i] += m_apfCoeff[kFir][j] * m_pfProcBuffer[j - 1];

            // increment read index
            m_pCFilterBuffer->getPostInc();
        }
        return Error_t::kNoError;
    }

private:
    enum FilterCoeffs_t
    {
        kFir,
        kIir,

        kNumFilterDims
    };

    CRingBuffer<T>* m_pCFilterBuffer = 0;
    T* m_apfCoeff[kNumFilterDims] = { 0,0 };
    float *m_pfProcBuffer = 0;

    int m_iNumFilterCoeffs = 0;

    bool m_bIsInitialized = false;
};

#endif // #if !defined(__Filter_hdr__)



