#if !defined(__Filter_hdr__)
#define __Filter_hdr__

#include "ErrorDef.h"

#include "Vector.h"
#include "Matrix.h"
#include "RingBuffer.h"

/*! \brief class providing a generic filter implementation
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


    /*! initializes the filter
    \param pfB numerator coefficients 
    \param pfA denominator coefficients (normalized: pfA[0] = 1)
    \param length of both pfB, and pfA
    \return Error_t
    */
    Error_t init(const T* pfB, const T* pfA, int iNumCoeffs)
    {
        if (!pfA || !pfB || iNumCoeffs <= 0)
            return Error_t::kFunctionInvalidArgsError;

        reset();

        // alloc internal coefficients
        for (auto i = 0; i < kNumFilterDims; i++)
        {
            m_aptCoeff[i] = new T[iNumCoeffs];
            CVector::setZero(m_aptCoeff[i], iNumCoeffs);
        }

        // set coefficients
        CVector::copy(m_aptCoeff[kFir], pfB, iNumCoeffs);
        CVector::copy(m_aptCoeff[kIir], pfA, iNumCoeffs);
        m_iNumFilterCoeffs = iNumCoeffs;

        // initialize other internal memory
        m_pCFilterBuffer = new CRingBuffer<T>(iNumCoeffs);
        m_pCFilterBuffer->setWriteIdx(iNumCoeffs - 1);

        m_ptProcBuffer = new T[iNumCoeffs-1];

        m_bIsInitialized = true;

        return Error_t::kNoError;
    }
    
    /*! resets a Filter instance
    \return Error_t
    */
    Error_t reset(bool bFreeMem = true)
    {
        if (!m_bIsInitialized)
            return Error_t::kNoError;

        if (bFreeMem)
        {
            for (auto i = 0; i < kNumFilterDims; i++)
            {
                delete m_aptCoeff[i];
                m_aptCoeff[i] = 0;
            }

            m_iNumFilterCoeffs = 0;

            delete m_pCFilterBuffer;
            m_pCFilterBuffer = 0;

            delete[] m_ptProcBuffer;
            m_ptProcBuffer = 0;

            m_bIsInitialized = false;
        }
        else
        {
            m_pCFilterBuffer->reset();
            m_pCFilterBuffer->setWriteIdx(m_iNumFilterCoeffs - 1);
        }

        return Error_t::kNoError;
    }

    Error_t process(T* pfOut, const T* pfIn, long long iNumSamples)
    {
        if (!m_bIsInitialized)
            return Error_t::kFunctionIllegalCallError;
        if (!pfOut || !pfIn || iNumSamples <= 0)
            return Error_t::kFunctionInvalidArgsError;

        for (auto i = 0; i < iNumSamples; i++)
        {
            T fTmp = pfIn[i];

            // get buffer values
            m_pCFilterBuffer->get(m_ptProcBuffer, m_iNumFilterCoeffs - 1);

            // IIR part
            for (auto j = 1; j < m_iNumFilterCoeffs; j++)
                fTmp -= m_aptCoeff[kIir][j] * m_ptProcBuffer[m_iNumFilterCoeffs - 1 - j];

            //put new value into buffer
            m_pCFilterBuffer->putPostInc(fTmp);

            // FIR part
            pfOut[i] = m_aptCoeff[kFir][0] * fTmp;
            for (auto j = 1; j < m_iNumFilterCoeffs; j++)
                pfOut[i] += m_aptCoeff[kFir][j] * m_ptProcBuffer[m_iNumFilterCoeffs - 1 - j];

            // increment read index
            m_pCFilterBuffer->getPostInc();
        }
        return Error_t::kNoError;
    }

    Error_t filtfilt(T* pfOut, const T* pfIn, long long iNumSamples)
    {
        if (!m_bIsInitialized)
            return Error_t::kFunctionIllegalCallError;
        if (!pfOut || !pfIn || iNumSamples <= 0)
            return Error_t::kFunctionInvalidArgsError;

        // reset filter but don't delete memory
        this->reset(false);

        float* pfTmpBuff = 0;
        CVectorFloat::alloc(pfTmpBuff, iNumSamples);

        // set initial state and process forward path
        this->setInitState();
        this->process(pfTmpBuff, pfIn, iNumSamples);

        // set initial state and process backward path
        this->setInitState();
        for (auto i = iNumSamples - 1; i >= 0; i--)
            this->process(&pfOut[i], &pfTmpBuff[i], 1);

        this->reset(false);

        CVectorFloat::free(pfTmpBuff);

        return Error_t::kNoError;
    }

private:
    CFilter(const CFilter& that);
    CFilter& operator=(const CFilter& c);

    enum FilterCoeffs_t
    {
        kFir,
        kIir,

        kNumFilterDims
    };

    // see sadovsky, bartusek, optimisation of the transient response of a digital filter, radioengineering (9) no 2, 2000
    void setInitState()
    {
        float** ppfA = 0;
        float* pfB = 0;

        int iLengthOfCoeffBuffers = m_iNumFilterCoeffs - 1; 

        CVectorFloat::alloc(pfB, iLengthOfCoeffBuffers);
        CMatrix::alloc(ppfA, iLengthOfCoeffBuffers, iLengthOfCoeffBuffers);
 
        for (auto i = 1; i < m_iNumFilterCoeffs; i++)
            pfB[i - 1] = m_aptCoeff[kFir][i] - m_aptCoeff[kFir][0] * m_aptCoeff[kIir][i];

        for (auto m = 0; m < iLengthOfCoeffBuffers; m++)
        {
            ppfA[m][m] = 1;
            ppfA[m][0] += m_aptCoeff[kIir][m + 1];
            if (m < iLengthOfCoeffBuffers)
                ppfA[m][m + 1] = -1;
        }

        // solve linear equation
        CMatrix::inv_I(ppfA, iLengthOfCoeffBuffers, iLengthOfCoeffBuffers);
        CMatrix::mulMatColVec(pfB, ppfA, pfB, iLengthOfCoeffBuffers, iLengthOfCoeffBuffers);

        // set internal state
        m_pCFilterBuffer->setWriteIdx(0);
        for (auto i = 0; i < iLengthOfCoeffBuffers; i++)
            m_pCFilterBuffer->putPostInc(pfB[i]);

        // clean up
        CMatrix::free(ppfA, iLengthOfCoeffBuffers);
        CVectorFloat::free(pfB);
    }

    CRingBuffer<T>* m_pCFilterBuffer = 0; //!< internal buffer for filter
    
    T* m_aptCoeff[kNumFilterDims] = { 0,0 }; //!< filter coefficients
    
    T *m_ptProcBuffer = 0; //!< temp buffer for processing

    int m_iNumFilterCoeffs = 0; //!< number of filter coefficients

    bool m_bIsInitialized = false; //!< true if initialized
};

#endif // #if !defined(__Filter_hdr__)



