#if !defined(__ACA_Filter_HEADER_INCLUDED__)
#define __ACA_Filter_HEADER_INCLUDED__

#include <cmath>

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
    \param iNumCoeffs length of both pfB, and pfA
    \return Error_t
    */
    Error_t init(const T *pfB, const T *pfA, int iNumCoeffs)
    {
        if (!pfA || !pfB || iNumCoeffs <= 0)
            return Error_t::kFunctionInvalidArgsError;

        reset();

        // alloc internal coefficients
        for (auto i = 0; i < kNumFilterDims; i++)
        {
            CVector::alloc(m_aptCoeff[i], iNumCoeffs);
            CVector::setZero(m_aptCoeff[i], iNumCoeffs);
        }

        // set coefficients
        CVector::copy(m_aptCoeff[kFir], pfB, iNumCoeffs);
        CVector::copy(m_aptCoeff[kIir], pfA, iNumCoeffs);
        m_iNumFilterCoeffs = iNumCoeffs;

        // initialize other internal memory
        m_pCFilterBuff = new CRingBuffer<T>(iNumCoeffs);
        m_pCFilterBuff->setWriteIdx(static_cast<long long>(iNumCoeffs - 1));

        CVector::alloc(m_ptProcBuff, static_cast<long long>(iNumCoeffs) - 1);

        m_bisInitialized = true;

        return Error_t::kNoError;
    }

    /*! resets a Filter instance
    \return Error_t
    */
    Error_t reset(bool bFreeMem = true)
    {
        if (!m_bisInitialized)
            return Error_t::kNoError;

        if (bFreeMem)
        {
            for (auto i = 0; i < kNumFilterDims; i++)
                CVector::free(m_aptCoeff[i]);

            m_iNumFilterCoeffs = 0;

            delete m_pCFilterBuff;
            m_pCFilterBuff = 0;

            CVector::free(m_ptProcBuff);

            m_bisInitialized = false;
        }
        else
        {
            m_pCFilterBuff->reset();
            m_pCFilterBuff->setWriteIdx(m_iNumFilterCoeffs - 1);
            CVector::setZero(m_ptProcBuff, m_iNumFilterCoeffs - static_cast<long long>(1));
        }

        return Error_t::kNoError;
    }

    /*! filters the signal (transposed direct form II implementation), can be called block by block
     \param pfOut output signal (to be written, allocated by user)
     \param pfIn input signal
     \param iNumSamples length of both pfOut and pfIn
     \return Error_t
     */
    Error_t process(T *pfOut, const T *pfIn, long long iNumSamples)
    {
        if (!m_bisInitialized)
            return Error_t::kFunctionIllegalCallError;
        if (!pfOut || !pfIn || iNumSamples <= 0)
            return Error_t::kFunctionInvalidArgsError;

        for (auto i = 0; i < iNumSamples; i++)
        {
            T fTmp = m_ptProcBuff[0] + m_aptCoeff[kFir][0] * pfIn[i];

            for (auto j = 1; j < m_iNumFilterCoeffs - 1; j++)
                m_ptProcBuff[j - 1] = m_ptProcBuff[j] - m_aptCoeff[kIir][j] * fTmp + m_aptCoeff[kFir][j] * pfIn[i];

            m_ptProcBuff[m_iNumFilterCoeffs - 2] = -m_aptCoeff[kIir][m_iNumFilterCoeffs - 1] * fTmp + m_aptCoeff[kFir][m_iNumFilterCoeffs - 1] * pfIn[i];

            pfOut[i] = fTmp;
        }
        return Error_t::kNoError;
    }

    /*! filters the signal (direct form II implementation, here as "informative" implementation)
     \param pfOut output signal (to be written, allocated by user)
     \param pfIn input signal
     \param iNumSamples length of both pfOut and pfIn
     \return Error_t
     */
    Error_t processDFII(T *pfOut, const T *pfIn, long long iNumSamples)
    {
        if (!m_bisInitialized)
            return Error_t::kFunctionIllegalCallError;
        if (!pfOut || !pfIn || iNumSamples <= 0)
            return Error_t::kFunctionInvalidArgsError;

        for (auto i = 0; i < iNumSamples; i++)
        {
            T fTmp = pfIn[i];

            // get buffer values
            m_pCFilterBuff->get(m_ptProcBuff, m_iNumFilterCoeffs - 1);

            // IIR part
            for (auto j = 1; j < m_iNumFilterCoeffs; j++)
                fTmp -= m_aptCoeff[kIir][j] * m_ptProcBuff[m_iNumFilterCoeffs - 1 - j];

            //put new value into buffer
            m_pCFilterBuff->putPostInc(fTmp);

            // FIR part
            pfOut[i] = m_aptCoeff[kFir][0] * fTmp;
            for (auto j = 1; j < m_iNumFilterCoeffs; j++)
                pfOut[i] += m_aptCoeff[kFir][j] * m_ptProcBuff[m_iNumFilterCoeffs - 1 - j];

            // increment read index
            m_pCFilterBuff->getPostInc();
        }
        return Error_t::kNoError;
    }

    /*! zero phase filtering (can only be called with a complete signal, not block by block)
     \param pfOut output signal (to be written, allocated by user)
     \param pfIn input signal
     \param iNumSamples length of both pfOut and pfIn
     \return Error_t
     */
    Error_t filtfilt(T *pfOut, const T *pfIn, long long iNumSamples)
    {
        if (!m_bisInitialized)
            return Error_t::kFunctionIllegalCallError;
        if (!pfOut || !pfIn || iNumSamples <= 0)
            return Error_t::kFunctionInvalidArgsError;

        // reset filter but don't delete memory
        this->reset(false);

        float *pfTmpBuff = 0;
        float *pfPadding = 0;
        int iPadLength = 3 * (m_iNumFilterCoeffs - 1);

        if (iPadLength > iNumSamples)
            return Error_t::kFunctionIllegalCallError;

        // alloc necessary memory
        CVector::alloc(pfTmpBuff, iNumSamples + iPadLength);
        CVector::alloc(pfPadding, iPadLength);

        for (auto i = 0; i < iPadLength; i++)
            pfPadding[iPadLength - 1 - i] = 2 * pfIn[0] - pfIn[i + 1];

        // set initial state
        this->setInitState_(pfPadding[0]);

        // front padding (discard results)
        this->process(pfTmpBuff, pfPadding, iPadLength);

        // forward path
        this->process(pfTmpBuff, pfIn, iNumSamples);

        // back padding in forward path
        for (auto i = 0; i < iPadLength; i++)
            pfPadding[i] = 2 * pfIn[iNumSamples - 1] - pfIn[iNumSamples - 2 - i];
        this->process(&pfTmpBuff[iNumSamples], pfPadding, iPadLength);

        // set initial state
        this->setInitState_(pfTmpBuff[iNumSamples + iPadLength - 1]);

        // padded backward path (discard results)
        for (auto i = iNumSamples + iPadLength - 1; i >= iNumSamples; i--)
            this->process(pfOut, &pfTmpBuff[i], 1);

        // backward path
        for (auto i = iNumSamples - 1; i >= 0; i--)
            this->process(&pfOut[i], &pfTmpBuff[i], 1);

        // reset filter but don't delete memory
        this->reset(false);

        // free memory
        CVector::free(pfTmpBuff);
        CVector::free(pfPadding);

        return Error_t::kNoError;
    }

private:
    CFilter(const CFilter &that);
    CFilter &operator=(const CFilter &c);

    enum FilterCoeffs_t
    {
        kFir,
        kIir,

        kNumFilterDims
    };

    // see sadovsky, bartusek, optimisation of the transient response of a digital filter, radioengineering (9) no 2, 2000
    void setInitState_(float fWeight)
    {
        float **ppfA = 0;
        float *pfB = 0;
        float *pfZi = 0;

        int iLenCoeffBuffs = m_iNumFilterCoeffs - 1;

        CVector::alloc(pfB, iLenCoeffBuffs);
        CVector::alloc(pfZi, iLenCoeffBuffs);
        CMatrix::alloc(ppfA, iLenCoeffBuffs, iLenCoeffBuffs);

        for (auto i = 1; i < m_iNumFilterCoeffs; i++)
            pfB[i - 1] = m_aptCoeff[kFir][i] - m_aptCoeff[kFir][0] * m_aptCoeff[kIir][i];

        for (auto m = 0; m < iLenCoeffBuffs; m++)
        {
            ppfA[m][m] = 1;
            ppfA[m][0] += m_aptCoeff[kIir][m + 1];
            if (m < iLenCoeffBuffs - 1)
                ppfA[m][m + 1] = -1;
        }

        // solve linear equation
        CMatrix::inv_I(ppfA, iLenCoeffBuffs, iLenCoeffBuffs);
        CMatrix::mulMatColvec(pfZi, ppfA, pfB, iLenCoeffBuffs, iLenCoeffBuffs);

        // set internal state
        for (auto i = 0; i < iLenCoeffBuffs; i++)
            m_ptProcBuff[i] = fWeight * pfZi[i];

        // clean up
        CMatrix::free(ppfA, iLenCoeffBuffs);
        CVector::free(pfB);
        CVector::free(pfZi);
    }

    CRingBuffer<T> *m_pCFilterBuff = 0; //!< internal ring buffer for filter (only used for direct form II implementation CFilter::processDFII)

    T *m_aptCoeff[kNumFilterDims] = { 0,0 }; //!< filter coefficients

    T *m_ptProcBuff = 0; //!< temp buffer for processing

    int m_iNumFilterCoeffs = 0; //!< number of filter coefficients

    bool m_bisInitialized = false; //!< true if initialized
};

/*! \brief class providing a generic filter implementation
* implementation inspired by src from https://exstrom.com/journal/sigproc/dsigproc.html
*/
class CButterLp
{
public:

    /*! computes buttworth lowpass coefficients
     \param pfB output signal (to be written, length iOrder+1, allocated by user)
     \param pfA input signal (to be written, length iOrder+1, allocated by user)
     \param iOrder number of poles
     \param fCutOff normed cutoff between 0 and 1 (fs/2)
     \return Error_t
     */
    template<typename T>
    static Error_t calcCoeffs(T *pfB, T *pfA, int iOrder, T fCutOff)
    {
        const long long iTmpOrder = static_cast<long long>(iOrder); // silly compiler warnings get annoying
        float *pfTmpA = 0;
        CVector::alloc(pfTmpA, 2 * iTmpOrder);
        CVector::setZero(pfB, iTmpOrder + 1);
        CVector::setZero(pfTmpA, 2 * iTmpOrder);

        calcB(pfB, iOrder, fCutOff);
        calcA(pfTmpA, iOrder, fCutOff);
        CVector::copy(pfA, pfTmpA, iTmpOrder + 1);

        CVector::free(pfTmpA);

        return Error_t::kNoError;
    }
private:

    template<typename T>
    static Error_t calcB(T *pfB, int iOrder, T fCutOff)
    {
        pfB[0] = 1;
        pfB[1] = static_cast<T>(iOrder);
        for (auto j = 2; j <= iOrder / 2; ++j)
        {
            pfB[j] = (iOrder - j + 1) * pfB[j - 1] / j;
            pfB[iOrder - j] = pfB[j];
        }
        pfB[iOrder - 1] = static_cast<T>(iOrder);
        pfB[iOrder] = 1;

        T fScale = calcScale(iOrder, fCutOff);

        for (auto j = 0; j < iOrder + 1; j++)
            pfB[j] *= fScale;

        return Error_t::kNoError;
    }

    template<typename T>
    static Error_t calcA(T *pfA, int iOrder, T fCutOff)
    {
        T *pfCoeff = 0;     // binomial coefficients

        CVector::alloc<T>(pfCoeff, static_cast<long long>(2) * iOrder);

        for (auto j = 0; j < iOrder; j++)
        {
            T fArg = static_cast<T>(M_PI * (2. * j + 1) / (2. * iOrder));
            T fNorm = static_cast<T>(1 + std::sin(M_PI * fCutOff) * std::sin(fArg));
            pfCoeff[2 * j] = static_cast<T>(-std::cos(M_PI * fCutOff) / fNorm);
            pfCoeff[2 * j + 1] = static_cast<T>(-std::sin(M_PI * fCutOff) * std::cos(fArg) / fNorm);
        }

        multBinomial(pfA, pfCoeff, iOrder);

        pfA[1] = pfA[0];
        pfA[0] = 1;
        for (auto j = 3; j <= iOrder; j++)
            pfA[j] = pfA[2 * j - 2];

        CVector::free<T>(pfCoeff);

        return Error_t::kNoError;
    }

    template<typename T>
    static T calcScale(int iOrder, T fCutOff)
    {
        T fPhase = static_cast<T>(M_PI / (2. * iOrder));
        T fScale = 1;

        for (auto j = 0; j < iOrder / 2; ++j)
            fScale *= static_cast<T>(1 + std::sin(M_PI * fCutOff) * std::sin((2 * j + 1) * fPhase));

        if (iOrder % 2)
            fScale *= static_cast<T>(std::sin(M_PI * fCutOff / 2) + std::cos(M_PI * fCutOff / 2));
        fScale = static_cast<T>(std::pow(std::sin(M_PI * fCutOff / 2), iOrder) / fScale);

        return fScale;
    }

    template<typename T>
    static Error_t multBinomial(T *pfOut, const T *pfIn, int iOrder)
    {
        for (auto i = 0; i < iOrder; i++)
        {
            for (auto j = i; j > 0; j--)
            {
                pfOut[2 * j] += pfIn[2 * i] * pfOut[2 * (j - 1)] - pfIn[2 * i + 1] * pfOut[2 * (j - 1) + 1];
                pfOut[2 * j + 1] += pfIn[2 * i] * pfOut[2 * (j - 1) + 1] + pfIn[2 * i + 1] * pfOut[2 * (j - 1)];
            }
            pfOut[0] += pfIn[2 * i];
            pfOut[1] += pfIn[2 * i + 1];
        }
        return Error_t::kNoError;
    }
};

#endif // #if !defined(__ACA_Filter_HEADER_INCLUDED__)
