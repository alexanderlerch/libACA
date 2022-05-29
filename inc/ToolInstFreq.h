#if !defined(__InstFreq_hdr__)
#define __InstFreq_hdr__

#define _USE_MATH_DEFINES
#include <cmath>

#include "Util.h"
#include "Vector.h"
#include "Fft.h"
#include "ErrorDef.h"


/*! \brief class for single-pole low pass filtering
*/
class CInstFreq
{
public:

    explicit CInstFreq(int iFftLength, int iHopLength, float fSampleRate) :
        m_iBlockLength(iFftLength),
        m_iHopLength(iHopLength),
        m_fSampleRate(fSampleRate)
    {
        // init Fft instance for handling phase and stuff
        m_pCFft = new CFft();
        m_pCFft->init(iFftLength);

        auto iPhaseLength = m_pCFft->getLength(CFft::kLengthPhase);
        m_pfOmega = new float[iPhaseLength];

        // use one buffer with two pointers
        m_pfSwapBuff = new float[2*iPhaseLength];
        CVectorFloat::setZero(m_pfSwapBuff, 2 * iPhaseLength);
        m_apfPhase[0] = &m_pfSwapBuff[0];
        m_apfPhase[1] = &m_pfSwapBuff[iPhaseLength];

        //init omega
        for (auto k = 0; k < iPhaseLength; k++)
            m_pfOmega[k] = k*m_iHopLength * 2*M_PI/ iPhaseLength;

    }

    virtual ~CInstFreq() 
    {
        delete[] m_pfOmega;
        delete[] m_pfSwapBuff;

        delete m_pCFft;
    }


    /*! performs the InstFreq computation
    \param pfInstFreq output vector containing the estimated frequencies (to be written), length iFftLength/2+1
    \param pfSpectrum current (complex) spectrum
    return Error_t
    */
    Error_t process(float *pfInstFreq, const CFft::complex_t* pfSpectrum)
    {
        if (!pfInstFreq || !pfSpectrum)
            return Error_t::kFunctionInvalidArgsError;

        // get current phase
        m_pCFft->getPhase(m_apfPhase[kPhaseCurr], pfSpectrum);

         //// calc instantaneous frequency
        //zplfRealSub_I(m_apfPhase[kPhasePrev], m_apfPhase[kPhaseCurr], m_iFftLength >> 1);
        //zplfRealAdd_I(m_apfPhase[kPhasePrev], m_pfOmega, m_iFftLength >> 1);
        //zplfRealMulC_I(m_apfPhase[kPhasePrev], -1.0F, m_iFftLength >> 1);
        //zplfPrincArg(m_apfPhase[kPhasePrev], m_apfPhase[kPhasePrev], m_iFftLength >> 1);
        //zplfRealAdd_I(m_apfPhase[kPhasePrev], m_pfOmega, m_iFftLength >> 1);
        //zplfRealMulC_I(m_apfPhase[kPhasePrev], 1.0F / (_2PI * static_cast<float>(m_iHopSize) / static_cast<float>(m_iFftLength)), m_iFftLength >> 1);


        // remember phase for next call but avoid copying
        CUtil::swap(m_apfPhase[kPhasePrev], m_apfPhase[kPhaseCurr]);

        return Error_t::kNoError;
    }

private:
    enum Phase_t
    {
        kPhasePrev,
        kPhaseCurr,

        kNumPhases
    };
    CInstFreq(const CInstFreq& that);
    CInstFreq& operator=(const CInstFreq& c);

    int m_iBlockLength = 0,
        m_iHopLength = 0;
    float m_fSampleRate = 0.F; 

    float* m_apfPhase[kNumPhases] = { 0 };
    float* m_pfSwapBuff = 0;
    float* m_pfOmega = 0;

    CFft *m_pCFft = 0;
};




#endif // #if !defined(__InstFreq_hdr__)



