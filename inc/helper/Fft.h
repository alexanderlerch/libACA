#if !defined(__ACA_Fft_HEADER_INCLUDED__)
#define __ACA_Fft_HEADER_INCLUDED__

#include "ErrorDef.h"

/*! \brief class for FFT computation with windowing
*/
class CFft
{
public:
    typedef float complex_t;

    /*! defines the window function to use -- a custom window can also be used (\sa overrideWindow) */
    enum WindowFunction_t
    {
        //kWindowNone,  //!< rectangular window (see Windowing_t::kNoWindow)
        kWindowSine, //!< sinusoidal window
        kWindowHann, //!< von-Hann window
        kWindowHamming, //!< Hamming window

        kNumWindows
    };
    enum Windowing_t
    {
        kNoWindow = 0x00L, //!< do not apply any window
        kPreWindow = 0x01L, //!< apply window function before FFT
        kPostWindow = 0x02L  //!< apply window function after IFFT
    };

    CFft();
    virtual ~CFft();

    /*! initializes an FFT instance
    \param iBlockLength: input data block length in samples
    \param iZeroPadFactor: fft length (zeropadded)
    \param eWindow: window type
    \param eWindowing: apply window before FFT, after IFFT, or both
    \return Error_t
    */
    Error_t init(int iBlockLength, int iZeroPadFactor = 1, WindowFunction_t eWindow = kWindowHann, Windowing_t eWindowing = kPreWindow);

    /*! resets an FFT instance
    \return Error_t
    */
    Error_t reset();

    /*! use a customized window
    \param pfNewWindow: window data of length iBlockLength (\sa initInstance)
    \return Error_t
    */
    Error_t overrideWindow(const float *pfNewWindow);

    /*! retrieve current window
    \param pfWindow: window data of length iBlockLength (\sa initInstance)
    \return Error_t
    */
    Error_t getWindow(float *pfWindow) const;

    /*! perform the FFT
    \param pfSpectrum: output result of length iBlockLength * iZeroPadFactor (\sa initInstance)
    \param pfIn: input data of length iBlockLength (\sa initInstance)
    \return Error_t
    */
    Error_t compFft(complex_t *pfSpectrum, const float *pfIn);

    /*! perform IFFT
    \param pfOut: time domain output signal of length iBlockLength * iZeroPadFactor (\sa initInstance)
    \param pfSpectrum: input spectrum of length iBlockLength * iZeroPadFactor (\sa initInstance)
    \return Error_t
    */
    Error_t compInvFft(float *pfOut, const complex_t *pfSpectrum);

    /*! extract magnitude spectrum from complex values
    \param pfMag: resulting magnitude spectrum of length (iBlockLength * iZeroPadFactor)/2+1 (\sa initInstance)
    \param pfSpectrum: input spectrum of length iBlockLength * iZeroPadFactor (\sa initInstance)
    \return Error_t
    */
    Error_t getMagnitude(float *pfMag, const complex_t *pfSpectrum) const;

    /*! extract phase spectrum from complex values
    \param pfPhase: resulting phase spectrum of length (iBlockLength * iZeroPadFactor)/2+1 (\sa initInstance)
    \param pfSpectrum: input spectrum of length iBlockLength * iZeroPadFactor (\sa initInstance)
    \return Error_t
    */
    Error_t getPhase(float *pfPhase, const complex_t *pfSpectrum) const;

    /*! get real and imaginary part from complex spectrum
    \param pfReal: resulting real part of length (iBlockLength * iZeroPadFactor)/2+1 (\sa initInstance)
    \param pfImag: resulting imaginary part of length (iBlockLength * iZeroPadFactor)/2 (\sa initInstance)
    \param pfSpectrum: input spectrum of length iBlockLength * iZeroPadFactor (\sa initInstance)
    \return Error_t
    */
    Error_t splitRealImag(float *pfReal, float *pfImag, const complex_t *pfSpectrum) const;

    /*! merge real and imaginary parts into complex spectrum
    \param pfSpectrum: resulting spectrum of length iBlockLength * iZeroPadFactor (\sa initInstance)
    \param pfReal: input real part of length (iBlockLength * iZeroPadFactor)/2+1 (\sa initInstance)
    \param pfImag: input imaginary part of length (iBlockLength * iZeroPadFactor)/2 (\sa initInstance)
    \return Error_t
    */
    Error_t mergeRealImag(complex_t *pfSpectrum, const float *pfReal, const float *pfImag) const;

    enum Length_t
    {
        kLengthFft, //!< length of the FFT
        kLengthData, //!< length of the input data
        kLengthMagnitude, //!< length of the magnitude spectrum
        kLengthPhase, //!< length of the phase spectrum

        kNumLengths
    };
    /*! return length of spectrum
    \param eLengthIdx spectrum type
    \return int
    */
    int getLength(Length_t eLengthIdx) const;

    /*! convert frequency to bin value
    \param fFreqInHz: frequency to convert
    \param fSampleRateInHz: sample rate of the audio signal
    \return index (not quantized to integer)
    */
    float freq2bin(float fFreqInHz, float fSampleRateInHz) const;

    /*! convert bin to frequency
    \param iBinIdx: index of the FFT bin
    \param fSampleRateInHz: sample rate of the audio signal
    \return frequency in Hz
    */
    float bin2freq(int iBinIdx, float fSampleRateInHz) const;

    /*! complex conjugate
    \param pfSpectrum: spectrum to be 'conjugated'
    */
    void conjugate_I(complex_t *pfSpectrum) const;

    /*! complex multiplication
    \param pfSrc1DestSpectrum: input/output
    \param pfSrc2Spectrum: multiplier
    */
    void multiply_I(complex_t *pfSrc1DestSpectrum, const complex_t *pfSrc2Spectrum) const;

private:
    CFft(const CFft &that);
    CFft &operator=(const CFft &c);
    Error_t allocMemory_();
    Error_t freeMemory_();
    Error_t computeWindow_(WindowFunction_t eWindow);

    float *m_pfProcBuff = 0; //!< generic processing buffer
    float *m_pfWindowBuff = 0; //!< window function

    int     m_iDataLength = 0; //!< length of data
    int     m_iFftLength = 0; //!<  length of FFT

    Windowing_t m_ePrePostWindowOpt = kNoWindow;  //!< windowing option

    bool    m_bIsInitialized = false; //!< flag indicating if the instance is initialized

    static const float m_Pi; //!< just to have it as float
    static const float m_Pi2; //!<  just to have it as float
};

#endif // #if !defined(__ACA_Fft_HEADER_INCLUDED__)
