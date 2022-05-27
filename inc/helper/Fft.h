#if !defined(__Fft_hdr__)
#define __Fft_hdr__

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
        //kWindowNone,            //!< rectangular window (see Windowing_t::kNoWindow)
        kWindowSine,            //!< sinusoidal window
        kWindowHann,            //!< von-Hann window
        kWindowHamming,         //!< Hamming window

        kNumWindows
    };
    enum Windowing_t
    {
        kNoWindow   = 0x00L,    //!< do not apply any window
        kPreWindow  = 0x01L,    //!< apply window function before FFT
        kPostWindow = 0x02L     //!< apply window function after IFFT
    };

    CFft();
    virtual ~CFft();;

    /*! initializes an FFT instance
    \param iBlockLength: input data block length in Frames
    \param iZeroPadFactor: fft length (zeropadded)
    \param eWindow: window type
    \param eWindowing: apply window before FFT, after IFFT, or both
    \return Error_t
    */
    Error_t init (int iBlockLength, int iZeroPadFactor = 1, WindowFunction_t eWindow = kWindowHann, Windowing_t eWindowing = kPreWindow);
    
    /*! resets an FFT instance
    \return Error_t
    */
    Error_t reset ();
 
    /*! use a customized window
    \param pfNewWindow: window data of length iBlockLength (\sa initInstance)
    \return Error_t
    */
    Error_t overrideWindow (const float *pfNewWindow);

    /*! retrieve current window
    \param pfWindow: window data of length iBlockLength (\sa initInstance)
    \return Error_t
    */
    Error_t getWindow (float *pfWindow) const;

    /*! perform the FFT
    \param pfSpectrum: output result of length iBlockLength * iZeroPadFactor (\sa initInstance)
    \param pfInput: input data of length iBlockLength (\sa initInstance)
    \return Error_t
    */
    Error_t compFft (complex_t *pfSpectrum, const float *pfInput);

    /*! perform IFFT
    \param pfOutput: time domain output signal of length iBlockLength * iZeroPadFactor (\sa initInstance)
    \param pfSpectrum: input spectrum of length iBlockLength * iZeroPadFactor (\sa initInstance)
    \return Error_t
    */
    Error_t compInvFft (float *pfOutput, const complex_t *pfSpectrum);

    /*! extract magnitude spectrum from complex values
    \param pfMag: resulting magnitude spectrum of length (iBlockLength * iZeroPadFactor)/2+1 (\sa initInstance)
    \param pfSpectrum: input spectrum of length iBlockLength * iZeroPadFactor (\sa initInstance)
    \return Error_t
    */
    Error_t getMagnitude (float *pfMag, const complex_t *pfSpectrum) const;

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
        kLengthFft,             //!< length of the FFT
        kLengthData,            //!< length of the input data
        kLengthMagnitude,       //!< length of the magnitude spectrum
        kLengthPhase,           //!< length of the phase spectrum

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
    float freq2bin (float fFreqInHz, float fSampleRateInHz) const;

    /*! convert bin to frequency
    \param iBinIdx: index of the FFT bin
    \param fSampleRateInHz: sample rate of the audio signal
    \return frequency in Hz
    */
    float bin2freq (int iBinIdx, float fSampleRateInHz) const;

    void conjugate_I(complex_t *pfFftResult) const;
    void multiply_I(complex_t* pfFftSrc1Dest, const complex_t* pfFftSrc2) const;

private:
    Error_t allocMemory_ ();
    Error_t freeMemory_ ();
    Error_t computeWindow_ (WindowFunction_t eWindow);

    float   *m_pfProcessBuff;
    float   *m_pfWindowBuff;

    int     m_iDataLength;
    int     m_iFftLength;

    Windowing_t m_ePrePostWindowOpt;

    bool    m_bIsInitialized;

    static const float m_Pi;
    static const float m_Pi2;
};

#endif // #if !defined(__Fft_hdr__)



