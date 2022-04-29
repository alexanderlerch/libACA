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
    virtual ~CFft() 
    {
        reset();
    };

    /*! initializes an FFT instance
    \param int iBlockLength: input data block length in Frames
    \param int iZeroPadFactor: fft length (zeropadded)
    \param WindowFunction_t eWindow: window type
    \param Windowing_t eWindowing: apply window before FFT, after IFFT, or both
    \return Error_t
    */
    Error_t init (int iBlockLength, int iZeroPadFactor = 1, WindowFunction_t eWindow = kWindowHann, Windowing_t eWindowing = kPreWindow);
    
    /*! resets an FFT instance
    \return Error_t
    */
    Error_t reset ();
 
    /*! use a customized window
    \param const float * pfNewWindow: window data of length iBlockLength (\sa initInstance)
    \return Error_t
    */
    Error_t overrideWindow (const float *pfNewWindow);

    /*! retrieve current window
    \param float * pfWindow: window data of length iBlockLength (\sa initInstance)
    \return Error_t
    */
    Error_t getWindow (float *pfWindow) const;

    /*! perform the FFT
    \param complex_t * pfSpectrum: output result of length iBlockLength * iZeroPadFactor (\sa initInstance)
    \param const float * pfInput: input data of length iBlockLength (\sa initInstance)
    \return Error_t
    */
    Error_t doFft (complex_t *pfSpectrum, const float *pfInput);

    /*! perform IFFT
    \param float * pfOutput: time domain output signal of length iBlockLength * iZeroPadFactor (\sa initInstance)
    \param const complex_t * pfSpectrum: input spectrum of length iBlockLength * iZeroPadFactor (\sa initInstance)
    \return Error_t
    */
    Error_t doInvFft (float *pfOutput, const complex_t *pfSpectrum);

    /*! extract magnitude spectrum from complex values
    \param float * pfMag: resulting magnitude spectrum of length (iBlockLength * iZeroPadFactor)/2+1 (\sa initInstance)
    \param const complex_t * pfSpectrum: input spectrum of length iBlockLength * iZeroPadFactor (\sa initInstance)
    \return Error_t
    */
    Error_t getMagnitude (float *pfMag, const complex_t *pfSpectrum) const;

    /*! extract phase spectrum from complex values
    \param float * pfPhase: resulting phase spectrum of length (iBlockLength * iZeroPadFactor)/2+1 (\sa initInstance)
    \param const complex_t * pfSpectrum: input spectrum of length iBlockLength * iZeroPadFactor (\sa initInstance)
    \return Error_t
    */
    Error_t getPhase(float *pfPhase, const complex_t *pfSpectrum) const;

    /*! get real and imaginary part from complex spectrum
    \param float * pfReal: resulting real part of length (iBlockLength * iZeroPadFactor)/2+1 (\sa initInstance) 
    \param float * pfImag: resulting imaginary part of length (iBlockLength * iZeroPadFactor)/2 (\sa initInstance)
    \param const complex_t * pfSpectrum: input spectrum of length iBlockLength * iZeroPadFactor (\sa initInstance)
    \return Error_t
    */
    Error_t splitRealImag(float *pfReal, float *pfImag, const complex_t *pfSpectrum) const;

    /*! merge real and imaginary parts into complex spectrum
    \param complex_t * pfSpectrum: resulting spectrum of length iBlockLength * iZeroPadFactor (\sa initInstance)
    \param const float * pfReal: input real part of length (iBlockLength * iZeroPadFactor)/2+1 (\sa initInstance) 
    \param const float * pfImag: input imaginary part of length (iBlockLength * iZeroPadFactor)/2 (\sa initInstance)
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
    \param Length_t eLengthIdx spectrum type
    \return int
    */
    int getLength(Length_t eLengthIdx);

    /*! convert frequency to bin value
    \param float fFreqInHz: frequency to convert
    \param float fSampleRateInHz: sample rate of the audio signal
    \return float: index (not quantized to integer)
    */
    float freq2bin (float fFreqInHz, float fSampleRateInHz) const;

    /*! convert bin to frequency
    \param int iBinIdx: index of the FFT bin
    \param float fSampleRateInHz: sample rate of the audio signal
    \return float: frequency in Hz
    */
    float bin2freq (int iBinIdx, float fSampleRateInHz) const;

private:
    Error_t allocMemory ();
    Error_t freeMemory_ ();
    Error_t computeWindow (WindowFunction_t eWindow);

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



