#if !defined(__Spectrogram_hdr__)
#define __Spectrogram_hdr__

#include <string>

#include "ErrorDef.h"

// forward declarations
class CAudioFileIf;
class CFft;
class CNormalizeAudio;
class CBlockAudioIf;

/*! \brief class for computation of a magnitude spectrogram from either a file or a vector
*/
class CSpectrogramIf
{
public:
    enum AxisLabel_t
    {
        kFrequencyInHz,
        kTimeInS,

        kNumAxisLabels
    };

    /*! initializes a Spectrogram instance with file reading
    \param string strAudioFilePath complete path to audio file
    \param int iBlockLength: FFT block length in Frames
    \param int iHopLength: hop length in Frames
    \param bool bNormalize: flag if input audio should be normalized
    \param float pfWindow: window function of length iBlockLength (optional, default will be von-Hann if 0)
    \return Error_t
    */
    static Error_t create(CSpectrogramIf*& pCInstance, const std::string strAudioFilePath, int iBlockLength = 2048, int iHopLength = 1024, bool bNormalize = true, float *pfWindow = 0);

    /*! initializes a Spectrogram instance from audio data
    \param float *pfAudio complete audio data
    \param long long iNumFrames: length of pfAudio
    \param float fSampleRate: sample rate in Hz
    \param int iBlockLength: FFT block length in Frames
    \param int iHopLength: hop length in Frames
    \param bool bNormalize: flag if input audio should be normalized
    \param float pfWindow: window function of length iBlockLength (optional, default will be von-Hann if 0)
    \return Error_t
    */
    static Error_t create(CSpectrogramIf*& pCInstance, const float *pfAudio, long long iNumFrames, float fSampleRate, int iBlockLength = 2048, int iHopLength = 1024, bool bNormalize = true, float* pfWindow = 0);

    /*! destroys a Spectrogram instance
    \return Error_t
    */
    static Error_t destroy(CSpectrogramIf*& pCInstance);

    /*! returns size of matrix to be allocated by user
    \param int iNumRows (number of rows, to be written) equals number of frequency bins
    \param int iNumCols (number of columns, to be written) equals number of blocks
    \return Error_t
    */
    Error_t getSpectrogramDimensions(int& iNumRows, int& iNumCols) const;

    /*! returns axis ticks
    \param float *pfAxisTicks (to be written) equals iNumRows if eAxisLabel == kFrequencyInHz, otherwise iNumCols
    \param AxisLabel_t eAxisLabel indicator which axis
    \return Error_t
    */
    Error_t getAxisVectors(float *pfAxisTicks, enum AxisLabel_t eAxisLabel) const;

    /*! performs the Spectrogram computation
    \param float **ppfSpectrogram (user-allocated, to be written, dimensions from ::getSpectrogramDimensions)
    \return Error_t
    */
    Error_t process (float **ppfSpectrogram);

protected:
    CSpectrogramIf();
    virtual ~CSpectrogramIf();
    CSpectrogramIf(const CSpectrogramIf& that);

    Error_t reset_();                    //!< reset configuration
    Error_t init_(float* pfWindow);      //!< init configuration

    CNormalizeAudio* m_pCNormalize = 0;  //!< instantiate if normalization is wanted
 
    CBlockAudioIf* m_pCBlockAudio = 0;   //!< instantiate for blocking time domain signal

    CFft* m_pCFft = 0;                   //!< fft instance

    int m_iBlockLength = 0,              //!< fft length
        m_iHopLength = 0;                //!< hop length

    float m_fSampleRate = 0;             //!< sample rate

    float* m_pfSpectrum = 0;             //!< temporary buffer for current spectrum
    float* m_pfProcessBuff = 0;          //!<  temporary buffer

    bool    m_bIsInitialized = false;    //!< true if initialized


};

#endif // #if !defined(__Spectrogram_hdr__)



