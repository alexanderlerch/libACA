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
    \param pCInstance pointer to instance to be written
    \param strAudioFilePath complete path to audio file
    \param iBlockLength: FFT block length in Frames
    \param iHopLength: hop length in Frames
    \param bNormalize: flag if input audio should be normalized
    \param pfWindow: window function of length iBlockLength (optional, default will be von-Hann if 0)
    \return Error_t
    */
    static Error_t create(CSpectrogramIf*& pCInstance, const std::string strAudioFilePath, int iBlockLength = 2048, int iHopLength = 1024, bool bNormalize = true, float *pfWindow = 0);

    /*! initializes a Spectrogram instance from audio data
    \param pCInstance pointer to instance to be written
    \param pfAudio complete audio data
    \param iNumFrames: length of pfAudio
    \param fSampleRate: sample rate in Hz
    \param iBlockLength: FFT block length in Frames
    \param iHopLength: hop length in Frames
    \param bNormalize: flag if input audio should be normalized
    \param pfWindow: window function of length iBlockLength (optional, default will be von-Hann if 0)
    \return Error_t
    */
    static Error_t create(CSpectrogramIf*& pCInstance, const float *pfAudio, long long iNumFrames, float fSampleRate, int iBlockLength = 2048, int iHopLength = 1024, bool bNormalize = true, float* pfWindow = 0);

    /*! destroys a Spectrogram instance
    \param pCInstance pointer to instance to be destroyed
    \return Error_t
    */
    static Error_t destroy(CSpectrogramIf*& pCInstance);

    /*! returns size of matrix to be allocated by user
    \param iNumRows (number of rows, to be written) equals number of frequency bins
    \param iNumCols (number of columns, to be written) equals number of blocks
    \return Error_t
    */
    Error_t getSpectrogramDimensions(int& iNumRows, int& iNumCols) const;

    /*! returns axis ticks
    \param pfAxisTicks (to be written) equals iNumRows if eAxisLabel == kFrequencyInHz, otherwise iNumCols
    \param eAxisLabel indicator which axis
    \return Error_t
    */
    Error_t getAxisVectors(float *pfAxisTicks, enum AxisLabel_t eAxisLabel) const;

    /*! performs the Spectrogram computation
    \param ppfSpectrogram (user-allocated, to be written, dimensions from CSpectrogramIf::getSpectrogramDimensions)
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



