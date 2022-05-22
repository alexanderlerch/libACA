#if !defined(__Feature_hdr__)
#define __Feature_hdr__

#include <string>

#include "ErrorDef.h"

// forward declarations
class CAudioFileIf;
class CFft;
class CNormalizeAudio;
class CBlockAudioIf;

/*! \brief class for computation of a magnitude Feature from either a file or a vector
*/
class CFeatureIf
{
public:
    /*! \brief enum to index all features
    */
    enum Feature_t
    {
        kFeatureSpectralCentroid,
        kFeatureSpectralCrestFactor,
        kFeatureSpectralDecrease,
        kFeatureSpectralFlatness,
        kFeatureSpectralFlux,
        kFeatureSpectralKurtosis,
        kFeatureSpectralMfccs,
        kFeatureSpectralPitchChroma,
        kFeatureSpectralRolloff,
        kFeatureSpectralSkewness,
        kFeatureSpectralSlope,
        kFeatureSpectralSpread,
        kFeatureSpectralTonalPowerRatio,
        kFeatureTimeAcfCoeff,
        kFeatureTimeMaxAcf,
        kFeatureTimePeakEnvelope,
        kFeatureTimeRms,
        kFeatureTimeStd,
        kFeatureTimeZeroCrossingRate,

        kNumFeatures
    };

    /*! initializes a Feature instance with file reading
    \param pCInstance pointer to instance to be written
    \param strAudioFilePath complete path to audio file
    \param iBlockLength: FFT block length in Frames
    \param iHopLength: hop length in Frames
    \param bNormalize: flag if input audio should be normalized
    \param pfWindow: window function of length iBlockLength (optional, default will be von-Hann if 0)
    \return Error_t
    */
    static Error_t create(CFeatureIf*& pCInstance, Feature_t eFeatureIdx, const std::string strAudioFilePath, int iBlockLength = 2048, int iHopLength = 1024, bool bNormalize = true);

    /*! initializes a Feature instance from audio data
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
    static Error_t create(CFeatureIf*& pCInstance, Feature_t eFeatureIdx, const float* pfAudio, long long iNumFrames, float fSampleRate, int iBlockLength = 2048, int iHopLength = 1024);

    /*! initializes a Feature instance from spectrogram data (spectral features only)
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
    static Error_t create(CFeatureIf*& pCInstance, Feature_t eFeatureIdx, const float** ppfSpectrogram, long long iNumFrames, float fSampleRate, int iBlockLength = 2048, int iHopLength = 1024);

    /*! destroys a Feature instance
    \param pCInstance pointer to instance to be destroyed
    \return Error_t
    */
    static Error_t destroy(CFeatureIf*& pCInstance);

    /*! returns size of matrix to be allocated by user
    \param iNumRows (number of rows, to be written) equals number of frequency bins
    \param iNumCols (number of columns, to be written) equals number of blocks
    \return Error_t
    */
    Error_t getFeatureDimensions(int& iNumRows, int& iNumCols) const;

    /*! returns axis ticks
    \param pfAxisTicks (to be written) equals iNumRows if eAxisLabel == kFrequencyInHz, otherwise iNumCols
    \return Error_t
    */
    Error_t getFeatureTimeStamps(float* pfAxisTicks) const;

    /*! performs the Feature computation for 1 dimensional features and writes the result
    \param ppfFeature (user-allocated, to be written, dimensions from CFeatureIf::getFeatureDimensions)
    \return Error_t
    */
    Error_t getFeature1Dim(float* pfFeature);

    /*! performs the Feature computation for N dimensional features and writes the result
    \param ppfFeature (user-allocated, to be written, dimensions from CFeatureIf::getFeatureDimensions)
    \return Error_t
    */
    Error_t getFeatureNDim(float** ppfFeature);

protected:
    CFeatureIf();
    virtual ~CFeatureIf();
    CFeatureIf(const CFeatureIf& that);

    Error_t reset_();                    //!< reset configuration
    Error_t init_(float* pfWindow);      //!< init configuration
    void computeMagSpectrum_(int iLength);

    CNormalizeAudio* m_pCNormalize = 0;  //!< instantiate if audio file normalization is wanted
 
    CBlockAudioIf* m_pCBlockAudio = 0;   //!< instantiate for blocking time domain signal

    CFft* m_pCFft = 0;                   //!< fft instance

    int m_iBlockLength = 0,              //!< fft length
        m_iHopLength = 0;                //!< hop length

    float m_fSampleRate = 0;             //!< sample rate

    float* m_pfSpectrum = 0;             //!< temporary buffer for current spectrum
    float* m_pfProcessBuff = 0;          //!<  temporary buffer

    bool    m_bIsInitialized = false;    //!< true if initialized
};

#endif // #if !defined(__Feature_hdr__)



