#if !defined(__ACA_Feature_HEADER_INCLUDED__)
#define __ACA_Feature_HEADER_INCLUDED__

#include <string>

#include "ErrorDef.h"

// forward declarations
class CAudioFileIf;
class CFft;
class CNormalizeAudio;
class CBlockAudioIf;
class CFeatureFromBlockIf;

/*! \brief class for computation of a instantaneous feature from either a file or a vector,
* supports both one-dimensional and multi-dimensional features
*/
class CFeatureIf
{
public:
    /*! \brief enum to index all features
    * note: update CFeatureIf::isFeatureSpectral_() when modifying
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
    \param eFeatureIdx as defined in Feature_t
    \param strAudioFilePath complete path to audio file
    \param iBlockLength: FFT block length in samples
    \param iHopLength: hop length in samples
    \return Error_t
    */
    static Error_t create(CFeatureIf *&pCInstance, Feature_t eFeatureIdx, const std::string &strAudioFilePath, int iBlockLength = 2048, int iHopLength = 1024);

    /*! initializes a Feature instance from audio data
    \param pCInstance pointer to instance to be written
    \param eFeatureIdx as defined in Feature_t
    \param pfAudio complete audio data
    \param iNumSamples: length of pfAudio
    \param fSampleRate: sample rate in Hz
    \param iBlockLength: FFT block length in samples
    \param iHopLength: hop length in samples
    \return Error_t
    */
    static Error_t create(CFeatureIf *&pCInstance, Feature_t eFeatureIdx, const float *pfAudio, long long iNumSamples, float fSampleRate, int iBlockLength = 2048, int iHopLength = 1024);

    /*! destroys a Feature instance
    \param pCInstance pointer to instance to be destroyed
    \return Error_t
    */
    static Error_t destroy(CFeatureIf *&pCInstance);

    /*! returns size of matrix to be allocated by user
    \param iNumRows (number of rows, to be written) equals number of feature values per time stamp
    \param iNumCols (number of columns, to be written) equals number of blocks
    \return Error_t
    */
    Error_t getFeatureDimensions(int &iNumRows, int &iNumCols) const;

    /*! returns time stamps
    \param iBlockIdx index of block
    \return float time stamp
    */
    float getTimeStamp(int iBlockIdx) const;

    /*! returns time stamps
    \param pfAxisTicks (user- allocated, to be written) length iNumBlocks
    \return Error_t
    */
    Error_t getTimeStamps(float *pfAxisTicks) const;

    /*! performs the Feature computation for 1 dimensional features and writes the result
    \param pfFeature (user-allocated, to be written, dimensions from CFeatureIf::getFeatureDimensions)
    \return Error_t
    */
    Error_t compFeature1Dim(float *pfFeature);

    /*! performs the Feature computation for N dimensional features and writes the result
    \param ppfFeature (user-allocated, to be written, dimensions from CFeatureIf::getFeatureDimensions)
    \return Error_t
    */
    Error_t compFeatureNDim(float **ppfFeature);

    /*! returns feature name as string
    \param eFeatureIdx feature index
    \return std::string
    */
    static std::string getFeatureString(Feature_t eFeatureIdx);

    /*! returns feature index from string
    \param sFeatureString string describing the feature
    \return Feature_t
    */
    static Feature_t getFeatureIdxFromString(std::string sFeatureString);

protected:
    CFeatureIf();
    virtual ~CFeatureIf();
    CFeatureIf(const CFeatureIf &that);
    CFeatureIf &operator=(const CFeatureIf &c);

    Error_t reset_();  //!< reset configuration
    Error_t init_(Feature_t eFeatureIdx); //!< init configuration
    bool isFeatureSpectral_(Feature_t eFeatureIdx);  //!< bool differentiating between time and spectral features
    void computeMagSpectrum_(); //!< compute magnitude spectrum for spectral features

    CNormalizeAudio *m_pCNormalize = 0; //!< instantiate if audio file normalization is wanted

    CBlockAudioIf *m_pCBlockAudio = 0; //!< instantiate for blocking time domain signal

    CFeatureFromBlockIf *m_pCFeature = 0;  //!< instance for feature computation with one block of data

    CFft *m_pCFft = 0; //!< fft instance

    int m_iBlockLength = 0, //!< fft length
        m_iHopLength = 0; //!< hop length

    float m_fSampleRate = 0; //!< sample rate

    float *m_pfProcBuff2 = 0; //!< temporary buffer 
    float *m_pfProcBuff1 = 0; //!<  temporary buffer

    bool    m_bIsInitialized = false; //!< true if initialized
};

#endif // #if !defined(__ACA_Feature_HEADER_INCLUDED__)
