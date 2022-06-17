#if !defined(__ACA_Key_HEADER_INCLUDED__)
#define __ACA_Key_HEADER_INCLUDED__

#include <string>

#include "ErrorDef.h"

// forward declarations
class CKeyFromChroma;
class CFeatureIf;

/*! \brief class for computation of the key from either a file or a vector
*/
class CKey
{
public:

    /*! detectable keys
    */
    enum Keys_t
    {
        kCMajor,
        kCsMajor,
        kDMajor,
        kDsMajor,
        kEMajor,
        kFMajor,
        kFsMajor,
        kGMajor,
        kGsMajor,
        kAMajor,
        kAsMajor,
        kBMajor,

        kCMinor,
        kCsMinor,
        kDMinor,
        kDsMinor,
        kEMinor,
        kFMinor,
        kFsMinor,
        kGMinor,
        kGsMinor,
        kAMinor,
        kAsMinor,
        kBMinor,

        kNoKey,

        kNumKeys
    };

    CKey();
    virtual ~CKey();

    /*! initializes a Key instance with file reading
    \param strAudioFilePath complete path to audio file
    \param iBlockLength: FFT block length in samples
    \param iHopLength: hop length in samples
    \return Error_t
    */
    Error_t init(const std::string &strAudioFilePath, int iBlockLength = 4096, int iHopLength = 2048);

    /*! initializes a Key instance from audio data
    \param pfAudio complete audio data
    \param iNumSamples: length of pfAudio
    \param fSampleRate: sample rate in Hz
    \param iBlockLength: FFT block length in samples
    \param iHopLength: hop length in samples
    \return Error_t
    */
    Error_t init(const float *pfAudio, long long iNumSamples, float fSampleRate, int iBlockLength = 4096, int iHopLength = 2048);

    /*! resets instance
    \return Error_t
    */
    Error_t reset();

    /*! performs the key computation and returns the key index
    \return Keys_t
    */
    int compKey();

    /*! returns Key name as string
    \param eKeyIdx Key index
    \return std::string
    */
    static std::string getKeyString(Keys_t eKeyIdx);

    /*! returns Key index from string
    \param sKeyString string describing the Key
    \return Keys_t
    */
    static Keys_t getKeyIdxFromString(std::string sKeyString);

protected:
    CKey(const CKey &that);
    CKey &operator=(const CKey &c);

    CFeatureIf *m_pCPitchChromaExtractor = 0; //!< instance for extracting the pitch chroma
    CKeyFromChroma *m_pCKeyFromChroma = 0; //!< estimate key from one chroma

    bool    m_bIsInitialized = false;  //!< true if initialized
};

#endif // #if !defined(__ACA_Key_HEADER_INCLUDED__)
