#if !defined(__ACA_Fingerprint_HEADER_INCLUDED__)
#define __ACA_Fingerprint_HEADER_INCLUDED__

#include <string>

#include "ErrorDef.h"

// forward declarations
class CFft;
class CBlockAudioIf;
class CSubFingerprint;

/*! \brief class for computation of a fingerprint from either a file or a vector
*/
class CFingerprint
{
public:
    CFingerprint() {};
    virtual ~CFingerprint();

    /*! initializes a Fingerprint instance with file reading
    \param strAudioFilePath complete path to audio file
    \return Error_t
    */
    Error_t init(const std::string &strAudioFilePath);

    /*! initializes a Fingerprint instance from audio data
    \param pfAudio complete audio data
    \param iNumSamples: length of pfAudio
    \param fSampleRate: sample rate in Hz
    \return Error_t
    */
    Error_t init(const float *pfAudio, long long iNumSamples, float fSampleRate);

    /*! resets a Fingerprint instance
    \return Error_t
    */
    Error_t reset();

    /*! returns length of result vector to be allocated by user
    \return long long number of blocks/subfingerprints
    */
    long long getFingerprintLength() const;

    /*! returns time stamps
    \param iBlockIdx index of block
    \return float time stamp
    */
    float getTimeStamp(int iBlockIdx) const;

    /*! returns time stamps
    \param pfAxisTicks (user- allocated, to be written) length from CFingerprint::getFingerprintLength
    \return Error_t
    */
    Error_t getTimeStamps(float *pfAxisTicks) const;

    /*! performs the Fingerprint extraction writes the result
    \param piFingerprint (user-allocated, to be written, length from CFingerprint::getFingerprintLength)
    \return Error_t
    */
    Error_t compFingerprint(uint32_t *piFingerprint);


protected:
    CFingerprint(const CFingerprint &that);
    CFingerprint &operator=(const CFingerprint &c);

    Error_t init_();
    void computeMagSpectrum_();

    CBlockAudioIf *m_pCBlockAudio = 0;   //!< instantiate for blocking the resampled time domain signal
    CSubFingerprint *m_pCSubFingerprint = 0; //!< fingerprint per block
    CFft *m_pCFft = 0;  //!< fft instance

    const int m_iBlockLength = 2048, //!< fft length
        m_iHopLength = 64; //!< hop length

    float *m_pfProcBuff2 = 0; //!< temporary buffer for current spectrum
    float *m_pfProcBuff1 = 0; //!<  temporary buffer

    const float m_fProcSampleRate = 5000.F; //!< sample rate for fingerprint extraction
    long long m_iAudioLength = 0; //!< length of audio data
    float *m_pfAudioBuff = 0; //!< buffer containing audio data at fs = 5000

    bool    m_bIsInitialized = false; //!< true if initialized
};

#endif // #if !defined(__ACA_Fingerprint_HEADER_INCLUDED__)
