#if !defined(__ACA_Chord_HEADER_INCLUDED__)
#define __ACA_Chord_HEADER_INCLUDED__

#include <string>

#include "ErrorDef.h"

// forward declarations
class CAudioFileIf;
class CNormalizeAudio;
class CBlockAudioIf;
class CFft;
class CChordFromBlockIf;
class CViterbi;

/*! \brief class for computation of a chord progression from either a file or a vector
*/
class CChordIf
{
public:
    /*! detectable chords
    */
    enum Chords_t
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

        kNoChord,

        kNumChords
    };

    /*! initializes a Chord instance with file reading
    \param pCInstance pointer to instance to be written
    \param strAudioFilePath complete path to audio file
    \param iBlockLength: FFT block length in samples
    \param iHopLength: hop length in samples
    \return Error_t
    */
    static Error_t create(CChordIf *&pCInstance, const std::string &strAudioFilePath, int iBlockLength = 8192, int iHopLength = 2048);

    /*! initializes a Chord instance from audio data
    \param pCInstance pointer to instance to be written
    \param pfAudio complete audio data
    \param iNumSamples: length of pfAudio
    \param fSampleRate: sample rate in Hz
    \param iBlockLength: FFT block length in samples
    \param iHopLength: hop length in samples
    \return Error_t
    */
    static Error_t create(CChordIf *&pCInstance, const float *pfAudio, long long iNumSamples, float fSampleRate, int iBlockLength = 8192, int iHopLength = 2048);

    /*! destroys a Chord instance
    \param pCInstance pointer to instance to be destroyed
    \return Error_t
    */
    static Error_t destroy(CChordIf *&pCInstance);

    /*! returns length of vector to be allocated by user
    \param iNumBlocks (number of blocks, to be written)
    \return Error_t
    */
    Error_t getNumBlocks(int &iNumBlocks) const;

    /*! returns length of vector to be allocated by user
    \return int
    */
    int getNumBlocks() const;

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

    /*! performs the Chord computation for 1 dimensional Chords and writes the result
    \param peChord resulting chord indices (user-allocated, to be written, dimensions from CChordIf::getNumBlocks)
    \param bWithViterbi use HMM post-processing
    \return Error_t
    */
    Error_t compChords(Chords_t *peChord, bool bWithViterbi = true);

    /*! returns Chord name as string
    \param eChordIdx Chord index
    \return std::string
    */
    static std::string getChordString(Chords_t eChordIdx);

    /*! returns Chord index from string
    \param sChordString string describing the Chord
    \return Chords_t
    */
    static Chords_t getChordIdxFromString(std::string sChordString);

protected:
    CChordIf();
    virtual ~CChordIf();
    CChordIf(const CChordIf &that);
    CChordIf &operator=(const CChordIf &c);

    Error_t reset_(); //!< reset configuration
    Error_t init_(); //!< init configuration
    void initViterbi_();
    void computeMagSpectrum_();

    CNormalizeAudio *m_pCNormalize = 0;  //!< instantiate if audio file normalization is wanted

    CBlockAudioIf *m_pCBlockAudio = 0;   //!< instantiate for blocking time domain signal

    CChordFromBlockIf *m_pCChord = 0;  //!< instantaneous chord detection instance
    CFft *m_pCFft = 0; //!< fft instance
    CViterbi *m_pCViterbi = 0;  //!< instance for viterbi postprocessing

    int m_iBlockLength = 0, //!< fft length
        m_iHopLength = 0; //!< hop length

    float m_fSampleRate = 0; //!< sample rate

    float *m_pfProcBuff2 = 0; //!< temporary buffer for current spectrum
    float *m_pfProcBuff1 = 0; //!<  temporary buffer

    float **m_ppfChordProbs = 0; //!< chord probabilities (chrods X observations)

    bool    m_bIsInitialized = false; //!< true if initialized
};

#endif // #if !defined(__ACA_Chord_HEADER_INCLUDED__)
