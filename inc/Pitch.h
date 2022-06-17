#if !defined(__ACA_Pitch_HEADER_INCLUDED__)
#define __ACA_Pitch_HEADER_INCLUDED__

#include <string>

#include "ErrorDef.h"

// forward declarations
class CAudioFileIf;
class CFft;
class CNormalizeAudio;
class CBlockAudioIf;
class CPitchFromBlockIf;
class CPitchAuditory;

/*! \brief class for computation of a magnitude Pitch from either a file or a vector
*/
class CPitchIf
{
public:
    /*! \brief enum to index all Pitch extractorss
    */
    enum PitchExtractors_t
    {
        kPitchSpectralAcf,
        kPitchSpectralHps,

        kPitchTimeAcf,
        kPitchTimeAmdf,
        kPitchTimeAuditory,
        kPitchTimeZeroCrossings,

        kNumPitchExtractors
    };

    /*! initializes a Pitch instance with file reading
    \param pCInstance pointer to instance to be written
    \param ePitchIdx as defined in PitchExtractors_t
    \param strAudioFilePath complete path to audio file
    \param iBlockLength: FFT block length in samples
    \param iHopLength: hop length in samples
    \return Error_t
    */
    static Error_t create(CPitchIf *&pCInstance, PitchExtractors_t ePitchIdx, const std::string &strAudioFilePath, int iBlockLength = 2048, int iHopLength = 1024);

    /*! initializes a Pitch instance from audio data
    \param pCInstance pointer to instance to be written
    \param ePitchIdx as defined in PitchExtractors_t
    \param pfAudio complete audio data
    \param iNumSamples: length of pfAudio
    \param fSampleRate: sample rate in Hz
    \param iBlockLength: FFT block length in samples
    \param iHopLength: hop length in samples
    \return Error_t
    */
    static Error_t create(CPitchIf *&pCInstance, PitchExtractors_t ePitchIdx, const float *pfAudio, long long iNumSamples, float fSampleRate, int iBlockLength = 2048, int iHopLength = 1024);

    /*! destroys a Pitch instance
    \param pCInstance pointer to instance to be destroyed
    \return Error_t
    */
    static Error_t destroy(CPitchIf *&pCInstance);

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

    /*! performs the Pitch computation for 1 dimensional Pitchs and writes the result
    \param pfPitch (user-allocated, to be written, dimensions from CPitchIf::getPitchDimensions)
    \return Error_t
    */
    Error_t compF0(float *pfPitch);

    /*! returns Pitch name as string
    \param ePitchIdx Pitch index
    \return std::string
    */
    static std::string getPitchString(PitchExtractors_t ePitchIdx);

    /*! returns Pitch index from string
    \param sPitchString string describing the Pitch
    \return PitchExtractors_t
    */
    static PitchExtractors_t getPitchIdxFromString(std::string sPitchString);

protected:
    CPitchIf();
    virtual ~CPitchIf();
    CPitchIf(const CPitchIf &that);
    CPitchIf &operator=(const CPitchIf &c);

    Error_t reset_(); //!< reset configuration
    Error_t init_(PitchExtractors_t ePitchIdx); //!< init configuration
    bool isPitchExtractorSpectral_(PitchExtractors_t ePitchIdx);
    void computeMagSpectrum_();

    CNormalizeAudio *m_pCNormalize = 0; //!< instantiate if audio file normalization is wanted

    CBlockAudioIf *m_pCBlockAudio = 0; //!< instantiate for blocking time domain signal

    CPitchFromBlockIf *m_pCPitch = 0; //!< class for extracting f0 from block of data

    CPitchAuditory *m_pCAuditory = 0; //!< instace for auditory pitch tracking (the other pitch trackers don't need a time dependent implementation)

    CFft *m_pCFft = 0; //!< fft instance

    int m_iBlockLength = 0, //!< fft length
        m_iHopLength = 0; //!< hop length

    float m_fSampleRate = 0;  //!< sample rate

    float *m_pfProcBuff2 = 0; //!< temporary buffer for current spectrum
    float *m_pfProcBuff1 = 0; //!<  temporary buffer

    bool    m_bIsInitialized = false; //!< true if initialized
};

#endif // #if !defined(__ACA_Pitch_HEADER_INCLUDED__)
