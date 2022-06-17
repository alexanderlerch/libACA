#if !defined(__ACA_Novelty_HEADER_INCLUDED__)
#define __ACA_Novelty_HEADER_INCLUDED__

#include <string>

#include "ErrorDef.h"

// forward declarations
class CAudioFileIf;
class CFft;
class CNormalizeAudio;
class CBlockAudioIf;
class CNoveltyFromBlockIf;
class CMovingAverage;

/*! \brief class for computation of a magnitude Novelty from either a file or a vector
*/
class CNoveltyIf
{
public:
    /*! \brief enum to index all Noveltys
    * note: update CNoveltyIf::isNoveltySpectral_() when modifying
    */
    enum Novelty_t
    {
        kNoveltyFlux,
        kNoveltyHainsworth,
        kNoveltyLaroche,

        kNumNoveltyFunctions
    };

    /*! initializes a Novelty instance with file reading
    \param pCInstance pointer to instance to be written
    \param eNoveltyIdx as defined in Novelty_t
    \param strAudioFilePath complete path to audio file
    \param iBlockLength: FFT block length in samples
    \param iHopLength: hop length in samples
    \return Error_t
    */
    static Error_t create(CNoveltyIf *&pCInstance, Novelty_t eNoveltyIdx, const std::string &strAudioFilePath, int iBlockLength = 4096, int iHopLength = 512);

    /*! initializes a Novelty instance from audio data
    \param pCInstance pointer to instance to be written
    \param eNoveltyIdx as defined in Novelty_t
    \param pfAudio complete audio data
    \param iNumSamples: length of pfAudio
    \param fSampleRate: sample rate in Hz
    \param iBlockLength: FFT block length in samples
    \param iHopLength: hop length in samples
    \return Error_t
    */
    static Error_t create(CNoveltyIf *&pCInstance, Novelty_t eNoveltyIdx, const float *pfAudio, long long iNumSamples, float fSampleRate, int iBlockLength = 4096, int iHopLength = 512);

    /*! destroys a Novelty instance
    \param pCInstance pointer to instance to be destroyed
    \return Error_t
    */
    static Error_t destroy(CNoveltyIf *&pCInstance);

    /*! returns size of matrix to be allocated by user
    \param iNumBlocks (number of blocks, to be written) equals number of hops
    \return Error_t
    */
    Error_t getNumBlocks(int &iNumBlocks) const;

    /*! returns size of matrix to be allocated by user
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

    /*! performs the Novelty computation for 1 dimensional Noveltys and writes the result
    \param pfNovelty (user-allocated, to be written, dimensions from CNoveltyIf::getNumBlocks)
    \param pbisOnset (user-allocated, to be written, true if onset)
    \return Error_t
    */
    Error_t compNovelty(float *pfNovelty, bool *pbisOnset = 0);

    /*! returns Novelty name as string
    \param eNoveltyIdx Novelty index
    \return std::string
    */
    static std::string getNoveltyString(Novelty_t eNoveltyIdx);

    /*! returns Novelty index from string
    \param sNoveltyString string describing the Novelty
    \return Novelty_t
    */
    static Novelty_t getNoveltyIdxFromString(std::string sNoveltyString);

protected:
    CNoveltyIf();
    virtual ~CNoveltyIf();
    CNoveltyIf(const CNoveltyIf &that);
    CNoveltyIf &operator=(const CNoveltyIf &c);

    Error_t reset_();  //!< reset configuration
    Error_t init_(Novelty_t eNoveltyIdx); //!< init configuration
    void computeMagSpectrum_();

    CNormalizeAudio *m_pCNormalize = 0; //!< instantiate if audio file normalization is wanted

    CBlockAudioIf *m_pCBlockAudio = 0; //!< instantiate for blocking time domain signal

    CNoveltyFromBlockIf *m_pCNovelty = 0; //!< instance for instantaneous novelty extraction

    CFft *m_pCFft = 0; //!< fft instance

    CMovingAverage *m_pCLpFilter = 0; //!< smoothing filter handle

    int m_iBlockLength = 0, //!< fft length
        m_iHopLength = 0; //!< hop length

    float m_fSampleRate = 0; //!< sample rate

    float *m_pfProcBuff2 = 0; //!< temporary buffer for current spectrum
    float *m_pfProcBuff1 = 0; //!<  temporary buffer

    bool    m_bIsInitialized = false; //!< true if initialized
};

#endif // #if !defined(__ACA_Novelty_HEADER_INCLUDED__)
