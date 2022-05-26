#if !defined(__BeatHisto_hdr__)
#define __BeatHisto_hdr__

#include <string>

#include "ErrorDef.h"

// forward declarations
class CFft;
class CCcf;
class CNoveltyIf;

/*! \brief class for computation of a BeatHistogram from either a file or a vector
*/
class CBeatHistoIf
{
public:
    /*! \brief enum to index all BeatHistogram computations
    */
    enum BeatHisto_t
    {
        kBeatHistoCorr,
        kBeatHistoFft,

        kNumBeatHistoComputations
    };

    /*! initializes a BeatHisto instance with file reading
    \param pCInstance pointer to instance to be written
    \param strAudioFilePath complete path to audio file
    \param iBlockLength: FFT block length in Frames
    \param iHopLength: hop length in Frames
    \return Error_t
    */
    static Error_t create(CBeatHistoIf*& pCInstance, const std::string& strAudioFilePath, int iBlockLength = 1024, int iHopLength = 8);

    /*! initializes a BeatHisto instance from audio data
    \param pCInstance pointer to instance to be written
    \param pfAudio complete audio data
    \param iNumFrames: length of pfAudio
    \param fSampleRate: sample rate in Hz
    \param iBlockLength: FFT block length in Frames
    \param iHopLength: hop length in Frames
    \return Error_t
    */
    static Error_t create(CBeatHistoIf*& pCInstance, const float* pfAudio, long long iNumFrames, float fSampleRate, int iBlockLength = 1024, int iHopLength = 8);

    /*! destroys a BeatHisto instance
    \param pCInstance pointer to instance to be destroyed
    \return Error_t
    */
    static Error_t destroy(CBeatHistoIf*& pCInstance);

    /*! returns size of matrix to be allocated by user
    \param iLengthOfBeatHisto (number of bins, to be written) 
    \return Error_t
    */
    Error_t getNumBins(int& iLengthOfBeatHisto, BeatHisto_t eBeatHistoComp) const;
    int getNumBins(BeatHisto_t eBeatHistoComp) const;

    /*! returns axis ticks
    \param pfAxisTicks (to be written) in BPM, length equals iNumBins
    \return Error_t
    */
    Error_t getBeatHistoAxisTicks(float* pfAxisTicks, BeatHisto_t eBeatHistoComp) const;

    /*! performs the BeatHisto computation for 1 dimensional BeatHistos and writes the result
    \param pfBeatHisto (user-allocated, to be written, dimensions from CBeatHistoIf::getNumBins)
    \param pbIsOnset (user-allocated, to be written, true if onset)
    \return Error_t
    */
    Error_t getBeatHisto(float* pfBeatHisto, BeatHisto_t eBeatHistoComp = kBeatHistoFft);

protected:
    explicit CBeatHistoIf(int iBlockLength, int iHopLength, float fSampleRate) :m_iBlockLength(iBlockLength), m_iHopLength(iHopLength), m_fSampleRate(fSampleRate){};
    virtual ~CBeatHistoIf();
    CBeatHistoIf(const CBeatHistoIf& that);

    Error_t reset_();                    //!< reset configuration
    Error_t init_(const std::string& strAudioFilePath);//!< init configuration
    Error_t init_(const float* pfAudio, long long iNumFrames, float fSampleRate);//!< init configuration
    //int compBeatHistoLength_(long long iLengthNovelty) const;
    void compHistoRange(int& iStartIdx, int& iStopIdx, BeatHisto_t eBeatHistoComp) const;

    CFft* m_pCFft = 0;                   //!< fft instance
    CCcf* m_pCCcf = 0;                  //!< correlation instance

    CNoveltyIf* m_pCNovelty = 0;

    float* m_pfNovelty = 0;
    float* m_pfProcessBuff = 0;
    float* m_pfBeatHisto = 0;
    
    int m_iBlockLength = 0,             //!< fft length
        m_iHopLength = 0;               //!< hop length

    const int m_iBeatHistoLength = 65536;

    float m_fSampleRate = 0;            //!< sample rate

    bool    m_bIsInitialized = false;   //!< true if initialized
};

#endif // #if !defined(__BeatHisto_hdr__)



