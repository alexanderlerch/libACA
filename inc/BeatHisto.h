#if !defined(__ACA_BeatHisto_HEADER_INCLUDED__)
#define __ACA_BeatHisto_HEADER_INCLUDED__

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
        kBeatHistoCorr, //!< use correlation based method
        kBeatHistoFft, //!< use FFT based method

        kNumBeatHistoCompModes
    };

    /*! initializes a BeatHisto instance with file reading
    \param pCInstance pointer to instance to be written
    \param strAudioFilePath complete path to audio file
    \param iBlockLength: FFT block length in samples
    \param iHopLength: hop length in samples
    \return Error_t
    */
    static Error_t create(CBeatHistoIf *&pCInstance, const std::string &strAudioFilePath, int iBlockLength = 1024, int iHopLength = 8);

    /*! initializes a BeatHisto instance from audio data
    \param pCInstance pointer to instance to be written
    \param pfAudio complete audio data
    \param iNumSamples: length of pfAudio
    \param fSampleRate: sample rate in Hz
    \param iBlockLength: FFT block length in samples
    \param iHopLength: hop length in samples
    \return Error_t
    */
    static Error_t create(CBeatHistoIf *&pCInstance, const float *pfAudio, long long iNumSamples, float fSampleRate, int iBlockLength = 1024, int iHopLength = 8);

    /*! destroys a BeatHisto instance
    \param pCInstance pointer to instance to be destroyed
    \return Error_t
    */
    static Error_t destroy(CBeatHistoIf *&pCInstance);

    /*! returns length of the frequency axis of the beat histogram
    \param iNumBeatHistoBins (number of bins, to be written)
    \param eBeatHistoComp (computation method used in CBeatHistoIf::compBeatHisto)
    \return Error_t
    */
    Error_t getNumBins(int &iNumBeatHistoBins, BeatHisto_t eBeatHistoComp) const;

    /*! returns length of the frequency axis of the beat histogram
    \param eBeatHistoComp (computation method used in CBeatHistoIf::compBeatHisto)
    \return ing number bins
    */
    int getNumBins(BeatHisto_t eBeatHistoComp) const;

    /*! returns axis ticks
    \param pfAxisTicks (to be written) in BPM, length equals iNumBins
    \param eBeatHistoComp (computation method used in CBeatHistoIf::compBeatHisto)
    \return Error_t
    */
    Error_t getBeatHistoAxisTicks(float *pfAxisTicks, BeatHisto_t eBeatHistoComp) const;

    /*! performs the BeatHisto computation for 1 dimensional BeatHistos and writes the result
    \param pfBeatHisto (user-allocated, to be written, dimensions from CBeatHistoIf::getNumBins)
    \param eBeatHistoComp computation method
    \return Error_t
    */
    Error_t compBeatHisto(float *pfBeatHisto, BeatHisto_t eBeatHistoComp = kBeatHistoFft);

protected:
    explicit CBeatHistoIf(int iBlockLength, int iHopLength, float fSampleRate) :
        m_iBlockLength(iBlockLength),
        m_iHopLength(iHopLength),
        m_fSampleRate(fSampleRate)
    {};
    virtual ~CBeatHistoIf();
    CBeatHistoIf(const CBeatHistoIf &that);
    CBeatHistoIf &operator=(const CBeatHistoIf &c);

    Error_t init_(const std::string &strAudioFilePath);//!< init configuration
    Error_t init_(const float *pfAudio, long long iNumSamples, float fSampleRate);//!< init configuration

    Error_t reset_(); //!< reset configuration

    void compHistoRange_(int &istartIdx, int &istopIdx, BeatHisto_t eBeatHistoComp) const;

    CFft *m_pCFft = 0;  //!< fft instance
    CCcf *m_pCCcf = 0;  //!< correlation instance

    CNoveltyIf *m_pCNovelty = 0; //!< novelty function extraction instance

    float *m_pfNovelty = 0;  //!< buffer to hold extracted novelty function
    float *m_pfProcBuff = 0; //!< temporary processing buffer
    float *m_pfBeatHisto = 0; //!< result buffer

    int m_iBlockLength = 0, //!< processing block length for novelty
        m_iHopLength = 0;  //!< hop length for novelty

    const int m_iBeatHistoLength = 65536; //!< default length of FFT

    float m_fSampleRate = 0;    //!< sample rate

    bool    m_bIsInitialized = false;   //!< true if initialized
};

#endif // #if !defined(__ACA_BeatHisto_HEADER_INCLUDED__)
