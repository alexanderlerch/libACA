#if !defined(__Gammatone_hdr__)
#define __Gammatone_hdr__

#include <string>

#include "ErrorDef.h"

// forward declarations
class CAudioFileIf;
class CFft;
class CNormalizeAudio;
class CBlockAudioIf;
template <class T> class CFilter;

/*! \brief class for a single gammtone filter
*/
class CGammatone
{
public:
    CGammatone();
    virtual ~CGammatone();

    /*! initializes a Gammatone instance with file reading
    \param pCInstance pointer to instance to be written
    \return Error_t
    */
    Error_t init(float fFreqCenter, float fSampleRate);

    /*! performs the Gammatone filter computation
    \param pfOutput filter result (user-allocated, to be written, length iNumSamples)
    \param pfInput input data of length iNumSamples
    \param iNumSamples length of buffers
    return Error_t
    */
    Error_t process(float* pfOutput, const float* pfInput, long long iNumSamples);

    /*! clears internal buffers and sets parameters to default
    \return Error_t
    */
    Error_t reset();

private:
    CGammatone(const CGammatone& that);

    /*! compute the filter coeffs
    \param iOrder filter order
    return float alpha
    */
    void calcFilterCoeffs_(int iOrder = 1);

    enum
    {
        kNumCoeffs = 3,
        kNumFilters = 4
    };

    float m_fFreqCenter = 0;
    float m_fSampleRate = 0;

    float m_aafCoeffB[kNumFilters][kNumCoeffs] = { 0 };
    float m_aafCoeffA[kNumFilters][kNumCoeffs] = { 0 };

    CFilter<float>* m_apCFilter[kNumFilters] = { 0 };

    bool m_bIsInitialized = false;
};

/*! \brief class for computation of a magnitude GammaToneFb from either a file or a vector
*/
class CGammaToneFbIf
{
public:

    /*! initializes a GammaToneFb instance with file reading
    \param pCInstance pointer to instance to be written
    \param eGammaToneFbIdx as defined in GammaToneFb_t
    \param strAudioFilePath complete path to audio file
    \param iBlockLength: FFT block length in Frames
    \param iHopLength: hop length in Frames
    \return Error_t
    */
    static Error_t create(CGammaToneFbIf*& pCInstance, const std::string& strAudioFilePath, int iNumBands = 20, float fStartInHz = 100);

    /*! initializes a GammaToneFb instance from audio data
    \param pCInstance pointer to instance to be written
    \param eGammaToneFbIdx as defined in GammaToneFb_t
    \param pfAudio complete audio data
    \param iNumFrames: length of pfAudio
    \param fSampleRate: sample rate in Hz
    \param iBlockLength: FFT block length in Frames
    \param iHopLength: hop length in Frames
    \return Error_t
    */
    static Error_t create(CGammaToneFbIf*& pCInstance, const float* pfAudio, long long iNumFrames, float fSampleRate, int iNumBands = 20, float fStartInHz = 100);

    /*! destroys a GammaToneFb instance
    \param pCInstance pointer to instance to be destroyed
    \return Error_t
    */
    static Error_t destroy(CGammaToneFbIf*& pCInstance);

    /*! returns size of matrix to be allocated by user
    \param iNumRows (number of bands, to be written)
    \param iNumCols (number of samples, to be written)
    \return Error_t
    */
    Error_t getOutputDimensions(long long& iNumRows, long long& iNumColumns) const;

    /*! returns size of matrix to be allocated by user
    \param iNumRows (number of bands, to be written)
    \param iNumCols (number of samples, to be written)
    \return Error_t
    */
    float getCenterFreq(int iBandIdx) const;

    /*! performs the GammaToneFb computation for 1 dimensional GammaToneFbs and writes the result
    \param pfGammaToneFb (user-allocated, to be written, dimensions from CGammaToneFbIf::getNumBlocks)
    \param pbIsOnset (user-allocated, to be written, true if onset)
    \return Error_t
    */
    Error_t process(float** ppfOutput);

protected:
    CGammaToneFbIf();
    virtual ~CGammaToneFbIf();
    CGammaToneFbIf(const CGammaToneFbIf& that);

    Error_t reset_();                    //!< reset configuration
    Error_t init_();//!< init configuration
    float compMidFreqs_(float fFreqLow, float fFreqHigh, int k) const;
        
    CNormalizeAudio* m_pCNormalize = 0;  //!< instantiate if audio file normalization is wanted

    CBlockAudioIf* m_pCBlockAudio = 0;   //!< instantiate for blocking time domain signal
    CGammatone** m_ppCGammatone = 0;

    const int m_iBlockLength = 4096;          //!< internal processing block length

    int m_iNumBands = 0;

    float m_fSampleRate = 0;            //!< sample rate
    float m_fStartInHz = 0;

    float *m_pfProcessBuff = 0;

    bool    m_bIsInitialized = false;   //!< true if initialized
};

#endif // #if !defined(__Gammatone_hdr__)



