#if !defined(__ACA_Gammatone_HEADER_INCLUDED__)
#define __ACA_Gammatone_HEADER_INCLUDED__

#include <string>

#include "ErrorDef.h"

// forward declarations
class CAudioFileIf;
class CFft;
class CNormalizeAudio;
class CBlockAudioIf;
class CGammatone;


/*! \brief class for computation of a Gammatone filterbank output from either a file or a vector
*/
class CGammaToneFbIf
{
public:

    /*! initializes a GammaToneFb instance with file reading
    \param pCInstance pointer to instance to be written
    \param strAudioFilePath complete path to audio file
    \param iNumBands: number of gammatone bands
    \param fStartInHz: center frequency of the lowest band
    \return Error_t
    */
    static Error_t create(CGammaToneFbIf *&pCInstance, const std::string &strAudioFilePath, int iNumBands = 20, float fStartInHz = 100);

    /*! initializes a GammaToneFb instance from audio data
    \param pCInstance pointer to instance to be written
    \param pfAudio complete audio data
    \param iNumSamples: length of pfAudio
    \param fSampleRate: sample rate in Hz
    \param iNumBands: number of gammatone bands
    \param fStartInHz: center frequency of the lowest band
    \return Error_t
    */
    static Error_t create(CGammaToneFbIf *&pCInstance, const float *pfAudio, long long iNumSamples, float fSampleRate, int iNumBands = 20, float fStartInHz = 100);


    /*! initializes a GammaToneFb instance for block based processing
    \param pCInstance pointer to instance to be written
    \param fSampleRate: sample rate in Hz
    \param iNumBands: number of gammatone bands
    \param fStartInHz: center frequency of the lowest band
    \return Error_t
    */
    static Error_t create(CGammaToneFbIf *&pCInstance, float fSampleRate, int iNumBands = 20, float fStartInHz = 100);

    /*! destroys a GammaToneFb instance
    \param pCInstance pointer to instance to be destroyed
    \return Error_t
    */
    static Error_t destroy(CGammaToneFbIf *&pCInstance);

    /*! returns size of matrix to be allocated by user for the output
    \param iNumRows (number of bands, to be written)
    \param iNumCols (number of samples, to be written)
    \return Error_t
    */
    Error_t getOutputDimensions(long long &iNumRows, long long &iNumCols) const;

    /*! returns center frequency
    \param iBandIdx index of band the frequency is request for
    \return float
    */
    float getCenterFreq(int iBandIdx) const;

    /*! performs the GammaToneFb computation for 1 dimensional GammaToneFbs and writes the result
    \param ppfOut (user-allocated, to be written, dimensions iNumBands x iNumSamples, see CGammaToneFbIf::getOutputDimensions)
    \param pfIn input data (for real-time block processing only)
    \param iNumSamples length of input data (for real-time block processing only)
    \return Error_t
    */
    Error_t process(float **ppfOut, const float *pfIn = 0, long long iNumSamples = 0);

protected:
    CGammaToneFbIf();
    virtual ~CGammaToneFbIf();
    CGammaToneFbIf(const CGammaToneFbIf &that);
    CGammaToneFbIf &operator=(const CGammaToneFbIf &c);

    Error_t reset_(); //!< reset configuration
    Error_t init_(); //!< init configuration
    float compMidFreqs_(float fFreqLow, float fFreqHigh, int k) const; //!< compute center frequencies for all bands

    CNormalizeAudio *m_pCNormalize = 0; //!< instantiate if audio file normalization is wanted

    CBlockAudioIf *m_pCBlockAudio = 0; //!< instantiate for blocking time domain signal
    CGammatone **m_ppCGammatone = 0; //!< array of filters

    const int m_iBlockLength = 4096; //!< internal processing block length

    int m_iNumBands = 0; //!< number of gammatone bands

    float m_fSampleRate = 0; //!< sample rate
    float m_fStartInHz = 0; //!< lowest band center frequency

    float *m_pfProcBuff = 0; //!< temporary processing buffer

    bool m_bIsInitialized = false; //!< true if initialized
};

#endif // #if !defined(__ACA_Gammatone_HEADER_INCLUDED__)
