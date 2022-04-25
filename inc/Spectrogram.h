#if !defined(__Spectrogram_hdr__)
#define __Spectrogram_hdr__

#include "ErrorDef.h"

class CAudioFileIf;
class CFft;
template <class T>
class CRingBuffer;
class CNormalizeAudio;

class CSpectrogram
{
public:

    CSpectrogram();
    virtual ~CSpectrogram();;

    /*! initializes an Spectrogram instance
    \param string strAudioFilePath complete path to audio file
    \param int iBlockLength: FFT block length in Frames
    \param int iHopLength: hop length in Frames
    \param bool bNormalize: flag if input audio should be normalized
    \return Error_t
    */
    Error_t init (const std::string strAudioFilePath, int iBlockLength, int iHopLength, bool bNormalize = true);
    
    /*! resets an Spectrogram instance
    \return Error_t
    */
    Error_t reset ();
 
    /*! return size of matrix to be allocated by user
    \param int iNumRows (number of rows, to be written) equals number of frequency bins
    \param int iNumCols (number of columns, to be written) equals number of blocks
    \return Error_t
    */
    Error_t getSpectrogramDimensions (int &iNumRows, int &iNumCols) const;

    /*! perform the Spectrogram computation
    \param float **ppfSpectrogram (user-allocated, to be written, dimensions from ::getSpectrogramDimensions)
    \return Error_t
    */
    Error_t process (float **ppfSpectrogram);

private:
    CSpectrogram(const CSpectrogram& that);

    long long m_iAudioLength,
                m_iNumBlocks;

    int m_iBlockLength,
        m_iHopLength;

    CRingBuffer<float>* m_pCRingBuffer;
    CFft* m_pCFft;
    CAudioFileIf* m_pCAudioFile;
    CNormalizeAudio* m_pCNormalize;

    float* m_pfSpectrum;
    float** m_ppfAudioData;

    bool    m_bIsInitialized;
};

#endif // #if !defined(__Spectrogram_hdr__)



