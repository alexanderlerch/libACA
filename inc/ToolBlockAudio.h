#if !defined(__ACA_ToolBlockAudio_HEADER_INCLUDED__)
#define __ACA_ToolBlockAudio_HEADER_INCLUDED__

#include "ErrorDef.h"

class CAudioFileIf; //!< forward declaration

/*! \brief class for audio blocking, supports both an audio file or an audio buffer as source to block
*/
class CBlockAudioIf
{
public:

    /*! creates an instance for blocking from file
    \param pCInstance pointer to instance to be written
    \param pCAudioFile pointer to audio file
    \param iBlockLength block length in samples
    \param iHopLength hop length in samples
    \return float
    */
    static Error_t create(CBlockAudioIf *&pCInstance, CAudioFileIf *pCAudioFile, int iBlockLength, int iHopLength);

    /*! creates an instance for blocking from vector
    \param pCInstance pointer to instance to be written
    \param pfAudioBuff buffer with audio data
    \param iAudioLength length of pfAudioBuff in samples
    \param iBlockLength block length in samples
    \param iHopLength hop length in samples
    \param fSampleRate sample rate in Hz
    \return float
    */
    static Error_t create(CBlockAudioIf *&pCInstance, const float *pfAudioBuff, long long iAudioLength, int iBlockLength, int iHopLength, float fSampleRate);

    /*! destroys a block audio instance
    \param pCInstance pointer to instance to be written
    \return Error_t
    */
    static Error_t destroy(CBlockAudioIf *&pCInstance)
    {
        delete pCInstance;
        pCInstance = 0;

        return Error_t::kNoError;
    };

    /*! returns number of blocks
    \return number of blocks for current audio
    */
    long long getNumBlocks() const
    {
        return m_iNumBlocks;
    };

    /*! returns number of samples
    \return number of samples for current audio
    */
    long long getLengthInSamples() const
    {
        return m_iAudioLength;
    };

    /*! returns time stamp in s for a given block index
    \param iBlockIdx index for requested time stamp
    \return float
    */
    float getTimeStamp(long long iBlockIdx)
    {
        return (m_iBlockLength / 2.F + iBlockIdx * m_iHopLength) / m_fSampleRate;
    }

    /*! checks for end of data
    \return bool true if end of audio data is reached
    */
    virtual bool isEndOfData() const = 0;

    /*! retrieves audio data for next block
    \param pfBlock (to be written) of length blocklength
    \param pfTimeStampInS (to be written, optional) for current time stamp
    \return int iNumsamplesRead
    */
    virtual int getNextBlock(float *pfBlock, float *pfTimeStampInS = 0) = 0;

protected:
    CBlockAudioIf() {};
    virtual ~CBlockAudioIf() {};
    CBlockAudioIf(const CBlockAudioIf &that);
    CBlockAudioIf &operator=(const CBlockAudioIf &c);

    long long m_iNumBlocks = 0, //!< number of blocks
        m_iCurrBlock = 0, //!< current block index
        m_iAudioLength = 0; //!< length of audio buffer

    int m_iBlockLength = 0, //!< block length
        m_iNumChannels = 0, //!< number of channels
        m_iHopLength = 0; //!< hop length

    float m_fSampleRate = 0; //!< sample rate
};

#endif // __ACA_ToolBlockAudio_HEADER_INCLUDED__
