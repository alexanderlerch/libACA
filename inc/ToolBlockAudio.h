#if !defined(__ToolBlockAudio_hdr__)
#define __ToolBlockAudio_hdr__

#include <cassert>

#include "ErrorDef.h"

class CAudioFileIf; //!< forward declaration

/*! \brief class for audio blocking, supports both and audio file or an audio buffer as source to block
*/
class CBlockAudioIf
{
public:

    /*! creates an instance for blocking from file
    \param CBlockAudioIf*& pCInstance pointer to instance to be written
    \param CAudioFileIf* pCAudioFile pointer to audio file
    \param int iBlockLength block length in frames
    \param int iHopLength hop length in frames
    \param float fSampleRate sample rate in Hz
    \return float
    */
    static Error_t create(CBlockAudioIf*& pCInstance, CAudioFileIf* pCAudioFile, int iBlockLength, int iHopLength);

    /*! creates an instance for blocking from vector
    \param CBlockAudioIf*& pCInstance pointer to instance to be written
    \param const float *pfAudioBuff buffer with audio data
    \param long long iAudioLength length of pfAudioBuff in frames
    \param int iBlockLength block length in frames
    \param int iHopLength hop length in frames
    \param float fSampleRate sample rate in Hz
    \return float
    */
    static Error_t create(CBlockAudioIf*& pCInstance, const float *pfAudioBuff, long long iAudioLength, int iBlockLength, int iHopLength, float fSampleRate);

    /*! destroys a block audio instance
    \return Error_t
    */
    static Error_t destroy(CBlockAudioIf*& pCInstance) 
    {
        delete pCInstance;
        pCInstance = 0;

        return Error_t::kNoError;
    };

    /*! returns number of blocks
    \return long long number of blocks for current audio 
    */
    long long getNumBlocks() const
    {
        return m_iNumBlocks;
    };

    /*! returns time stamp in s for a given block index
    \param long long iBlockIdx index for requested time stamp
    \return float
    */
    float getTimeStamp(long long iBlockIdx)
    {
        return (m_iBlockLength / 2.F + iBlockIdx * m_iHopLength) / m_fSampleRate;
    }

    /*! checks for end of data
    \return bool true if end of audio data is reached
    */
    virtual bool IsEndOfData() const = 0;

    /*! retrieves audio data for next block
    \param float *pfBlock (to be written) of length blocklength
    \param float *pfTimeStampInS (to be written, optional) for current time stamp
    \return Error_t
    */
    virtual Error_t getNextBlock(float* pfBlock, float *pfTimeStampInS = 0) = 0;

protected:
    CBlockAudioIf() {};
    virtual ~CBlockAudioIf() {};

    long long m_iNumBlocks = 0,     //!< number of blocks
        m_iCurrBlock = 0,           //!< current block index
        m_iAudioLength = 0;         //!< length of audio buffer

    int m_iBlockLength = 0,         //!< block length
        m_iNumChannels = 0,         //!< number of channels
        m_iHopLength = 0;           //!< hop length

    float m_fSampleRate = 0;        //!< sample rate
};
#endif // __ToolBlockAudio_hdr__