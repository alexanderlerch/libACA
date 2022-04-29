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
    static Error_t create(CBlockAudioIf*& pCInstance, CAudioFileIf* pCAudioFile, int iBlockLength, int iHopLength, float fSampleRate);
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
        if (!m_bIsInitialized)
            return 0;

        return m_iNumBlocks;
    };

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
    float computeTimeStamp_()
    {
        float fTimeStamp = (m_iBlockLength / 2.F + m_iCurrBlock * m_iHopLength) / m_fSampleRate;

        m_iCurrBlock++;
        return fTimeStamp;
    }

    bool m_bIsInitialized = false;
    long long m_iNumBlocks = 0,
        m_iCurrBlock = 0,
        m_iAudioLength = 0;

    int m_iBlockLength = 0,
        m_iNumChannels = 0,
        m_iHopLength = 0;

    float m_fSampleRate = 0;
};
#endif // __ToolBlockAudio_hdr__