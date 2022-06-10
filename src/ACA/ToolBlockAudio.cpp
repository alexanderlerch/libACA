#include "Vector.h"
#include "Matrix.h"
#include "RingBuffer.h"
#include "AudioFileIf.h"
#include "ToolPreProc.h"

#include "ToolBlockAudio.h"


class CBlockAudioFile : public CBlockAudioIf
{
public:
    CBlockAudioFile(CAudioFileIf* pCAudioFile, int iBlockLength, int iHopLength) :
        m_pCAudioFile(pCAudioFile),
        m_pCRingBuffer(0),
        m_ppfAudioData(0)
    {
        m_iBlockLength = iBlockLength;
        m_iHopLength = iHopLength;
            
        CAudioFileIf::FileSpec_t stFileSpec;

        // get length of audio file
        m_pCAudioFile->getLength(m_iAudioLength);
        m_pCAudioFile->getFileSpec(stFileSpec);
        m_iNumChannels = stFileSpec.iNumChannels;
        m_fSampleRate = stFileSpec.fSampleRateInHz;

        // compute number of blocks
        m_iNumBlocks = m_iAudioLength / m_iHopLength + 1;

        // initialize read buffers
        m_pCRingBuffer = new CRingBuffer<float>(iBlockLength+1);
        CMatrix::alloc(m_ppfAudioData, m_iNumChannels, iHopLength);
        
        // prefill from file to read buffer
        while(m_pCRingBuffer->getNumValuesInBuffer() < m_pCRingBuffer->getLength() - m_iHopLength - 1)
            readFile2RingBuff();
    }

    virtual ~CBlockAudioFile()
    {
        CVector::free(m_pCRingBuffer);

        CMatrix::free(m_ppfAudioData, m_iNumChannels);
    }
    bool IsEndOfData() const override
    {
        assert(m_pCAudioFile);
        return m_pCAudioFile->isEof();
    }

    int getNextBlock(float* pfBlock, float* pfTimeStamp) override
    {
        if (!m_pCAudioFile || !pfBlock)
            return -1;

        // read from file to read buffer
        readFile2RingBuff();

        int iNumFrames = m_iBlockLength;
        if (m_pCAudioFile->isEof())
            iNumFrames = m_pCRingBuffer->getNumValuesInBuffer();

        // get data from ringbuffer and increment read index
        m_pCRingBuffer->get(pfBlock, iNumFrames);
        if (iNumFrames < m_iBlockLength)
        {
            for (int c = 0; c < m_iNumChannels; c++)
                CVector::setZero(&pfBlock[iNumFrames], static_cast<long long>(m_iBlockLength) - iNumFrames);

            iNumFrames = m_iHopLength;
        }
        m_pCRingBuffer->setReadIdx(m_pCRingBuffer->getReadIdx() + m_iHopLength);

        if (pfTimeStamp)
            *pfTimeStamp = getTimeStamp(m_iCurrBlock);
        
        m_iCurrBlock++;

        return iNumFrames;
    }

private:
    CBlockAudioFile(const CBlockAudioFile& that);     //!< disallow copy construction   
    CBlockAudioFile& operator=(const CBlockAudioFile& c);
    
    inline void readFile2RingBuff()
    {        
        // set file read length variable
        //long long iNumFrames = std::min(m_iHopLength, m_pCRingBuffer->getLength() - m_pCRingBuffer->getNumValuesInBuffer() + 1);
        long long iNumFrames = m_iHopLength;

        // read data (iNumOfFrames might be updated!)
        m_pCAudioFile->readData(m_ppfAudioData, iNumFrames);

        // set buffer to zero if not written (EOF)
        if (iNumFrames < m_iHopLength)
        {
            for (int c = 0; c < m_iNumChannels; c++)
                CVector::setZero(&m_ppfAudioData[c][iNumFrames], m_iHopLength - iNumFrames);
            
            iNumFrames = m_iHopLength;
        }

        // downmix in case of multichannel
        CPreProc::downmix(m_ppfAudioData[0], m_ppfAudioData, m_iNumChannels, iNumFrames);

        // write data into inputbuffer
        m_pCRingBuffer->putPostInc(m_ppfAudioData[0], m_iHopLength);
    }
    CAudioFileIf* m_pCAudioFile;
    CRingBuffer<float>* m_pCRingBuffer;
    float** m_ppfAudioData;
};




class CBlockAudioBuffer : public CBlockAudioIf
{
public:
    CBlockAudioBuffer(const float* pfAudioBuff, long long iAudioLength, int iBlockLength, int iHopLength, float fSampleRate) :
        m_iCurrIdx(0),
        m_pfAudioData(0)
    {
        m_fSampleRate = fSampleRate;

        m_iBlockLength = iBlockLength;
        m_iHopLength = iHopLength;
        m_iAudioLength = iAudioLength;

        // compute number of blocks
        m_iNumBlocks = m_iAudioLength / m_iHopLength + 1;

        // initialize read buffers
        CVector::alloc(m_pfAudioData, m_iAudioLength);
        CVector::copy(m_pfAudioData, pfAudioBuff, m_iAudioLength);
    }

    virtual ~CBlockAudioBuffer()
    {
        CVector::free(m_pfAudioData);

        m_iAudioLength = 0;
        m_iCurrIdx = 0;
    }

    bool IsEndOfData() const override
    {
        return m_iAudioLength == m_iCurrIdx;
    }

    int getNextBlock(float* pfBlock, float *pfTimeStamp) override
    {
        if (!m_pfAudioData)
            return -1;

        int iNumFramesInBlock = m_iAudioLength - m_iCurrIdx < m_iBlockLength ? static_cast<int>(m_iAudioLength - m_iCurrIdx) : m_iBlockLength;

        CVector::copy(pfBlock, &m_pfAudioData[m_iCurrIdx], iNumFramesInBlock);
        CVector::setZero(&pfBlock[iNumFramesInBlock], static_cast<long long>(m_iBlockLength) - iNumFramesInBlock);

        if (pfTimeStamp)
            *pfTimeStamp = getTimeStamp(m_iCurrBlock);

        m_iCurrIdx += std::min(iNumFramesInBlock, m_iHopLength);
        m_iCurrBlock++;

        return iNumFramesInBlock;
    }

private:
    CBlockAudioBuffer(const CBlockAudioBuffer& that);     //!< disallow copy construction   
    CBlockAudioBuffer& operator=(const CBlockAudioBuffer& c);

    long long m_iCurrIdx = 0;
    float* m_pfAudioData = 0;
};


Error_t CBlockAudioIf::create(CBlockAudioIf*& pCInstance, CAudioFileIf* pCAudioFile, int iBlockLength, int iHopLength)
{
    if (!pCAudioFile)
        return Error_t::kFunctionInvalidArgsError;
    if (iBlockLength <= 0 || iHopLength <= 0 || iHopLength > iBlockLength)
        return Error_t::kFunctionInvalidArgsError;

    pCInstance = new CBlockAudioFile(pCAudioFile, iBlockLength, iHopLength);

    return Error_t::kNoError;
}

Error_t CBlockAudioIf::create(CBlockAudioIf*& pCInstance, const float* pfAudioBuff, long long iAudioLength, int iBlockLength, int iHopLength, float fSampleRate)
{
    if (!pfAudioBuff)
        return Error_t::kFunctionInvalidArgsError;
    if (iAudioLength <= 0 || iBlockLength <= 0 || iHopLength <= 0 || iHopLength > iBlockLength || fSampleRate <= 0)
        return Error_t::kFunctionInvalidArgsError;

    pCInstance = new CBlockAudioBuffer(pfAudioBuff, iAudioLength, iBlockLength, iHopLength, fSampleRate);

    return Error_t::kNoError;
}
