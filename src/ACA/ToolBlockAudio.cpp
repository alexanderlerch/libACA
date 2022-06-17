#include "Vector.h"
#include "Matrix.h"
#include "RingBuffer.h"
#include "AudioFileIf.h"

#include "ToolPreProc.h"

#include "ToolBlockAudio.h"


/*! \brief class for audio blocking from a file
*/
class CBlockAudioFile : public CBlockAudioIf
{
public:
    CBlockAudioFile(CAudioFileIf *pCAudioFile, int iBlockLength, int iHopLength) :
        m_pCAudioFile(pCAudioFile),
        m_pCRingBuff(0),
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
        m_pCRingBuff = new CRingBuffer<float>(iBlockLength + 1);
        CMatrix::alloc(m_ppfAudioData, m_iNumChannels, iHopLength);

        // prefill from file to read buffer
        while (m_pCRingBuff->getNumValuesInBuffer() < m_pCRingBuff->getLength() - m_iHopLength - 1)
            readFile2RingBuff();
    }

    virtual ~CBlockAudioFile()
    {
        CVector::free(m_pCRingBuff);

        CMatrix::free(m_ppfAudioData, m_iNumChannels);
    }
    bool isEndOfData() const override
    {
        assert(m_pCAudioFile);
        return m_pCAudioFile->isEof();
    }

    int getNextBlock(float *pfBlock, float *pfTimeStamp) override
    {
        if (!m_pCAudioFile || !pfBlock)
            return -1;

        // read from file to read buffer
        readFile2RingBuff();

        int iNumSamples = m_iBlockLength;
        if (m_pCAudioFile->isEof())
            iNumSamples = m_pCRingBuff->getNumValuesInBuffer();

        // get data from ringbuffer and increment read index
        m_pCRingBuff->get(pfBlock, iNumSamples);
        if (iNumSamples < m_iBlockLength)
        {
            for (int c = 0; c < m_iNumChannels; c++)
                CVector::setZero(&pfBlock[iNumSamples], static_cast<long long>(m_iBlockLength) - iNumSamples);

            iNumSamples = m_iHopLength;
        }
        m_pCRingBuff->setReadIdx(m_pCRingBuff->getReadIdx() + m_iHopLength);

        if (pfTimeStamp)
            *pfTimeStamp = getTimeStamp(m_iCurrBlock);

        m_iCurrBlock++;

        return iNumSamples;
    }

private:
    CBlockAudioFile(const CBlockAudioFile &that);     //!< disallow copy construction   
    CBlockAudioFile &operator=(const CBlockAudioFile &c);

    inline void readFile2RingBuff()
    {
        // set file read length variable
        //long long iNumSamples = std::min(m_iHopLength, m_pCRingBuff->getLength() - m_pCRingBuff->getNumValuesInBuffer() + 1);
        long long iNumSamples = m_iHopLength;

        // read data (iNumOfSamples might be updated!)
        m_pCAudioFile->readData(m_ppfAudioData, iNumSamples);

        // set buffer to zero if not written (EOF)
        if (iNumSamples < m_iHopLength)
        {
            for (int c = 0; c < m_iNumChannels; c++)
                CVector::setZero(&m_ppfAudioData[c][iNumSamples], m_iHopLength - iNumSamples);

            iNumSamples = m_iHopLength;
        }

        // downmix in case of multichannel
        CPreProc::downmix(m_ppfAudioData[0], m_ppfAudioData, m_iNumChannels, iNumSamples);

        // write data into inputbuffer
        m_pCRingBuff->putPostInc(m_ppfAudioData[0], m_iHopLength);
    }
    CAudioFileIf *m_pCAudioFile;
    CRingBuffer<float> *m_pCRingBuff;
    float **m_ppfAudioData;
};


/*! \brief class for audio blocking from a buffer 
*/
class CBlockAudioBuffer : public CBlockAudioIf
{
public:
    CBlockAudioBuffer(const float *pfAudioBuff, long long iAudioLength, int iBlockLength, int iHopLength, float fSampleRate) :
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

    bool isEndOfData() const override
    {
        return m_iAudioLength == m_iCurrIdx;
    }

    int getNextBlock(float *pfBlock, float *pfTimeStamp) override
    {
        if (!m_pfAudioData)
            return -1;

        int iNumSamplesInBlock = m_iAudioLength - m_iCurrIdx < m_iBlockLength ? static_cast<int>(m_iAudioLength - m_iCurrIdx) : m_iBlockLength;

        CVector::copy(pfBlock, &m_pfAudioData[m_iCurrIdx], iNumSamplesInBlock);
        CVector::setZero(&pfBlock[iNumSamplesInBlock], static_cast<long long>(m_iBlockLength) - iNumSamplesInBlock);

        if (pfTimeStamp)
            *pfTimeStamp = getTimeStamp(m_iCurrBlock);

        m_iCurrIdx += std::min(iNumSamplesInBlock, m_iHopLength);
        m_iCurrBlock++;

        return iNumSamplesInBlock;
    }

private:
    CBlockAudioBuffer(const CBlockAudioBuffer &that);     //!< disallow copy construction   
    CBlockAudioBuffer &operator=(const CBlockAudioBuffer &c);

    long long m_iCurrIdx = 0;
    float *m_pfAudioData = 0;
};


Error_t CBlockAudioIf::create(CBlockAudioIf *&pCInstance, CAudioFileIf *pCAudioFile, int iBlockLength, int iHopLength)
{
    if (!pCAudioFile)
        return Error_t::kFunctionInvalidArgsError;
    if (iBlockLength <= 0 || iHopLength <= 0 || iHopLength > iBlockLength)
        return Error_t::kFunctionInvalidArgsError;

    pCInstance = new CBlockAudioFile(pCAudioFile, iBlockLength, iHopLength);

    return Error_t::kNoError;
}

Error_t CBlockAudioIf::create(CBlockAudioIf *&pCInstance, const float *pfAudioBuff, long long iAudioLength, int iBlockLength, int iHopLength, float fSampleRate)
{
    if (!pfAudioBuff)
        return Error_t::kFunctionInvalidArgsError;
    if (iAudioLength <= 0 || iBlockLength <= 0 || iHopLength <= 0 || iHopLength > iBlockLength || fSampleRate <= 0)
        return Error_t::kFunctionInvalidArgsError;

    pCInstance = new CBlockAudioBuffer(pfAudioBuff, iAudioLength, iBlockLength, iHopLength, fSampleRate);

    return Error_t::kNoError;
}
