
#include "helper/Util.h"
#include "helper/Ringbuffer.h"

#include "ToolPreProc.h"
#include "AudioFileIf.h"
#include "Fft.h"
#include "Spectrogram.h"

CSpectrogram::CSpectrogram() :
    m_bIsInitialized(false),
    m_pCRingBuffer(0),
    m_pCFft(0),
    m_pCAudioFile(0),
    m_pCNormalize(0),
    m_pfSpectrum(0),
    m_ppfAudioData(0),
    m_iAudioLength(0),
    m_iBlockLength(0),
    m_iHopLength(0),
    m_iNumBlocks(0)
{
    reset ();
}

inline CSpectrogram::~CSpectrogram()
{
    reset();
}


Error_t CSpectrogram::init(std::string strAudioFilePath, int iBlockLength, int iHopLength, bool bNormalize)
{
    if (strAudioFilePath.empty())
        return Error_t::kFunctionInvalidArgsError;
    if (iBlockLength <= 2 || !CUtil::isPowOf2(iBlockLength))
        return Error_t::kFunctionInvalidArgsError;
    if (iHopLength <= 0 || iHopLength > iBlockLength)
        return Error_t::kFunctionInvalidArgsError;

    this->reset();

    CAudioFileIf::FileSpec_t stFileSpec;
    CAudioFileIf::create(m_pCAudioFile);
    m_pCAudioFile->openFile(strAudioFilePath, CAudioFileIf::kFileRead);
    if (!m_pCAudioFile->isOpen())
    {
        CAudioFileIf::destroy(m_pCAudioFile);
        return Error_t::kFileOpenError;
    }
    m_pCAudioFile->getFileSpec(stFileSpec);

    // get length of audio file
    m_pCAudioFile->getLength(m_iAudioLength);

    if (bNormalize)
        m_pCNormalize = new CNormalizeAudio(m_pCAudioFile);

    // set length variables
    m_iBlockLength = iBlockLength;
    m_iHopLength = iHopLength;

    // compute number of blocks
    m_iNumBlocks = m_iAudioLength / m_iHopLength + 1;

    // initialize read buffers
    m_pCRingBuffer = new CRingBuffer<float>(m_iBlockLength);
    m_ppfAudioData = new float* [stFileSpec.iNumChannels];
    for (auto i = 0; i < stFileSpec.iNumChannels; i++)
        m_ppfAudioData[i] = new float[m_iBlockLength];

    // initialize FFT and fft output buffer
    m_pCFft = new CFft();
    m_pCFft->init(m_iBlockLength);
    m_pfSpectrum = new float[m_iBlockLength];

    return Error_t::kNoError;
}

Error_t CSpectrogram::reset()
{
    CAudioFileIf::FileSpec_t stFileSpec;

    delete m_pCRingBuffer;
    m_pCRingBuffer = 0;

    delete m_pCFft;
    m_pCFft = 0;

    delete m_pCNormalize;
    m_pCNormalize = 0;

    delete [] m_pfSpectrum;
    m_pfSpectrum = 0;

    m_pCAudioFile->getFileSpec(stFileSpec);
    for (auto c = 0; c < stFileSpec.iNumChannels; c++)
        delete[] m_ppfAudioData[c];
    delete[] m_ppfAudioData;
    m_ppfAudioData = 0;

    m_pCAudioFile->closeFile();
    CAudioFileIf::destroy(m_pCAudioFile);

    m_iAudioLength = 0;
    m_iBlockLength = 0;
    m_iHopLength = 0;
    m_iNumBlocks = 0;

    m_bIsInitialized = false;

    return Error_t::kNoError;

}

Error_t CSpectrogram::getSpectrogramDimensions(int& iNumRows, int& iNumCols) const
{
    if (!m_bIsInitialized)
    {
        iNumRows = 0;
        iNumCols = 0;
        return Error_t::kFunctionIllegalCallError;
    }

    iNumRows = (m_iBlockLength >> 1) + 1;
    iNumCols = m_iNumBlocks;

    return Error_t::kNoError;
}

Error_t CSpectrogram::process(float** ppfSpectrogram)
{
    if (!m_bIsInitialized)
        return Error_t::kFunctionIllegalCallError;
    if (!ppfSpectrogram)
        return Error_t::kFunctionInvalidArgsError;
    if (!ppfSpectrogram[0])
        return Error_t::kFunctionInvalidArgsError;

    assert(m_ppfAudioData);
    assert(m_pfSpectrum);
    assert(m_pCAudioFile);
    assert(m_pCRingBuffer);
    assert(m_pCFft);

    CAudioFileIf::FileSpec_t stFileSpec;
    m_pCAudioFile->getFileSpec(stFileSpec);

    for (auto n = 0; n < m_iNumBlocks; n++)
    {
        assert(!m_pCAudioFile->isEof());

        // set block length variable
        long long iNumFrames = m_iHopLength;

        // read data (iNumOfFrames might be updated!)
        m_pCAudioFile->readData(m_ppfAudioData, iNumFrames);

        // set buffer to zero if not written (EOF)
        if (iNumFrames < m_iHopLength)
        {
            for (int c = 0; c < stFileSpec.iNumChannels; c++)
                CVectorFloat::setZero(&m_ppfAudioData[c][iNumFrames], m_iHopLength - iNumFrames);
        }

        // downmix in case of multichannel
        CPreProc::downmix(m_ppfAudioData[0], m_ppfAudioData, stFileSpec.iNumChannels, iNumFrames);
        if (m_pCNormalize)
            m_pCNormalize->normalizeBlock(m_ppfAudioData[0], iNumFrames);

        // write data into inputbuffer
        m_pCRingBuffer->putPostInc(m_ppfAudioData[0], m_iHopLength);

        // get data from ringbuffer and increment read index
        m_pCRingBuffer->get(m_ppfAudioData[0], m_iBlockLength);
        m_pCRingBuffer->setReadIdx(m_pCRingBuffer->getReadIdx() + m_iHopLength);

        // compute magnitude spectrum (hack
        m_pCFft->doFft(m_pfSpectrum, m_ppfAudioData[0]);
        m_pCFft->getMagnitude(m_ppfAudioData[0], m_pfSpectrum);

        // copy to output buffer
        for (auto k = 0; k < m_pCFft->getLength(CFft::kLengthMagnitude); k++)
            ppfSpectrogram[k][n] = m_ppfAudioData[0][k];

    }

    return Error_t::kNoError;
}
