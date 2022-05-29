
#include "Util.h"
#include "Vector.h"
#include "Fft.h"
#include "Novelty.h"
#include "ToolCcf.h"
#include "ToolBlockAudio.h"
#include "ToolPreProc.h"
#include "ToolConversion.h"

#include "BeatHisto.h"


inline CBeatHistoIf::~CBeatHistoIf()
{
    reset_();
}
                            
Error_t CBeatHistoIf::create(CBeatHistoIf*& pCInstance, const std::string& strAudioFilePath, int iBlockLength, int iHopLength)
{
    if (strAudioFilePath.empty())
        return Error_t::kFunctionInvalidArgsError;
    if (iBlockLength <= 2 || !CUtil::isPowOf2(iBlockLength))
        return Error_t::kFunctionInvalidArgsError;
    if (iHopLength <= 0 || iHopLength > iBlockLength)
        return Error_t::kFunctionInvalidArgsError;

    pCInstance = new CBeatHistoIf(iBlockLength, iHopLength, 0.F);

    assert(pCInstance);

    return pCInstance->init_(strAudioFilePath);
}

Error_t CBeatHistoIf::create(CBeatHistoIf*& pCInstance, const float* pfAudio, long long iNumFrames, float fSampleRate, int iBlockLength, int iHopLength)
{
    if (!pfAudio)
        return Error_t::kFunctionInvalidArgsError;
    if (iNumFrames <= 0)
        return Error_t::kFunctionInvalidArgsError;
    if (fSampleRate <= 0)
        return Error_t::kFunctionInvalidArgsError;
    if (iBlockLength <= 2)
        return Error_t::kFunctionInvalidArgsError;
    if (iHopLength <= 0 || iHopLength > iBlockLength)
        return Error_t::kFunctionInvalidArgsError;

    pCInstance = new CBeatHistoIf(iBlockLength, iHopLength, fSampleRate);

    assert(pCInstance);

    return pCInstance->init_(pfAudio, iNumFrames, fSampleRate);
}

Error_t CBeatHistoIf::destroy(CBeatHistoIf*& pCInstance)
{
    delete pCInstance;
    pCInstance = 0;

    return Error_t::kNoError;
}

Error_t CBeatHistoIf::getNumBins(int& iLengthOfBeatHisto, BeatHisto_t eBeatHistoComp) const
{
    if (!m_bIsInitialized)
    {
        iLengthOfBeatHisto = 0;
        return Error_t::kFunctionIllegalCallError;
    }

    iLengthOfBeatHisto = this->getNumBins(eBeatHistoComp);

    return Error_t::kNoError;
}
int CBeatHistoIf::getNumBins(BeatHisto_t eBeatHistoComp) const
{
    int aiRangeIndices[2] = { 0,0 };
    compHistoRange_(aiRangeIndices[0], aiRangeIndices[1], eBeatHistoComp);
    return aiRangeIndices[1] - aiRangeIndices[0] + 1;
}

Error_t CBeatHistoIf::getBeatHistoAxisTicks(float* pfAxisTicks, BeatHisto_t eBeatHistoComp) const
{
    if (!m_bIsInitialized)
    {
        return Error_t::kFunctionIllegalCallError;
    }

    if (!pfAxisTicks)
        return Error_t::kFunctionInvalidArgsError;

    assert(m_iBlockLength > 0);
    assert(m_iHopLength > 0);
    assert(m_fSampleRate > 0);

    int aiRangeIndices[2] = { 0,0 };
    compHistoRange_(aiRangeIndices[0], aiRangeIndices[1], eBeatHistoComp);
    
    if (eBeatHistoComp == kBeatHistoFft)
    {
        for (auto k = aiRangeIndices[0], j = 0; k <= aiRangeIndices[1]; k++, j++)
            pfAxisTicks[j] = 60.F * CConversion::convertBin2Freq(1.F * k, m_iBeatHistoLength << 1, m_fSampleRate / m_iHopLength);
    }
    else
    {
        for (auto k = aiRangeIndices[0], j = 0; k <= aiRangeIndices[1]; k++, j++)
            pfAxisTicks[j] = 60.F / m_iHopLength *  m_fSampleRate / (m_iBeatHistoLength - k);
    }

    return Error_t::kNoError;
}

Error_t CBeatHistoIf::compBeatHisto(float* pfBeatHisto, BeatHisto_t eBeatHistoComp)
{
    if (!m_bIsInitialized)
        return Error_t::kFunctionIllegalCallError;
    if (!pfBeatHisto)
        return Error_t::kFunctionInvalidArgsError;

    CBlockAudioIf* pCBlock = 0;
    int aiBeatHistoRange[2] = { 0,0 };

    int iBeatHistoLength = getNumBins(eBeatHistoComp);
    int iHopSize = m_iBeatHistoLength >> 2;

    assert(iHopSize > 0);

    m_pCNovelty->compNovelty(m_pfNovelty);

    // create a blocking instance to use it on the Novelty function
    CBlockAudioIf::create(pCBlock, m_pfNovelty, m_pCNovelty->getNumBlocks(), m_iBeatHistoLength, iHopSize, m_fSampleRate / m_iHopLength);
    long long iNumBlocks = pCBlock->getNumBlocks();

    if (eBeatHistoComp == kBeatHistoCorr)
    {
        CCcf* pCAcf = new CCcf();

        // create correlation instance with zeropadding
        pCAcf->init(m_iBeatHistoLength);

        // loop over novelty functions with hopsize
        CVectorFloat::setZero(m_pfBeatHisto, m_iBeatHistoLength);
        for (auto n = 0; n < iNumBlocks; n++)
        {
            // get next novelty block
            pCBlock->getNextBlock(m_pfProcessBuff);

            // compute magnitude spectrum
            pCAcf->compCcf(m_pfProcessBuff, m_pfProcessBuff);
            pCAcf->getCcf(m_pfProcessBuff, true);

            // accumulate average spectrum
            CVectorFloat::add_I(m_pfBeatHisto, m_pfProcessBuff, m_iBeatHistoLength);
        }

        CVector::flip_I(m_pfBeatHisto, m_iBeatHistoLength);

        // destroy correlation instance
        delete pCAcf;
    }
    else if (eBeatHistoComp == kBeatHistoFft)
    {
        CFft *pCFft = new CFft();
        
        // create FFT instance with zeropadding
        pCFft->init(m_iBeatHistoLength, 2);
        
        // loop over novelty functions with hopsize
        CVectorFloat::setZero(m_pfBeatHisto, m_iBeatHistoLength);
        for (auto n = 0; n < iNumBlocks; n++)
        {
            // get next novelty block
            pCBlock->getNextBlock(m_pfProcessBuff);

            // compute magnitude spectrum
            pCFft->compFft(m_pfProcessBuff, m_pfProcessBuff);
            pCFft->getMagnitude(m_pfProcessBuff, m_pfProcessBuff);

            // accumulate average spectrum
            CVectorFloat::add_I(m_pfBeatHisto, m_pfProcessBuff, m_iBeatHistoLength);
        }

        // destroy FFT instance
        delete pCFft;
    }
    else
        return Error_t::kFunctionInvalidArgsError;

    // restrict output BPM to 30-200
    compHistoRange_(aiBeatHistoRange[0], aiBeatHistoRange[1], eBeatHistoComp);

    // copy to output
    //CVectorFloat::moveInMem(m_pfBeatHisto, 0, aiBeatHistoRange[0], aiBeatHistoRange[1] - aiBeatHistoRange[0] + 1);
    //CVectorFloat::setZero(&m_pfBeatHisto[aiBeatHistoRange[1] - aiBeatHistoRange[0] + 1], iBeatHistoLength - (aiBeatHistoRange[1] - aiBeatHistoRange[0] + 1));
    CVectorFloat::copy(pfBeatHisto, &m_pfBeatHisto[aiBeatHistoRange[0]], iBeatHistoLength);

    // create BPM axis labels

    // normalize result
    CNormalizeAudio::normalizeSignal(pfBeatHisto, iBeatHistoLength);

    // cleanup
    CBlockAudioIf::destroy(pCBlock);

    return Error_t::kNoError;
}



Error_t CBeatHistoIf::reset_()
{
    delete[] m_pfNovelty;
    m_pfNovelty = 0;

    delete[] m_pfProcessBuff;
    m_pfProcessBuff = 0;

    delete[] m_pfBeatHisto;
    m_pfBeatHisto = 0;

    m_iBlockLength = 0;
    m_iHopLength = 0;
    m_fSampleRate = 0;

    CNoveltyIf::destroy(m_pCNovelty);

    m_bIsInitialized = false;

    return Error_t::kNoError;
}

Error_t CBeatHistoIf::init_(const std::string& strAudioFilePath)
{
    if (Error_t::kNoError == CNoveltyIf::create(m_pCNovelty, CNoveltyIf::kNoveltyFlux, strAudioFilePath, m_iBlockLength, m_iHopLength))
    {
        int iNumBlocks = 0;
        m_pCNovelty->getNumBlocks(iNumBlocks);
        assert(iNumBlocks > 2);
        m_pfNovelty = new float[iNumBlocks];

        m_pCNovelty->getNoveltyTimeStamps(m_pfNovelty);
        m_fSampleRate = m_iHopLength / (m_pfNovelty[1] - m_pfNovelty[0]);

        m_pfProcessBuff = new float[m_iBeatHistoLength * 2];

        m_pfBeatHisto = new float[m_iBeatHistoLength + 1];

        m_bIsInitialized = true;
        return Error_t::kNoError;
    }
    else
        return Error_t::kMemError;
}

Error_t CBeatHistoIf::init_(const float* pfAudio, long long iNumFrames, float fSampleRate)
{
    if (Error_t::kNoError == CNoveltyIf::create(m_pCNovelty, CNoveltyIf::kNoveltyFlux, pfAudio, iNumFrames, fSampleRate, m_iBlockLength, m_iHopLength))
    {
        int iNumBlocks = 0;
        m_pCNovelty->getNumBlocks(iNumBlocks);
        assert(iNumBlocks > 2);
        m_pfNovelty = new float[iNumBlocks];

        m_pfProcessBuff = new float[m_iBeatHistoLength * 2];

        m_pfBeatHisto = new float[m_iBeatHistoLength+1];

        m_bIsInitialized = true;
        return Error_t::kNoError;
    }
    else
        return Error_t::kMemError;
}

//int compBeatHistoLength_(long long iLengthNovelty) const;

inline void CBeatHistoIf::compHistoRange_(int& iStartIdx, int& iStopIdx, BeatHisto_t eBeatHistoComp) const
{
    const float afBpmRange[2] = { 30.F, 200.F };

    if (eBeatHistoComp == kBeatHistoFft)
    {
        iStartIdx = static_cast<int>(CConversion::convertFreq2Bin(afBpmRange[0] / 60.F, m_iBeatHistoLength << 1, m_fSampleRate / m_iHopLength));
        iStopIdx = static_cast<int>(CConversion::convertFreq2Bin(afBpmRange[1] / 60.F, m_iBeatHistoLength << 1, m_fSampleRate / m_iHopLength)) + 1;
    }
    else
    {
        iStartIdx = m_iBeatHistoLength - static_cast<int>(60.F * m_fSampleRate / m_iHopLength / afBpmRange[0]);
        iStopIdx = m_iBeatHistoLength - static_cast<int>(60.F * m_fSampleRate / m_iHopLength / afBpmRange[1]);
    }
}
