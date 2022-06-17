
#include <map>
#include <functional>

#include "Util.h"
#include "AudioFileIf.h"
#include "Fft.h"

#include "ToolPreProc.h"
#include "ToolBlockAudio.h"

#include "Pitch.h"
#include "PitchFromBlock.h"


/*! \brief class for computation of the pitch from a file
*/
class CPitchFromFile : public CPitchIf
{
public:
    CPitchFromFile(PitchExtractors_t ePitchIdx, std::string strAudioFilePath, int iBlockLength, int iHopLength);

    virtual ~CPitchFromFile()
    {
        delete m_pCNormalize;
        m_pCNormalize = 0;

        CVector::free(m_pfProcBuff1);

        m_pCAudioFile->closeFile();
        CAudioFileIf::destroy(m_pCAudioFile);
    };

private:
    CAudioFileIf* m_pCAudioFile;
};

CPitchFromFile::CPitchFromFile(PitchExtractors_t ePitchIdx, std::string strAudioFilePath, int iBlockLength, int iHopLength) :
    m_pCAudioFile(0)
{
    this->reset_();

    CAudioFileIf::FileSpec_t stFileSpec;
    CAudioFileIf::create(m_pCAudioFile);
    m_pCAudioFile->openFile(strAudioFilePath, CAudioFileIf::kFileRead);
    m_pCAudioFile->getFileSpec(stFileSpec);
    m_fSampleRate = stFileSpec.fSampleRateInHz;

    CBlockAudioIf::create(m_pCBlockAudio, m_pCAudioFile, iBlockLength, iHopLength);

    m_pCNormalize = new CNormalizeAudio(m_pCAudioFile);

    // set length variables
    m_iBlockLength = iBlockLength;
    m_iHopLength = iHopLength;

    init_(ePitchIdx);
}


/*! \brief class for computation of the pitch from a vector of audio data
*/
class CPitchFromVector : public CPitchIf
{
public:
    CPitchFromVector(PitchExtractors_t ePitchIdx, const float* pfAudio, long long iAudioLength, float fSampleRate, int iBlockLength, int iHopLength);
    virtual ~CPitchFromVector() {};
};

CPitchFromVector::CPitchFromVector(PitchExtractors_t ePitchIdx, const float* pfAudio, long long iAudioLength, float fSampleRate, int iBlockLength, int iHopLength)
{
    // set length variables
    m_iBlockLength = iBlockLength;
    m_iHopLength = iHopLength;

    // set sample rate
    m_fSampleRate = fSampleRate;

    CBlockAudioIf::create(m_pCBlockAudio, pfAudio, iAudioLength, iBlockLength, iHopLength, m_fSampleRate);

    m_pCNormalize = new CNormalizeAudio(pfAudio, iAudioLength);

    init_(ePitchIdx);
}


/////////////////////////////////////////////////////////////////////////////////
// base class
CPitchIf::CPitchIf()
{
    reset_();
}

inline CPitchIf::~CPitchIf()
{
    reset_();
}

Error_t CPitchIf::create(CPitchIf*& pCInstance, PitchExtractors_t ePitchIdx, const std::string& strAudioFilePath, int iBlockLength, int iHopLength)
{
    if (strAudioFilePath.empty())
        return Error_t::kFunctionInvalidArgsError;
    if (iBlockLength <= 2 || !CUtil::isPowOf2(iBlockLength))
        return Error_t::kFunctionInvalidArgsError;
    if (iHopLength <= 0 || iHopLength > iBlockLength)
        return Error_t::kFunctionInvalidArgsError;

    pCInstance = new CPitchFromFile(ePitchIdx, strAudioFilePath, iBlockLength, iHopLength);


    return Error_t::kNoError;
}

Error_t CPitchIf::create(CPitchIf*& pCInstance, PitchExtractors_t ePitchIdx, const float* pfAudio, long long iNumSamples, float fSampleRate, int iBlockLength, int iHopLength)
{
    if (!pfAudio)
        return Error_t::kFunctionInvalidArgsError;
    if (iNumSamples <= 0)
        return Error_t::kFunctionInvalidArgsError;
    if (fSampleRate <= 0)
        return Error_t::kFunctionInvalidArgsError;
    if (iBlockLength <= 2)
        return Error_t::kFunctionInvalidArgsError;
    if (iHopLength <= 0 || iHopLength > iBlockLength)
        return Error_t::kFunctionInvalidArgsError;

    pCInstance = new CPitchFromVector(ePitchIdx, pfAudio, iNumSamples, fSampleRate, iBlockLength, iHopLength);

    return Error_t::kNoError;
}

Error_t CPitchIf::destroy(CPitchIf*& pCInstance)
{
    delete pCInstance;
    pCInstance = 0;

    return Error_t::kNoError;
}

Error_t CPitchIf::getNumBlocks(int& iNumBlocks) const
{
    if (!m_bIsInitialized)
    {
        iNumBlocks = 0;
        return Error_t::kFunctionIllegalCallError;
    }

    iNumBlocks = this->getNumBlocks();

    return Error_t::kNoError;
}

int CPitchIf::getNumBlocks() const
{
    assert(m_bIsInitialized);
    assert(m_pCBlockAudio);

    return static_cast<int>(m_pCBlockAudio->getNumBlocks());
}

float CPitchIf::getTimeStamp(int iBlockIdx) const
{
    return m_pCBlockAudio->getTimeStamp(iBlockIdx);
}

Error_t CPitchIf::getTimeStamps(float* pfAxisTicks) const
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

    long long iNumBlocks = m_pCBlockAudio->getNumBlocks();
    for (auto n = 0; n < iNumBlocks; n++)
        pfAxisTicks[n] = m_pCBlockAudio->getTimeStamp(n);

    return Error_t::kNoError;
}

Error_t CPitchIf::compF0(float* pfPitch)
{
    if (!m_bIsInitialized)
        return Error_t::kFunctionIllegalCallError;
    if (!pfPitch)
        return Error_t::kFunctionInvalidArgsError;

    assert(m_pfProcBuff1);
    assert(m_pCBlockAudio);
    assert(m_pCNormalize);
    assert(m_pCPitch);

    auto iNumBlocks = m_pCBlockAudio->getNumBlocks();

    for (auto n = 0; n < iNumBlocks; n++)
    {
        // retrieve the next audio block
        m_pCBlockAudio->getNextBlock(m_pfProcBuff1);

        // normalize if specified
        if (m_pCNormalize)
            m_pCNormalize->normalizeBlock(m_pfProcBuff1, m_iBlockLength);

        if (isPitchExtractorSpectral_(m_pCPitch->getPitchExtractorIdx()))
        {
            // compute magnitude specturm
            assert(m_pfProcBuff2);
            assert(m_pCFft);
            computeMagSpectrum_();
        }

        // extract f0
        pfPitch[n] = m_pCPitch->compF0(m_pfProcBuff1);
    }

    return Error_t::kNoError;
}


std::string CPitchIf::getPitchString(PitchExtractors_t ePitchIdx)
{
    const std::map<CPitchIf::PitchExtractors_t, std::string> PitchMap
    {
            {kPitchSpectralAcf, "SpectralAcf"},
            {kPitchSpectralHps, "SpectralHps"},

            {kPitchTimeAcf, "TimeAcf"},
            {kPitchTimeAmdf, "TimeAmdf"},
            {kPitchTimeAuditory, "TimeAuditory,"},
            {kPitchTimeZeroCrossings, "TimeZeroCrossings"}
    };

    return PitchMap.at(ePitchIdx);
}

CPitchIf::PitchExtractors_t CPitchIf::getPitchIdxFromString(std::string sPitchString)
{
    const std::map<std::string, CPitchIf::PitchExtractors_t> PitchMap
    {
            {"SpectralAcf", kPitchSpectralAcf},
            {"SpectralHps", kPitchSpectralHps},

            {"TimeAcf", kPitchTimeAcf},
            {"TimeAmdf", kPitchTimeAmdf},
            {"TimeAuditory", kPitchTimeAuditory},
            {"TimeZeroCrossings", kPitchTimeZeroCrossings}
    };
    auto search = PitchMap.find(sPitchString);
    if (search != PitchMap.end())
        return PitchMap.at(sPitchString);
    else
        return kNumPitchExtractors;
}

void CPitchIf::computeMagSpectrum_()
{
    assert(m_pCFft);

    // compute magnitude spectrum 
    m_pCFft->compFft(m_pfProcBuff2, m_pfProcBuff1);
    m_pCFft->getMagnitude(m_pfProcBuff1, m_pfProcBuff2);

    CVector::mulC_I(m_pfProcBuff2, 2.F, m_pCFft->getLength(CFft::kLengthMagnitude));
}


Error_t CPitchIf::reset_()
{
    CVector::free(m_pfProcBuff1);

    CVector::free(m_pfProcBuff2);

    delete m_pCFft;
    m_pCFft = 0;

    delete m_pCNormalize;
    m_pCNormalize = 0;

    CBlockAudioIf::destroy(m_pCBlockAudio);
    CPitchFromBlockIf::destroy(m_pCPitch);

    m_iBlockLength = 0;
    m_iHopLength = 0;

    m_bIsInitialized = false;

    return Error_t::kNoError;
}

Error_t CPitchIf::init_(PitchExtractors_t ePitchIdx)
{
    if (isPitchExtractorSpectral_(ePitchIdx))
    {
        // initialize FFT and fft  buffer
        m_pCFft = new CFft();
        m_pCFft->init(m_iBlockLength);
        // allocate processing memory
        CVector::alloc(m_pfProcBuff1, m_pCFft->getLength(CFft::kLengthFft));
        CVector::alloc(m_pfProcBuff2, m_pCFft->getLength(CFft::kLengthFft));
        CPitchFromBlockIf::create(m_pCPitch, ePitchIdx, m_pCFft->getLength(CFft::kLengthMagnitude), m_fSampleRate);
    }
    else
    {
        // allocate processing memory
        CVector::alloc(m_pfProcBuff1, m_iBlockLength);
        CPitchFromBlockIf::create(m_pCPitch, ePitchIdx, m_iBlockLength, m_fSampleRate);
    }
    m_bIsInitialized = true;

    return Error_t::kNoError;
}

bool CPitchIf::isPitchExtractorSpectral_(PitchExtractors_t ePitchIdx)
{
    return (ePitchIdx <= kPitchSpectralHps) ? true : false;
}
