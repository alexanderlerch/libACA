

#include <cmath>

#include <map>

#include "Matrix.h"
#include "Util.h"
#include "AudioFileIf.h"
#include "Fft.h"

#include "ToolPreProc.h"
#include "ToolBlockAudio.h"
#include "ToolViterbi.h"

#include "Chord.h"
#include "ChordFromBlock.h"


/*! \brief class for computation of chord progression from a file
*/
class CChordFromFile : public CChordIf
{
public:
    CChordFromFile(std::string strAudioFilePath, int iBlockLength, int iHopLength);

    virtual ~CChordFromFile()
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

CChordFromFile::CChordFromFile(std::string strAudioFilePath, int iBlockLength, int iHopLength) :
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

    init_();
}


/*! \brief class for computation of chord progression from a vector of audio data
*/
class CChordFromVector : public CChordIf
{
public:
    CChordFromVector(const float* pfAudio, long long iAudioLength, float fSampleRate, int iBlockLength, int iHopLength);
    virtual ~CChordFromVector() {};
};

CChordFromVector::CChordFromVector(const float* pfAudio, long long iAudioLength, float fSampleRate, int iBlockLength, int iHopLength)
{
    // set length variables
    m_iBlockLength = iBlockLength;
    m_iHopLength = iHopLength;

    // set sample rate
    m_fSampleRate = fSampleRate;

    CBlockAudioIf::create(m_pCBlockAudio, pfAudio, iAudioLength, iBlockLength, iHopLength, m_fSampleRate);

    m_pCNormalize = new CNormalizeAudio(pfAudio, iAudioLength);

    init_();
}


/////////////////////////////////////////////////////////////////////////////////
// base (interface) class
CChordIf::CChordIf()
{
    reset_();
}

inline CChordIf::~CChordIf()
{
    reset_();
}

Error_t CChordIf::create(CChordIf*& pCInstance, const std::string& strAudioFilePath, int iBlockLength, int iHopLength)
{
    if (strAudioFilePath.empty())
        return Error_t::kFunctionInvalidArgsError;
    if (iBlockLength <= 2 || !CUtil::isPowOf2(iBlockLength))
        return Error_t::kFunctionInvalidArgsError;
    if (iHopLength <= 0 || iHopLength > iBlockLength)
        return Error_t::kFunctionInvalidArgsError;

    pCInstance = new CChordFromFile(strAudioFilePath, iBlockLength, iHopLength);


    return Error_t::kNoError;
}

Error_t CChordIf::create(CChordIf*& pCInstance, const float* pfAudio, long long iNumSamples, float fSampleRate, int iBlockLength, int iHopLength)
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

    pCInstance = new CChordFromVector(pfAudio, iNumSamples, fSampleRate, iBlockLength, iHopLength);

    return Error_t::kNoError;
}

Error_t CChordIf::destroy(CChordIf*& pCInstance)
{
    delete pCInstance;
    pCInstance = 0;

    return Error_t::kNoError;
}

Error_t CChordIf::getNumBlocks(int& iNumBlocks) const
{
    if (!m_bIsInitialized)
    {
        iNumBlocks = 0;
        return Error_t::kFunctionIllegalCallError;
    }

    iNumBlocks = this->getNumBlocks();

    return Error_t::kNoError;
}


/*! returns size of vector to be allocated by user
\return int
*/

int CChordIf::getNumBlocks() const
{
    assert(m_bIsInitialized);
    assert(m_pCBlockAudio);

    return static_cast<int>(m_pCBlockAudio->getNumBlocks());
}

float CChordIf::getTimeStamp(int iBlockIdx) const
{
    return m_pCBlockAudio->getTimeStamp(iBlockIdx);
}

Error_t CChordIf::getTimeStamps(float* pfAxisTicks) const
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

Error_t CChordIf::compChords(Chords_t* peChord, bool bWithViterbi /*= true*/)
{
    if (!m_bIsInitialized)
        return Error_t::kFunctionIllegalCallError;
    if (!peChord)
        return Error_t::kFunctionInvalidArgsError;

    assert(m_pfProcBuff1);
    assert(m_pCBlockAudio);
    assert(m_pCNormalize);
    assert(m_pCChord);

    auto iNumBlocks = m_pCBlockAudio->getNumBlocks();

    for (auto n = 0; n < iNumBlocks; n++)
    {
        float afChordProb[kNumChords] = { 0 };

        // retrieve the next audio block
        m_pCBlockAudio->getNextBlock(m_pfProcBuff1);

        // normalize if specified
        if (m_pCNormalize)
            m_pCNormalize->normalizeBlock(m_pfProcBuff1, m_iBlockLength);

        assert(m_pfProcBuff2);
        assert(m_pCFft);

        // compute magnitude spectrum
        computeMagSpectrum_();

        // compute instantaneous chord probs
        m_pCChord->compChordProb(afChordProb, m_pfProcBuff1);

        // store result
        if (bWithViterbi)
            CMatrix::setCol(m_ppfChordProbs, afChordProb, n, kNumChords);
        else
        {
            float fTmp = 0;
            long long iMaxIdx = -1;
            CVector::findMax(afChordProb, fTmp, iMaxIdx, kNumChords);
            peChord[n] = static_cast<Chords_t>(iMaxIdx);
        }
    }

    // compute viterbi
    if (bWithViterbi)
    {
        int* piTmp = 0;
        CVector::alloc(piTmp, iNumBlocks);

        m_pCViterbi->compViterbi(m_ppfChordProbs);

        // retrieve result
        m_pCViterbi->getStateSequence(piTmp);

        // write output
        for (auto n = 0; n < iNumBlocks; n++)
            peChord[n] = static_cast<Chords_t>(piTmp[n]);

        CVector::free(piTmp);
    }

    return Error_t::kNoError;
}


std::string CChordIf::getChordString(Chords_t eChordIdx)
{
    const std::map<CChordIf::Chords_t, std::string> ChordMap
    {
        {kCMajor, "C Major"},
        {kCsMajor, "C# Major/Db Major"},
        {kDMajor, "D Major"},
        {kDsMajor, "D# Major/Eb Major"},
        {kEMajor, "E Major"},
        {kFMajor, "F Major"},
        {kFsMajor, "F# Major/Gb Major"},
        {kGMajor, "G Major"},
        {kGsMajor, "G# Major/Ab Major"},
        {kAMajor, "A Major"},
        {kAsMajor, "A# Major/Bb Major"},
        {kBMajor, "B Major"},

        {kCMinor, "C Minor"},
        {kCsMinor, "C# Minor/Db Minor"},
        {kDMinor, "D Minor"},
        {kDsMinor, "D# Minor/Eb Minor"},
        {kEMinor, "E Minor"},
        {kFMinor, "F Minor"},
        {kFsMinor, "F# Minor/Gb Minor"},
        {kGMinor, "G Minor"},
        {kGsMinor, "G# Minor/Ab Minor"},
        {kAMinor, "A Minor"},
        {kAsMinor, "A# Minor/Bb Minor"},
        {kBMinor, "B Minor"},

        {kNoChord, "No Chord"}
    };

    return ChordMap.at(eChordIdx);
}

CChordIf::Chords_t CChordIf::getChordIdxFromString(std::string sChordString)
{
    const std::map<std::string, CChordIf::Chords_t> ChordMap
    {
        {"C Major", kCMajor},
        {"C# Major", kCsMajor},
        {"Db Major", kCsMajor},
        {"C# Major/Db Major", kCsMajor},
        {"D Major", kDMajor},
        {"D# Major", kDsMajor},
        {"Eb Major", kDsMajor},
        {"D# Major/Eb Major", kDsMajor},
        {"E Major", kEMajor},
        {"F Major", kFMajor},
        {"F# Majo", kFsMajor},
        {"Gb Major", kFsMajor},
        {"F# Major/Gb Major", kFsMajor},
        {"G Major", kGMajor},
        {"G# Major", kGsMajor},
        {"Ab Major", kGsMajor},
        {"G# Major/Ab Major", kGsMajor},
        {"A Major", kAMajor},
        {"A# Major", kAsMajor},
        {"Bb Major", kAsMajor},
        {"A# Major/Bb Major", kAsMajor},
        {"B Major", kBMajor},

        {"C Minor", kCMinor},
        {"C# Minor", kCsMinor},
        {"Db Minor", kCsMinor},
        {"C# Minor/Db Minor", kCsMinor},
        {"D Minor", kDMinor},
        {"D# Minor", kDsMinor},
        {"Eb Minor", kDsMinor},
        {"D# Minor/Eb Minor", kDsMinor},
        {"E Minor", kEMinor},
        {"F Minor", kFMinor},
        {"F# Minor", kFsMinor},
        {"Gb Minor", kFsMinor},
        {"F# Minor/Gb Minor", kFsMinor},
        {"G Minor", kGMinor},
        {"G# Minor", kGsMinor},
        {"Ab Minor", kGsMinor},
        {"G# Minor/Ab Minor", kGsMinor},
        {"A Minor", kAMinor},
        {"A# Minor", kAsMinor},
        {"Bb Minor", kAsMinor},
        {"A# Minor/Bb Minor", kAsMinor},
        {"B Minor", kBMinor},

        {"No Chord", kNoChord}
    };
    auto search = ChordMap.find(sChordString);
    if (search != ChordMap.end())
        return ChordMap.at(sChordString);
    else
        return kNumChords;
}


void CChordIf::computeMagSpectrum_()
{
    assert(m_pCFft);

    // compute magnitude spectrum 
    m_pCFft->compFft(m_pfProcBuff2, m_pfProcBuff1);
    m_pCFft->getMagnitude(m_pfProcBuff1, m_pfProcBuff2);

    CVector::mulC_I(m_pfProcBuff2, 2.F, m_pCFft->getLength(CFft::kLengthMagnitude));
}


Error_t CChordIf::reset_()
{
    CVector::free(m_pfProcBuff1);

    CVector::free(m_pfProcBuff2);

    CMatrix::free(m_ppfChordProbs, kNumChords);

    delete m_pCFft;
    m_pCFft = 0;

    delete m_pCNormalize;
    m_pCNormalize = 0;

    delete m_pCViterbi;
    m_pCViterbi = 0;

    CBlockAudioIf::destroy(m_pCBlockAudio);
    CChordFromBlockIf::destroy(m_pCChord);

    m_iBlockLength = 0;
    m_iHopLength = 0;

    m_bIsInitialized = false;

    return Error_t::kNoError;
}

Error_t CChordIf::init_()
{
    // initialize FFT and fft  buffer
    m_pCFft = new CFft();
    m_pCFft->init(m_iBlockLength);

    initViterbi_();

    // allocate processing memory
    CVector::alloc(m_pfProcBuff1, m_pCFft->getLength(CFft::kLengthFft));
    CVector::alloc(m_pfProcBuff2, m_pCFft->getLength(CFft::kLengthFft));
    CChordFromBlockIf::create(m_pCChord, m_pCFft->getLength(CFft::kLengthMagnitude), m_fSampleRate);
    CMatrix::alloc(m_ppfChordProbs, kNumChords, static_cast<int>(m_pCBlockAudio->getNumBlocks()));

    m_bIsInitialized = true;

    return Error_t::kNoError;
}

void CChordIf::initViterbi_()
{
    int iNumBlocks = static_cast<int>(m_pCBlockAudio->getNumBlocks());

    /////////////////////////////
    // start probabilities
    float afStartProb[kNumChords] = { 0 };

    CVector::setValue(afStartProb, 1.F / (kNumChords + 1), kNumChords);
    afStartProb[kNoChord] *= 2.F;

    /////////////////////////////
    // transition probabilities
    float** ppfTransProb = 0;
    CMatrix::alloc(ppfTransProb, kNumChords, kNumChords);
    const int aiCircleOfFifths[24] = { 0, -5, 2, -3, 4, -1, 6, 1, -4, 3, -2, 5, -3, 4, -1, 6, 1, -4, 3, -2, 5, 0, -5, 2 };
    float fRadius = 1.F;
    float fDistance = .5F;

    for (auto m = 0; m < kNumChords - 1; m++)
    {
        float fX1 = static_cast<float>(fRadius * std::cos(2. * M_PI * aiCircleOfFifths[m] / 12.));
        float fY1 = static_cast<float>(fRadius * std::sin(2. * M_PI * aiCircleOfFifths[m] / 12.));
        float fZ1 = fDistance - ((m < 12) ? 0 : fDistance);
        for (auto n = 0; n < kNumChords - 1; n++)
        {
            float fXDist = static_cast<float>(fX1 - fRadius * std::cos(2. * M_PI * aiCircleOfFifths[n] / 12.));
            float fYDist = static_cast<float>(fY1 - fRadius * std::sin(2. * M_PI * aiCircleOfFifths[n] / 12.));
            float fZDist = fZ1 - (fDistance - ((n < 12) ? 0 : fDistance));

            ppfTransProb[m][n] = .1F + std::sqrt(fXDist * fXDist + fYDist * fYDist + fZDist * fZDist);
        }
    }
    // convert to probabilities
    CMatrix::mulC_I(ppfTransProb, -1.F / (.1F + CMatrix::getMax(ppfTransProb, kNumChords, kNumChords)), kNumChords, kNumChords);
    CMatrix::addC_I(ppfTransProb, 1.F, kNumChords, kNumChords);

    // no chord probs
    CVector::setValue(ppfTransProb[kNoChord], 1.F / kNumChords, kNumChords);
    CMatrix::setCol(ppfTransProb, ppfTransProb[kNoChord], kNoChord, kNumChords);

    // normalization
    for (auto m = 0; m < kNumChords; m++)
        CVector::mulC_I(ppfTransProb[m], 1.F / CVector::getSum(ppfTransProb[m], kNumChords), kNumChords);

    /////////////////////////////
    // instance creation and initialization
    m_pCViterbi = new CViterbi();
    m_pCViterbi->init(ppfTransProb, afStartProb, kNumChords, iNumBlocks);

    // free temp memory
    CMatrix::free(ppfTransProb, kNumChords);
}

