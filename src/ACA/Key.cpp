
#include <map>

#include "Matrix.h"
#include "Feature.h"

#include "Key.h"
#include "KeyFromChroma.h"


CKey::CKey()
{
    m_pCKeyFromChroma = new CKeyFromChroma();
}

CKey::~CKey()
{
    reset();

    delete m_pCKeyFromChroma;
}

Error_t CKey::init(const std::string &strAudioFilePath, int iBlockLength, int iHopLength)
{
    if (strAudioFilePath.empty())
        return Error_t::kFunctionInvalidArgsError;
    if (iBlockLength <= 2)
        return Error_t::kFunctionInvalidArgsError;
    if (iHopLength <= 0 || iHopLength > iBlockLength)
        return Error_t::kFunctionInvalidArgsError;

    reset();

    Error_t rErr = CFeatureIf::create(m_pCPitchChromaExtractor, CFeatureIf::kFeatureSpectralPitchChroma, strAudioFilePath, iBlockLength, iHopLength);

    if (rErr == Error_t::kNoError)
    {
        m_bIsInitialized = true;

        return Error_t::kNoError;
    }
    else
        return rErr;
}

Error_t CKey::init(const float *pfAudio, long long iNumSamples, float fSampleRate, int iBlockLength, int iHopLength)
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

    reset();

    Error_t rErr = CFeatureIf::create(m_pCPitchChromaExtractor, CFeatureIf::kFeatureSpectralPitchChroma, pfAudio, iNumSamples, fSampleRate, iBlockLength, iHopLength);

    if (rErr == Error_t::kNoError)
    {
        m_bIsInitialized = true;

        return Error_t::kNoError;
    }
    else
        return rErr;
}

Error_t CKey::reset()
{
    m_bIsInitialized = false;

    CFeatureIf::destroy(m_pCPitchChromaExtractor);

    return Error_t::kNoError;
}

int CKey::compKey()
{
    if (!m_bIsInitialized)
        return -1;

    int aiDim[2] = { 0 };
    float **m_ppfPitchChromagram = 0;
    float afAvgPitchChroma[12] = { 0 };

    // alloc memory
    m_pCPitchChromaExtractor->getFeatureDimensions(aiDim[0], aiDim[1]);
    CMatrix::alloc(m_ppfPitchChromagram, aiDim[0], aiDim[1]);

    // compute pitch chromagram
    m_pCPitchChromaExtractor->compFeatureNDim(m_ppfPitchChromagram);

    // average pitch chroma
    for (auto n = 0; n < aiDim[0]; n++)
        afAvgPitchChroma[n] = CVector::getMean(m_ppfPitchChromagram[n], aiDim[1]);

    CMatrix::free(m_ppfPitchChromagram, aiDim[0]);

    // compute key
    return m_pCKeyFromChroma->getKey(afAvgPitchChroma);
}

std::string CKey::getKeyString(Keys_t eKeyIdx)
{
    const std::map<CKey::Keys_t, std::string> KeyMap
    {
        { kCMajor, "C Major" },
        { kCsMajor, "C# Major/Db Major" },
        { kDMajor, "D Major" },
        { kDsMajor, "D# Major/Eb Major" },
        { kEMajor, "E Major" },
        { kFMajor, "F Major" },
        { kFsMajor, "F# Major/Gb Major" },
        { kGMajor, "G Major" },
        { kGsMajor, "G# Major/Ab Major" },
        { kAMajor, "A Major" },
        { kAsMajor, "A# Major/Bb Major" },
        { kBMajor, "B Major" },

        { kCMinor, "C Minor" },
        { kCsMinor, "C# Minor/Db Minor" },
        { kDMinor, "D Minor" },
        { kDsMinor, "D# Minor/Eb Minor" },
        { kEMinor, "E Minor" },
        { kFMinor, "F Minor" },
        { kFsMinor, "F# Minor/Gb Minor" },
        { kGMinor, "G Minor" },
        { kGsMinor, "G# Minor/Ab Minor" },
        { kAMinor, "A Minor" },
        { kAsMinor, "A# Minor/Bb Minor" },
        { kBMinor, "B Minor" },

        { kNoKey, "No Chord" }
    };

    return KeyMap.at(eKeyIdx);
}

CKey::Keys_t CKey::getKeyIdxFromString(std::string sKeyString)
{
    const std::map<std::string, CKey::Keys_t> KeyMap
    {
        { "C Major", kCMajor },
        { "C# Major", kCsMajor },
        { "Db Major", kCsMajor },
        { "C# Major/Db Major", kCsMajor },
        { "D Major", kDMajor },
        { "D# Major", kDsMajor },
        { "Eb Major", kDsMajor },
        { "D# Major/Eb Major", kDsMajor },
        { "E Major", kEMajor },
        { "F Major", kFMajor },
        { "F# Majo", kFsMajor },
        { "Gb Major", kFsMajor },
        { "F# Major/Gb Major", kFsMajor },
        { "G Major", kGMajor },
        { "G# Major", kGsMajor },
        { "Ab Major", kGsMajor },
        { "G# Major/Ab Major", kGsMajor },
        { "A Major", kAMajor },
        { "A# Major", kAsMajor },
        { "Bb Major", kAsMajor },
        { "A# Major/Bb Major", kAsMajor },
        { "B Major", kBMajor },

        { "C Minor", kCMinor },
        { "C# Minor", kCsMinor },
        { "Db Minor", kCsMinor },
        { "C# Minor/Db Minor", kCsMinor },
        { "D Minor", kDMinor },
        { "D# Minor", kDsMinor },
        { "Eb Minor", kDsMinor },
        { "D# Minor/Eb Minor", kDsMinor },
        { "E Minor", kEMinor },
        { "F Minor", kFMinor },
        { "F# Minor", kFsMinor },
        { "Gb Minor", kFsMinor },
        { "F# Minor/Gb Minor", kFsMinor },
        { "G Minor", kGMinor },
        { "G# Minor", kGsMinor },
        { "Ab Minor", kGsMinor },
        { "G# Minor/Ab Minor", kGsMinor },
        { "A Minor", kAMinor },
        { "A# Minor", kAsMinor },
        { "Bb Minor", kAsMinor },
        { "A# Minor/Bb Minor", kAsMinor },
        { "B Minor", kBMinor },

        { "No Chord", kNoKey }
    };
    auto search = KeyMap.find(sKeyString);
    if (search != KeyMap.end())
        return KeyMap.at(sKeyString);
    else
        return kNumKeys;
}

