#include "AcaAll.h"

#ifdef WITH_TESTS
#include "catch.hpp"

TEST_CASE("Key profile distance", "[KeyFromChroma]")
{
    CKeyFromChroma* pCInstance = new CKeyFromChroma();

    float afInput[12] = { 0 };

    SECTION("Chroma")
    {
        CHECK(CKey::kNoKey == pCInstance->getKey(afInput));

        for (auto i = 0; i < 12; i++)
        {
            afInput[i] = .333F; afInput[(i + 4) % 12] = .333F; afInput[(i + 7) % 12] = .333F;
            CHECK(i == pCInstance->getKey(afInput));
            CVector::mulC_I(afInput, 500.F, 12);
            CHECK(i == pCInstance->getKey(afInput));
            CVector::setZero(afInput, 12);

            afInput[i] = .333F; afInput[(i + 3) % 12] = .333F; afInput[(i + 7) % 12] = .333F;
            CHECK(i == pCInstance->getKey(afInput) - 12);
            CVector::mulC_I(afInput, 500.F, 12);
            CHECK(i == pCInstance->getKey(afInput) - 12);
            CVector::setZero(afInput, 12);
        }
    }

    delete pCInstance;
}

TEST_CASE("Key class", "[Key]")
{
    CKey* pCInstance = new CKey();

    int iBlockLength = 4096;
    int iHopLength = 2048;
    int iNumFrames = 44100;
    float fSampleRate = 44100.F;
    float* pfIn = 0;

    CVector::alloc(pfIn, iNumFrames);

    SECTION("Api")
    {
        CHECK(CKey::kCsMajor == CKey::getKeyIdxFromString("Db Major"));
        CHECK(CKey::kCsMajor == CKey::getKeyIdxFromString("C# Major"));

        CHECK("C# Major/Db Major" == CKey::getKeyString(CKey::kCsMajor));

        CHECK(Error_t::kFunctionInvalidArgsError == pCInstance->init(0, iNumFrames, fSampleRate, iBlockLength, iHopLength));
        CHECK(Error_t::kFunctionInvalidArgsError == pCInstance->init(pfIn, 0, fSampleRate, iBlockLength, iHopLength));
        CHECK(Error_t::kFunctionInvalidArgsError == pCInstance->init(pfIn, iNumFrames, 0, iBlockLength, iHopLength));
        CHECK(Error_t::kFunctionInvalidArgsError == pCInstance->init(pfIn, iNumFrames, fSampleRate, 0, iHopLength));
        CHECK(Error_t::kFunctionInvalidArgsError == pCInstance->init(pfIn, iNumFrames, fSampleRate, iBlockLength, 0));

        CHECK(Error_t::kNoError == pCInstance->init(pfIn, iNumFrames, fSampleRate, iBlockLength, iHopLength));

        CHECK(CKey::kNoKey == pCInstance->compKey());

        CHECK(Error_t::kNoError == pCInstance->reset());
    }

    SECTION("Audio")
    {
        float* pfTmp = 0;
        CVector::alloc(pfTmp, iNumFrames);
        float afChordPitches[3] = { 65, 69, 72 };
        for (auto l = 0; l < 3; l++)
        {
            CSynthesis::genSine(pfTmp, CConversion::convertMidi2Freq(12 + afChordPitches[l]), fSampleRate, iNumFrames);
            CVector::add_I(pfIn, pfTmp, iNumFrames);
        }
        CHECK(Error_t::kNoError == pCInstance->init(pfIn, iNumFrames, fSampleRate, iBlockLength, iHopLength));

        CHECK(CKey::kFMajor == pCInstance->compKey());

        CHECK(Error_t::kNoError == pCInstance->reset());

        CVector::free(pfTmp);
    }
    CVector::free(pfIn);

    delete pCInstance;
}

#endif //WITH_TESTS
