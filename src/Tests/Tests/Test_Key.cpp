#include "AcaAll.h"

#ifdef WITH_TESTS
#include "catch.hpp"

TEST_CASE("Key profile distance", "[KeyFromChroma]")
{
    CKeyFromChroma* pCInstance = new CKeyFromChroma();

    float afIn[12] = { 0 };

    SECTION("Chroma")
    {
        CHECK(CKey::kNoKey == pCInstance->getKey(afIn));

        for (auto i = 0; i < 12; i++)
        {
            afIn[i] = .333F; afIn[(i + 4) % 12] = .333F; afIn[(i + 7) % 12] = .333F;
            CHECK(i == pCInstance->getKey(afIn));
            CVector::mulC_I(afIn, 500.F, 12);
            CHECK(i == pCInstance->getKey(afIn));
            CVector::setZero(afIn, 12);

            afIn[i] = .333F; afIn[(i + 3) % 12] = .333F; afIn[(i + 7) % 12] = .333F;
            CHECK(i == pCInstance->getKey(afIn) - 12);
            CVector::mulC_I(afIn, 500.F, 12);
            CHECK(i == pCInstance->getKey(afIn) - 12);
            CVector::setZero(afIn, 12);
        }
    }

    delete pCInstance;
}

TEST_CASE("Key class", "[Key]")
{
    CKey* pCInstance = new CKey();

    int iBlockLength = 4096;
    int iHopLength = 2048;
    int iNumSamples = 44100;
    float fSampleRate = 44100.F;
    float* pfIn = 0;

    CVector::alloc(pfIn, iNumSamples);

    SECTION("Api")
    {
        CHECK(CKey::kCsMajor == CKey::getKeyIdxFromString("Db Major"));
        CHECK(CKey::kCsMajor == CKey::getKeyIdxFromString("C# Major"));

        CHECK("C# Major/Db Major" == CKey::getKeyString(CKey::kCsMajor));

        CHECK(Error_t::kFunctionInvalidArgsError == pCInstance->init(0, iNumSamples, fSampleRate, iBlockLength, iHopLength));
        CHECK(Error_t::kFunctionInvalidArgsError == pCInstance->init(pfIn, 0, fSampleRate, iBlockLength, iHopLength));
        CHECK(Error_t::kFunctionInvalidArgsError == pCInstance->init(pfIn, iNumSamples, 0, iBlockLength, iHopLength));
        CHECK(Error_t::kFunctionInvalidArgsError == pCInstance->init(pfIn, iNumSamples, fSampleRate, 0, iHopLength));
        CHECK(Error_t::kFunctionInvalidArgsError == pCInstance->init(pfIn, iNumSamples, fSampleRate, iBlockLength, 0));

        CHECK(Error_t::kNoError == pCInstance->init(pfIn, iNumSamples, fSampleRate, iBlockLength, iHopLength));

        CHECK(CKey::kNoKey == pCInstance->compKey());

        CHECK(Error_t::kNoError == pCInstance->reset());
    }

    SECTION("Audio")
    {
        float* pfTmp = 0;
        CVector::alloc(pfTmp, iNumSamples);
        float afChordPitches[3] = { 65, 69, 72 };
        for (auto l = 0; l < 3; l++)
        {
            CSynthesis::genSine(pfTmp, CConversion::convertMidi2Freq(12 + afChordPitches[l]), fSampleRate, iNumSamples);
            CVector::add_I(pfIn, pfTmp, iNumSamples);
        }
        CHECK(Error_t::kNoError == pCInstance->init(pfIn, iNumSamples, fSampleRate, iBlockLength, iHopLength));

        CHECK(CKey::kFMajor == pCInstance->compKey());

        CHECK(Error_t::kNoError == pCInstance->reset());

        CVector::free(pfTmp);
    }
    CVector::free(pfIn);

    delete pCInstance;
}

#endif //WITH_TESTS
