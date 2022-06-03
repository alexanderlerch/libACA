#include "ACAConfig.h"

#ifdef WITH_TESTS

#include "Vector.h"
#include "Synthesis.h"
#include "ToolConversion.h"

#include "PitchFromBlock.h"
#include "catch.hpp"

TEST_CASE("Pitch tracking (class interface per block)", "[PitchBlockClass]")
{
    CPitchFromBlockIf* pCInstance = 0;

    float* pfInput = 0;

    int iBlockLength = 4096;
    float fSampleRate = 4096;
    //float fResult = -1.F;

    CVector::alloc(pfInput, iBlockLength);

    SECTION("Api")
    {
        for (auto j = 0; j < CPitchFromBlockIf::kNumPitchExtractors; j++)
        {
            CHECK(Error_t::kFunctionInvalidArgsError == CPitchFromBlockIf::create(pCInstance, static_cast<CPitchFromBlockIf::PitchExtractors_t>(j), 0, fSampleRate));
            CHECK(Error_t::kFunctionInvalidArgsError == CPitchFromBlockIf::create(pCInstance, static_cast<CPitchFromBlockIf::PitchExtractors_t>(j), -1, fSampleRate));
            CHECK(Error_t::kFunctionInvalidArgsError == CPitchFromBlockIf::create(pCInstance, static_cast<CPitchFromBlockIf::PitchExtractors_t>(j), iBlockLength, 0));

            CHECK(Error_t::kNoError == CPitchFromBlockIf::create(pCInstance, static_cast<CPitchFromBlockIf::PitchExtractors_t>(j), iBlockLength, fSampleRate));

            CHECK_FALSE(pCInstance == 0);

            CHECK(static_cast<CPitchFromBlockIf::PitchExtractors_t>(j) == pCInstance->getPitchExtractorIdx());

            CHECK(Error_t::kNoError == CPitchFromBlockIf::destroy(pCInstance));
        }
    }

    SECTION("SpectralAcf")
    {
        fSampleRate = 48000;
        iBlockLength = 2049;
        CHECK(Error_t::kNoError == CPitchFromBlockIf::create(pCInstance, CPitchFromBlockIf::kPitchSpectralAcf, iBlockLength, fSampleRate));

        // zero input
        CHECK(0.F == pCInstance->compF0(pfInput));

        // sine 
        int iIdx = 30;
        pfInput[iIdx] = 1.F;
        CHECK(iIdx * .5 * fSampleRate / (iBlockLength - 1) == pCInstance->compF0(pfInput));

        // harmonics
        pfInput[iIdx] = 1.F;
        pfInput[2 * iIdx] = .5F;
        pfInput[3 * iIdx] = .25F;
        pfInput[4 * iIdx] = .125F;
        CHECK(iIdx * .5 * fSampleRate / (iBlockLength - 1) == pCInstance->compF0(pfInput));

    }

    SECTION("SpectralHps")
    {
        fSampleRate = 48000;
        iBlockLength = 2049;
        CHECK(Error_t::kNoError == CPitchFromBlockIf::create(pCInstance, CPitchFromBlockIf::kPitchSpectralHps, iBlockLength, fSampleRate));

        // zero input
        CHECK(0.F == pCInstance->compF0(pfInput));

        // sine 
        int iIdx = 30;
        pfInput[iIdx] = 1.F;
        CHECK(0.F == pCInstance->compF0(pfInput));

        // harmonics
        pfInput[iIdx] = 1.F;
        pfInput[2 * iIdx] = .5F;
        pfInput[3 * iIdx] = .25F;
        pfInput[4 * iIdx] = .125F;
        CHECK(iIdx * .5 * fSampleRate / (iBlockLength - 1) == pCInstance->compF0(pfInput));

    }

    SECTION("TimeAcf")
    {
        CHECK(Error_t::kNoError == CPitchFromBlockIf::create(pCInstance, CPitchFromBlockIf::kPitchTimeAcf, iBlockLength, fSampleRate));

        // zero input
        CHECK(0.F == pCInstance->compF0(pfInput));
        
        // sine 
        float fFreq = 16.F;
        CSynthesis::genSine(pfInput, fFreq, fSampleRate, iBlockLength);

        CHECK(fFreq == pCInstance->compF0(pfInput));

        // T0 between bins (T0 = 511.5)
        fFreq = 8.0078201369F;
        CSynthesis::genSine(pfInput, fFreq, fSampleRate, iBlockLength);

        CHECK(((fSampleRate/511 == pCInstance->compF0(pfInput)) || (fSampleRate / 512 == pCInstance->compF0(pfInput))));
    }

    SECTION("TimeAmdf")
    {
        fSampleRate = 40960;
        CHECK(Error_t::kNoError == CPitchFromBlockIf::create(pCInstance, CPitchFromBlockIf::kPitchTimeAmdf, iBlockLength, fSampleRate));

        // zero input
        CHECK(0.F == pCInstance->compF0(pfInput));

        // sine 
        float fFreq = 160.F;
        CSynthesis::genSine(pfInput, fFreq, fSampleRate, iBlockLength);

        CHECK(((fFreq == pCInstance->compF0(pfInput)) ||
            (fFreq / 2 == pCInstance->compF0(pfInput)) ||
            (fFreq / 3 == pCInstance->compF0(pfInput))));
    }

    SECTION("TimeZeroCrossings")
    {
        fSampleRate = 40960;
        CHECK(Error_t::kNoError == CPitchFromBlockIf::create(pCInstance, CPitchFromBlockIf::kPitchTimeZeroCrossings, iBlockLength, fSampleRate));

        // zero input
        CHECK(0.F == pCInstance->compF0(pfInput));

        // sine 
        float fFreq = 160.F;
        CSynthesis::genSine(pfInput, fFreq, fSampleRate, iBlockLength, 1.F, 3.13F);

        CHECK(fFreq == pCInstance->compF0(pfInput));
    }

    CHECK(Error_t::kNoError == CPitchFromBlockIf::destroy(pCInstance));

    CVector::free(pfInput);
}

TEST_CASE("Pitch (per array)", "[PitchClass]")
{

    SECTION("Api")
    {
    }

}

#endif //WITH_TESTS
