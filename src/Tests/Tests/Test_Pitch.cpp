#include "ACAConfig.h"

#ifdef WITH_TESTS

#include "Vector.h"
#include "Synthesis.h"
#include "ToolConversion.h"

#include "PitchFromBlock.h"
#include "catch.hpp"

TEST_CASE("Pitch tracking (class interface per block)", "[PitchBlockClass]")
{

    SECTION("Api")
    {
        //CHECK(Error_t::kFunctionInvalidArgsError == CNoveltyFromBlockIf::create(pCInstance, CNoveltyIf::kNoveltyFlux, 0, fSampleRate));
        //CHECK(Error_t::kFunctionInvalidArgsError == CNoveltyFromBlockIf::create(pCInstance, CNoveltyIf::kNoveltyFlux, -1, fSampleRate));
        //CHECK(Error_t::kFunctionInvalidArgsError == CNoveltyFromBlockIf::create(pCInstance, CNoveltyIf::kNoveltyFlux, iBufferLength, 0));
        //CHECK(Error_t::kFunctionInvalidArgsError == CNoveltyFromBlockIf::create(pCInstance, CNoveltyIf::kNoveltyFlux, iBufferLength, -1));

        //CHECK(Error_t::kNoError == CNoveltyFromBlockIf::create(pCInstance, CNoveltyIf::kNoveltyFlux, iBufferLength, fSampleRate));
        //CHECK(1 == pCInstance->getNoveltyDimension());
        //CHECK(Error_t::kNoError == CNoveltyFromBlockIf::destroy(pCInstance));
    }

}

TEST_CASE("Pitch (per array)", "[PitchClass]")
{

    SECTION("Api")
    {
        //for (auto f = 0; f < CNoveltyIf::kNumNoveltyFunctions; f++)
        //{
        //    CHECK(Error_t::kFunctionInvalidArgsError == CNoveltyIf::create(pCInstance, static_cast<CNoveltyIf::Novelty_t>(f), 0, iBufferLength, fSampleRate, iBlockLength, iHopLength));
        //    CHECK(Error_t::kFunctionInvalidArgsError == CNoveltyIf::create(pCInstance, static_cast<CNoveltyIf::Novelty_t>(f), pfInput, 0, fSampleRate, iBlockLength, iHopLength));
        //    CHECK(Error_t::kFunctionInvalidArgsError == CNoveltyIf::create(pCInstance, static_cast<CNoveltyIf::Novelty_t>(f), pfInput, -1, fSampleRate, iBlockLength, iHopLength));
        //    CHECK(Error_t::kFunctionInvalidArgsError == CNoveltyIf::create(pCInstance, static_cast<CNoveltyIf::Novelty_t>(f), pfInput, iBufferLength, 0, iBlockLength, iHopLength));
        //    CHECK(Error_t::kFunctionInvalidArgsError == CNoveltyIf::create(pCInstance, static_cast<CNoveltyIf::Novelty_t>(f), pfInput, iBufferLength, fSampleRate, 0, iHopLength));
        //    CHECK(Error_t::kFunctionInvalidArgsError == CNoveltyIf::create(pCInstance, static_cast<CNoveltyIf::Novelty_t>(f), pfInput, iBufferLength, fSampleRate, iBlockLength, 0));

        //    CHECK(Error_t::kNoError == CNoveltyIf::create(pCInstance, static_cast<CNoveltyIf::Novelty_t>(f), pfInput, iBufferLength, fSampleRate, iBlockLength, iHopLength));

        //    CHECK_FALSE(pCInstance == 0);

        //    CHECK(Error_t::kNoError == pCInstance->getNumBlocks(iDim));
        //    CHECK(iDim == 188);

        //    CHECK(Error_t::kFunctionInvalidArgsError == pCInstance->compNovelty(0));

        //    CHECK(Error_t::kNoError == CNoveltyIf::destroy(pCInstance));

        //}
    }

}

#endif //WITH_TESTS
