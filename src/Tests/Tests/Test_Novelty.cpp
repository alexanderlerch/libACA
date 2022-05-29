#include "ACAConfig.h"

#ifdef WITH_TESTS

#include "Vector.h"
#include "Synthesis.h"
#include "ToolConversion.h"
#include "ToolLowPass.h"

#include "NoveltyFromBlock.h"
#include "catch.hpp"

TEST_CASE("Novelty (static functions)", "[NoveltyStatic]")
{

    float* pfInput = 0;
    float fSampleRate = 1;
    int iBufferLength = 1025;

    pfInput = new float[iBufferLength];
    CVectorFloat::setZero(pfInput, iBufferLength);

    SECTION("NoveltyFlux")
    {
        int iIdx = iBufferLength / 2;

        // zero test
        CHECK(0.F == CNoveltyFromBlockIf::compNoveltyFlux(pfInput, &pfInput[iIdx], iBufferLength / 2));

        // 'sine' test
        pfInput[10] = 1;
        pfInput[iIdx + 10] = 1;
        fSampleRate = 2048;
        CHECK(0.F == Approx(CNoveltyFromBlockIf::compNoveltyFlux(pfInput, &pfInput[iIdx], iBufferLength / 2, fSampleRate)).margin(1e-6F).epsilon(1e-6F));
        fSampleRate = 32;
        CHECK(0.F == Approx(CNoveltyFromBlockIf::compNoveltyFlux(pfInput, &pfInput[iIdx], iBufferLength / 2, fSampleRate)).margin(1e-6F).epsilon(1e-6F));

        // 'noise' test
        CVectorFloat::setValue(pfInput, 1, iBufferLength);
        CHECK(0.F == Approx(CNoveltyFromBlockIf::compNoveltyFlux(pfInput, &pfInput[iIdx], iBufferLength / 2)).margin(1e-6F).epsilon(1e-6F));

        // one spectrum zero, the other one
        CVectorFloat::setZero(pfInput, iBufferLength / 2);
        CHECK(0.F == Approx(CNoveltyFromBlockIf::compNoveltyFlux(pfInput, &pfInput[iIdx], iBufferLength / 2)).margin(1e-4F).epsilon(1e-4F));
        CHECK(1.F / std::sqrt(iBufferLength / 2.F) == Approx(CNoveltyFromBlockIf::compNoveltyFlux(&pfInput[iIdx], pfInput, iBufferLength / 2)).margin(1e-4F).epsilon(1e-4F));

        // one spectrum zero, the other two
        CVectorFloat::mulC_I(pfInput, 2.F, iBufferLength);
        CHECK(0.F == Approx(CNoveltyFromBlockIf::compNoveltyFlux(pfInput, &pfInput[iIdx], iBufferLength / 2)).margin(1e-4F).epsilon(1e-4F));
        CHECK(std::sqrt(4.F * (iBufferLength / 2)) / (iBufferLength / 2) == Approx(CNoveltyFromBlockIf::compNoveltyFlux(&pfInput[iIdx], pfInput, iBufferLength / 2)).margin(1e-4F).epsilon(1e-4F));

        //decreasing spectrum
        CVectorFloat::setZero(pfInput, iBufferLength / 2);
        CVectorFloat::setValue(&pfInput[iBufferLength / 2], 1.F, iBufferLength / 2);
        for (auto n = 0; n < 5; n++)
        {
            CHECK(0.F == Approx(CNoveltyFromBlockIf::compNoveltyFlux(pfInput, &pfInput[iBufferLength / 2], iBufferLength / 2)).margin(1e-4F).epsilon(1e-4F));
            CVectorFloat::mulC_I(&pfInput[iBufferLength / 2], .75F, iBufferLength / 2);
        }

        // alternating spectral bins
        CVectorFloat::setZero(pfInput, iBufferLength);
        iIdx = 4;
        iBufferLength = 4;
        pfInput[0] = 1;
        pfInput[2] = 1;
        pfInput[iIdx + 1] = 1;
        pfInput[iIdx + 3] = 1;
        CHECK(std::sqrt(2.F) / 4.F == Approx(CNoveltyFromBlockIf::compNoveltyFlux(pfInput, &pfInput[iIdx], iBufferLength)).margin(1e-4F).epsilon(1e-4F));
    }

    SECTION("NoveltyHainsworth")
    {
        int iIdx = iBufferLength / 2;

        float fEpsilon = 1e-5F;

        // zero test
        CHECK(0.F == CNoveltyFromBlockIf::compNoveltyHainsworth(pfInput, &pfInput[iIdx], iBufferLength / 2));

        // 'sine' test
        pfInput[10] = 1;
        pfInput[iIdx + 10] = 1;
        fSampleRate = 2048;
        CHECK(0.F == Approx(CNoveltyFromBlockIf::compNoveltyHainsworth(pfInput, &pfInput[iIdx], iBufferLength / 2, fSampleRate)).margin(1e-6F).epsilon(1e-6F));
        fSampleRate = 32;
        CHECK(0.F == Approx(CNoveltyFromBlockIf::compNoveltyHainsworth(pfInput, &pfInput[iIdx], iBufferLength / 2, fSampleRate)).margin(1e-6F).epsilon(1e-6F));

        // 'noise' test
        CVectorFloat::setValue(pfInput, 1, iBufferLength);
        CHECK(0.F == Approx(CNoveltyFromBlockIf::compNoveltyHainsworth(pfInput, &pfInput[iIdx], iBufferLength / 2)).margin(1e-6F).epsilon(1e-6F));

        // one spectrum zero, the other one
        CVectorFloat::setZero(pfInput, iBufferLength / 2);
        CHECK(std::log2(fEpsilon) == Approx(CNoveltyFromBlockIf::compNoveltyHainsworth(pfInput, &pfInput[iIdx], iBufferLength / 2)).margin(1e-4F).epsilon(1e-4F));
        CHECK(std::log2(1.F/ fEpsilon) == Approx(CNoveltyFromBlockIf::compNoveltyHainsworth(&pfInput[iIdx], pfInput, iBufferLength / 2)).margin(1e-4F).epsilon(1e-4F));

        // one spectrum zero, the other two
        CVectorFloat::mulC_I(pfInput, 2.F, iBufferLength);
        CHECK(std::log2(.5* fEpsilon) == Approx(CNoveltyFromBlockIf::compNoveltyHainsworth(pfInput, &pfInput[iIdx], iBufferLength / 2)).margin(1e-4F).epsilon(1e-4F));
        CHECK(std::log2(2.F/ fEpsilon) == Approx(CNoveltyFromBlockIf::compNoveltyHainsworth(&pfInput[iIdx], pfInput, iBufferLength / 2)).margin(1e-4F).epsilon(1e-4F));

        //decreasing spectrum
        CVectorFloat::setZero(pfInput, iBufferLength / 2);
        CVectorFloat::setValue(&pfInput[iBufferLength / 2], 1.F, iBufferLength / 2);
        for (auto n = 0; n < 5; n++)
        {
            CHECK(std::log2(fEpsilon) <= Approx(CNoveltyFromBlockIf::compNoveltyHainsworth(pfInput, &pfInput[iBufferLength / 2], iBufferLength / 2)).margin(1e-4F).epsilon(1e-4F));
            CVectorFloat::mulC_I(&pfInput[iBufferLength / 2], .75F, iBufferLength / 2);
        }

        // alternating spectral bins
        CVectorFloat::setZero(pfInput, iBufferLength);
        iIdx = 4;
        iBufferLength = 4;
        pfInput[0] = 1;
        pfInput[2] = 1;
        pfInput[iIdx + 1] = 1;
        pfInput[iIdx + 3] = 1;
        CHECK(0.F == Approx(CNoveltyFromBlockIf::compNoveltyHainsworth(pfInput, &pfInput[iIdx], iBufferLength)).margin(1e-4F).epsilon(1e-4F));
    }

    SECTION("NoveltyLaroche")
    {
        int iIdx = iBufferLength / 2;

        // zero test
        CHECK(0.F == CNoveltyFromBlockIf::compNoveltyLaroche(pfInput, &pfInput[iIdx], iBufferLength / 2));

        // 'sine' test
        pfInput[10] = 1;
        pfInput[iIdx + 10] = 1;
        fSampleRate = 2048;
        CHECK(0.F == Approx(CNoveltyFromBlockIf::compNoveltyLaroche(pfInput, &pfInput[iIdx], iBufferLength / 2, fSampleRate)).margin(1e-6F).epsilon(1e-6F));
        fSampleRate = 32;
        CHECK(0.F == Approx(CNoveltyFromBlockIf::compNoveltyLaroche(pfInput, &pfInput[iIdx], iBufferLength / 2, fSampleRate)).margin(1e-6F).epsilon(1e-6F));

        // 'noise' test
        CVectorFloat::setValue(pfInput, 1, iBufferLength);
        CHECK(0.F == Approx(CNoveltyFromBlockIf::compNoveltyLaroche(pfInput, &pfInput[iIdx], iBufferLength / 2)).margin(1e-6F).epsilon(1e-6F));

        // one spectrum zero, the other one
        CVectorFloat::setZero(pfInput, iBufferLength / 2);
        CHECK(0.F == Approx(CNoveltyFromBlockIf::compNoveltyLaroche(pfInput, &pfInput[iIdx], iBufferLength / 2)).margin(1e-4F).epsilon(1e-4F));
        CHECK(1.F == Approx(CNoveltyFromBlockIf::compNoveltyLaroche(&pfInput[iIdx], pfInput, iBufferLength / 2)).margin(1e-4F).epsilon(1e-4F));

        // one spectrum zero, the other two
        CVectorFloat::mulC_I(pfInput, 2.F, iBufferLength);
        CHECK(0.F == Approx(CNoveltyFromBlockIf::compNoveltyLaroche(pfInput, &pfInput[iIdx], iBufferLength / 2)).margin(1e-4F).epsilon(1e-4F));
        CHECK(std::sqrt(2.F) == Approx(CNoveltyFromBlockIf::compNoveltyLaroche(&pfInput[iIdx], pfInput, iBufferLength / 2)).margin(1e-4F).epsilon(1e-4F));

        //decreasing spectrum
        CVectorFloat::setZero(pfInput, iBufferLength / 2);
        CVectorFloat::setValue(&pfInput[iBufferLength / 2], 1.F, iBufferLength / 2);
        for (auto n = 0; n < 5; n++)
        {
            CHECK(0.F == Approx(CNoveltyFromBlockIf::compNoveltyLaroche(pfInput, &pfInput[iBufferLength / 2], iBufferLength / 2)).margin(1e-4F).epsilon(1e-4F));
            CVectorFloat::mulC_I(&pfInput[iBufferLength / 2], .75F, iBufferLength / 2);
        }

        // alternating spectral bins
        CVectorFloat::setZero(pfInput, iBufferLength);
        iIdx = 4;
        iBufferLength = 4;
        pfInput[0] = 1;
        pfInput[2] = 1;
        pfInput[iIdx + 1] = 1;
        pfInput[iIdx + 3] = 1;
        CHECK(2.F / 4.F == Approx(CNoveltyFromBlockIf::compNoveltyLaroche(pfInput, &pfInput[iIdx], iBufferLength)).margin(1e-4F).epsilon(1e-4F));
    }



    delete[] pfInput;
}

TEST_CASE("Novelty (class interface per block)", "[NoveltyBlockClass]")
{
    CNoveltyFromBlockIf* pCInstance = 0;
    float* pfInput = 0;
    float fSampleRate = 1;
    int iBufferLength = 1025;
    
    pfInput = new float[iBufferLength];
    CVectorFloat::setValue(pfInput, 1.F, iBufferLength);

    SECTION("Api")
    {
        CHECK(Error_t::kFunctionInvalidArgsError == CNoveltyFromBlockIf::create(pCInstance, CNoveltyIf::kNoveltyFlux, 0, fSampleRate));
        CHECK(Error_t::kFunctionInvalidArgsError == CNoveltyFromBlockIf::create(pCInstance, CNoveltyIf::kNoveltyFlux, -1, fSampleRate));
        CHECK(Error_t::kFunctionInvalidArgsError == CNoveltyFromBlockIf::create(pCInstance, CNoveltyIf::kNoveltyFlux, iBufferLength, 0));
        CHECK(Error_t::kFunctionInvalidArgsError == CNoveltyFromBlockIf::create(pCInstance, CNoveltyIf::kNoveltyFlux, iBufferLength, -1));

        CHECK(Error_t::kNoError == CNoveltyFromBlockIf::create(pCInstance, CNoveltyIf::kNoveltyFlux, iBufferLength, fSampleRate));
        CHECK(1 == pCInstance->getNoveltyDimension());
        CHECK(Error_t::kNoError == CNoveltyFromBlockIf::destroy(pCInstance));
    }

    SECTION("NoveltyCalc")
    {
        fSampleRate = 16000.F;
        for (auto k = 0; k < CNoveltyIf::kNumNoveltyFunctions; k++)
        {
            float fResult = 0;
            CHECK(Error_t::kNoError == CNoveltyFromBlockIf::create(pCInstance, static_cast<CNoveltyIf::Novelty_t>(k), iBufferLength, fSampleRate));
            CHECK(Error_t::kNoError == pCInstance->compNovelty(&fResult, pfInput));
            CHECK(Error_t::kNoError == CNoveltyFromBlockIf::destroy(pCInstance));
        }
    }

    CNoveltyFromBlockIf::destroy(pCInstance);

    delete[] pfInput;
}

TEST_CASE("Novelty (per array)", "[NoveltyClass]")
{

    CNoveltyIf* pCInstance = 0;
    float* pfInput = 0;
    float* pfNovelty = 0;
    bool* pbOnset = 0;
    float fSampleRate = 44100;
    int iBlockLength = 1023,
        iHopLength = 512,
        iBufferLength = 96000;
    int iDim = 0;

    pbOnset = new bool[188];
    pfNovelty = new float[188];
    pfInput = new float[iBufferLength];
    CVectorFloat::setZero(pfInput, iBufferLength);


    SECTION("Api")
    {
        for (auto f = 0; f < CNoveltyIf::kNumNoveltyFunctions; f++)
        {
            CHECK(Error_t::kFunctionInvalidArgsError == CNoveltyIf::create(pCInstance, static_cast<CNoveltyIf::Novelty_t>(f), 0, iBufferLength, fSampleRate, iBlockLength, iHopLength));
            CHECK(Error_t::kFunctionInvalidArgsError == CNoveltyIf::create(pCInstance, static_cast<CNoveltyIf::Novelty_t>(f), pfInput, 0, fSampleRate, iBlockLength, iHopLength));
            CHECK(Error_t::kFunctionInvalidArgsError == CNoveltyIf::create(pCInstance, static_cast<CNoveltyIf::Novelty_t>(f), pfInput, -1, fSampleRate, iBlockLength, iHopLength));
            CHECK(Error_t::kFunctionInvalidArgsError == CNoveltyIf::create(pCInstance, static_cast<CNoveltyIf::Novelty_t>(f), pfInput, iBufferLength, 0, iBlockLength, iHopLength));
            CHECK(Error_t::kFunctionInvalidArgsError == CNoveltyIf::create(pCInstance, static_cast<CNoveltyIf::Novelty_t>(f), pfInput, iBufferLength, fSampleRate, 0, iHopLength));
            CHECK(Error_t::kFunctionInvalidArgsError == CNoveltyIf::create(pCInstance, static_cast<CNoveltyIf::Novelty_t>(f), pfInput, iBufferLength, fSampleRate, iBlockLength, 0));

            CHECK(Error_t::kNoError == CNoveltyIf::create(pCInstance, static_cast<CNoveltyIf::Novelty_t>(f), pfInput, iBufferLength, fSampleRate, iBlockLength, iHopLength));

            CHECK_FALSE(pCInstance == 0);

            CHECK(Error_t::kNoError == pCInstance->getNumBlocks(iDim));
            CHECK(iDim == 188);

            CHECK(Error_t::kFunctionInvalidArgsError == pCInstance->compNovelty(0));

            CHECK(Error_t::kNoError == CNoveltyIf::destroy(pCInstance));

        }
    }

    SECTION("Output")
    {
        CVectorFloat::setValue(&pfInput[10 * 512], 1.F, 512);
        CHECK(Error_t::kNoError == CNoveltyIf::create(pCInstance, CNoveltyIf::kNoveltyFlux, pfInput, iBufferLength, fSampleRate));
        CHECK_FALSE(pCInstance == 0);

        CHECK(Error_t::kNoError == pCInstance->getNumBlocks(iDim));

        CHECK(Error_t::kNoError == pCInstance->compNovelty(pfNovelty, pbOnset));

        for (auto n = 0; n < iDim; n++)
        {
            if (n != 5)
                CHECK_FALSE(pbOnset[n]);
            else
                CHECK(pbOnset[n]);

        }
        CHECK(Error_t::kNoError == CNoveltyIf::destroy(pCInstance));
    }

    CNoveltyIf::destroy(pCInstance);

    delete[] pfInput;
    delete[] pfNovelty;
    delete[] pbOnset;

}

#endif //WITH_TESTS
