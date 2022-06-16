#include "AcaAll.h"

#ifdef WITH_TESTS
#include "catch.hpp"

TEST_CASE("Novelty (static functions)", "[NoveltyStatic]")
{

    float* pfIn = 0;
    float fSampleRate = 1;
    int iLenBuff = 1025;

    pfIn = new float[iLenBuff];
    CVector::setZero(pfIn, iLenBuff);

    SECTION("NoveltyFlux")
    {
        int iIdx = iLenBuff / 2;

        // zero test
        CHECK(0.F == CNoveltyFromBlockIf::compNoveltyFlux(pfIn, &pfIn[iIdx], iLenBuff / 2));

        // 'sine' test
        pfIn[10] = 1;
        pfIn[iIdx + 10] = 1;
        fSampleRate = 2048;
        CHECK(0.F == Approx(CNoveltyFromBlockIf::compNoveltyFlux(pfIn, &pfIn[iIdx], iLenBuff / 2, fSampleRate)).margin(1e-6F).epsilon(1e-6F));
        fSampleRate = 32;
        CHECK(0.F == Approx(CNoveltyFromBlockIf::compNoveltyFlux(pfIn, &pfIn[iIdx], iLenBuff / 2, fSampleRate)).margin(1e-6F).epsilon(1e-6F));

        // 'noise' test
        CVector::setValue(pfIn, 1.F, iLenBuff);
        CHECK(0.F == Approx(CNoveltyFromBlockIf::compNoveltyFlux(pfIn, &pfIn[iIdx], iLenBuff / 2)).margin(1e-6F).epsilon(1e-6F));

        // one spectrum zero, the other one
        CVector::setZero(pfIn, iLenBuff / 2);
        CHECK(0.F == Approx(CNoveltyFromBlockIf::compNoveltyFlux(pfIn, &pfIn[iIdx], iLenBuff / 2)).margin(1e-4F).epsilon(1e-4F));
        CHECK(1.F / std::sqrt(iLenBuff / 2.F) == Approx(CNoveltyFromBlockIf::compNoveltyFlux(&pfIn[iIdx], pfIn, iLenBuff / 2)).margin(1e-4F).epsilon(1e-4F));

        // one spectrum zero, the other two
        CVector::mulC_I(pfIn, 2.F, iLenBuff);
        CHECK(0.F == Approx(CNoveltyFromBlockIf::compNoveltyFlux(pfIn, &pfIn[iIdx], iLenBuff / 2)).margin(1e-4F).epsilon(1e-4F));
        CHECK(std::sqrt(4.F * (iLenBuff / 2)) / (iLenBuff / 2) == Approx(CNoveltyFromBlockIf::compNoveltyFlux(&pfIn[iIdx], pfIn, iLenBuff / 2)).margin(1e-4F).epsilon(1e-4F));

        //decreasing spectrum
        CVector::setZero(pfIn, iLenBuff / 2);
        CVector::setValue(&pfIn[iLenBuff / 2], 1.F, iLenBuff / 2);
        for (auto n = 0; n < 5; n++)
        {
            CHECK(0.F == Approx(CNoveltyFromBlockIf::compNoveltyFlux(pfIn, &pfIn[iLenBuff / 2], iLenBuff / 2)).margin(1e-4F).epsilon(1e-4F));
            CVector::mulC_I(&pfIn[iLenBuff / 2], .75F, iLenBuff / 2);
        }

        // alternating spectral bins
        CVector::setZero(pfIn, iLenBuff);
        iIdx = 4;
        iLenBuff = 4;
        pfIn[0] = 1;
        pfIn[2] = 1;
        pfIn[iIdx + 1] = 1;
        pfIn[iIdx + 3] = 1;
        CHECK(std::sqrt(2.F) / 4.F == Approx(CNoveltyFromBlockIf::compNoveltyFlux(pfIn, &pfIn[iIdx], iLenBuff)).margin(1e-4F).epsilon(1e-4F));
    }

    SECTION("NoveltyHainsworth")
    {
        int iIdx = iLenBuff / 2;

        float fEpsilon = 1e-5F;

        // zero test
        CHECK(0.F == CNoveltyFromBlockIf::compNoveltyHainsworth(pfIn, &pfIn[iIdx], iLenBuff / 2));

        // 'sine' test
        pfIn[10] = 1;
        pfIn[iIdx + 10] = 1;
        fSampleRate = 2048;
        CHECK(0.F == Approx(CNoveltyFromBlockIf::compNoveltyHainsworth(pfIn, &pfIn[iIdx], iLenBuff / 2, fSampleRate)).margin(1e-6F).epsilon(1e-6F));
        fSampleRate = 32;
        CHECK(0.F == Approx(CNoveltyFromBlockIf::compNoveltyHainsworth(pfIn, &pfIn[iIdx], iLenBuff / 2, fSampleRate)).margin(1e-6F).epsilon(1e-6F));

        // 'noise' test
        CVector::setValue(pfIn, 1.F, iLenBuff);
        CHECK(0.F == Approx(CNoveltyFromBlockIf::compNoveltyHainsworth(pfIn, &pfIn[iIdx], iLenBuff / 2)).margin(1e-6F).epsilon(1e-6F));

        // one spectrum zero, the other one
        CVector::setZero(pfIn, iLenBuff / 2);
        CHECK(std::log2(fEpsilon) == Approx(CNoveltyFromBlockIf::compNoveltyHainsworth(pfIn, &pfIn[iIdx], iLenBuff / 2)).margin(1e-4F).epsilon(1e-4F));
        CHECK(std::log2(1.F/ fEpsilon) == Approx(CNoveltyFromBlockIf::compNoveltyHainsworth(&pfIn[iIdx], pfIn, iLenBuff / 2)).margin(1e-4F).epsilon(1e-4F));

        // one spectrum zero, the other two
        CVector::mulC_I(pfIn, 2.F, iLenBuff);
        CHECK(std::log2(.5* fEpsilon) == Approx(CNoveltyFromBlockIf::compNoveltyHainsworth(pfIn, &pfIn[iIdx], iLenBuff / 2)).margin(1e-4F).epsilon(1e-4F));
        CHECK(std::log2(2.F/ fEpsilon) == Approx(CNoveltyFromBlockIf::compNoveltyHainsworth(&pfIn[iIdx], pfIn, iLenBuff / 2)).margin(1e-4F).epsilon(1e-4F));

        //decreasing spectrum
        CVector::setZero(pfIn, iLenBuff / 2);
        CVector::setValue(&pfIn[iLenBuff / 2], 1.F, iLenBuff / 2);
        for (auto n = 0; n < 5; n++)
        {
            CHECK(std::log2(fEpsilon) <= Approx(CNoveltyFromBlockIf::compNoveltyHainsworth(pfIn, &pfIn[iLenBuff / 2], iLenBuff / 2)).margin(1e-4F).epsilon(1e-4F));
            CVector::mulC_I(&pfIn[iLenBuff / 2], .75F, iLenBuff / 2);
        }

        // alternating spectral bins
        CVector::setZero(pfIn, iLenBuff);
        iIdx = 4;
        iLenBuff = 4;
        pfIn[0] = 1;
        pfIn[2] = 1;
        pfIn[iIdx + 1] = 1;
        pfIn[iIdx + 3] = 1;
        CHECK(0.F == Approx(CNoveltyFromBlockIf::compNoveltyHainsworth(pfIn, &pfIn[iIdx], iLenBuff)).margin(1e-4F).epsilon(1e-4F));
    }

    SECTION("NoveltyLaroche")
    {
        int iIdx = iLenBuff / 2;

        // zero test
        CHECK(0.F == CNoveltyFromBlockIf::compNoveltyLaroche(pfIn, &pfIn[iIdx], iLenBuff / 2));

        // 'sine' test
        pfIn[10] = 1;
        pfIn[iIdx + 10] = 1;
        fSampleRate = 2048;
        CHECK(0.F == Approx(CNoveltyFromBlockIf::compNoveltyLaroche(pfIn, &pfIn[iIdx], iLenBuff / 2, fSampleRate)).margin(1e-6F).epsilon(1e-6F));
        fSampleRate = 32;
        CHECK(0.F == Approx(CNoveltyFromBlockIf::compNoveltyLaroche(pfIn, &pfIn[iIdx], iLenBuff / 2, fSampleRate)).margin(1e-6F).epsilon(1e-6F));

        // 'noise' test
        CVector::setValue(pfIn, 1.F, iLenBuff);
        CHECK(0.F == Approx(CNoveltyFromBlockIf::compNoveltyLaroche(pfIn, &pfIn[iIdx], iLenBuff / 2)).margin(1e-6F).epsilon(1e-6F));

        // one spectrum zero, the other one
        CVector::setZero(pfIn, iLenBuff / 2);
        CHECK(0.F == Approx(CNoveltyFromBlockIf::compNoveltyLaroche(pfIn, &pfIn[iIdx], iLenBuff / 2)).margin(1e-4F).epsilon(1e-4F));
        CHECK(1.F == Approx(CNoveltyFromBlockIf::compNoveltyLaroche(&pfIn[iIdx], pfIn, iLenBuff / 2)).margin(1e-4F).epsilon(1e-4F));

        // one spectrum zero, the other two
        CVector::mulC_I(pfIn, 2.F, iLenBuff);
        CHECK(0.F == Approx(CNoveltyFromBlockIf::compNoveltyLaroche(pfIn, &pfIn[iIdx], iLenBuff / 2)).margin(1e-4F).epsilon(1e-4F));
        CHECK(std::sqrt(2.F) == Approx(CNoveltyFromBlockIf::compNoveltyLaroche(&pfIn[iIdx], pfIn, iLenBuff / 2)).margin(1e-4F).epsilon(1e-4F));

        //decreasing spectrum
        CVector::setZero(pfIn, iLenBuff / 2);
        CVector::setValue(&pfIn[iLenBuff / 2], 1.F, iLenBuff / 2);
        for (auto n = 0; n < 5; n++)
        {
            CHECK(0.F == Approx(CNoveltyFromBlockIf::compNoveltyLaroche(pfIn, &pfIn[iLenBuff / 2], iLenBuff / 2)).margin(1e-4F).epsilon(1e-4F));
            CVector::mulC_I(&pfIn[iLenBuff / 2], .75F, iLenBuff / 2);
        }

        // alternating spectral bins
        CVector::setZero(pfIn, iLenBuff);
        iIdx = 4;
        iLenBuff = 4;
        pfIn[0] = 1;
        pfIn[2] = 1;
        pfIn[iIdx + 1] = 1;
        pfIn[iIdx + 3] = 1;
        CHECK(2.F / 4.F == Approx(CNoveltyFromBlockIf::compNoveltyLaroche(pfIn, &pfIn[iIdx], iLenBuff)).margin(1e-4F).epsilon(1e-4F));
    }



    delete[] pfIn;
}

TEST_CASE("Novelty (class interface per block)", "[NoveltyBlockClass]")
{
    CNoveltyFromBlockIf* pCInstance = 0;
    float* pfIn = 0;
    float fSampleRate = 1;
    int iBufferLength = 1025;
    
    pfIn = new float[iBufferLength];
    CVector::setValue(pfIn, 1.F, iBufferLength);

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
            CHECK(Error_t::kNoError == pCInstance->compNovelty(&fResult, pfIn));
            CHECK(Error_t::kNoError == CNoveltyFromBlockIf::destroy(pCInstance));
        }
    }

    CNoveltyFromBlockIf::destroy(pCInstance);

    delete[] pfIn;
}

TEST_CASE("Novelty (per array)", "[NoveltyClass]")
{

    CNoveltyIf* pCInstance = 0;
    float* pfIn = 0;
    float* pfNovelty = 0;
    bool* pbOnset = 0;
    float fSampleRate = 44100;
    int iBlockLength = 1023,
        iHopLength = 512,
        iBuffLength = 96000;
    int iDim = 0;

    pbOnset = new bool[188];
    pfNovelty = new float[188];
    pfIn = new float[iBuffLength];
    CVector::setZero(pfIn, iBuffLength);


    SECTION("Api")
    {
        for (auto f = 0; f < CNoveltyIf::kNumNoveltyFunctions; f++)
        {
            CHECK(Error_t::kFunctionInvalidArgsError == CNoveltyIf::create(pCInstance, static_cast<CNoveltyIf::Novelty_t>(f), 0, iBuffLength, fSampleRate, iBlockLength, iHopLength));
            CHECK(Error_t::kFunctionInvalidArgsError == CNoveltyIf::create(pCInstance, static_cast<CNoveltyIf::Novelty_t>(f), pfIn, 0, fSampleRate, iBlockLength, iHopLength));
            CHECK(Error_t::kFunctionInvalidArgsError == CNoveltyIf::create(pCInstance, static_cast<CNoveltyIf::Novelty_t>(f), pfIn, -1, fSampleRate, iBlockLength, iHopLength));
            CHECK(Error_t::kFunctionInvalidArgsError == CNoveltyIf::create(pCInstance, static_cast<CNoveltyIf::Novelty_t>(f), pfIn, iBuffLength, 0, iBlockLength, iHopLength));
            CHECK(Error_t::kFunctionInvalidArgsError == CNoveltyIf::create(pCInstance, static_cast<CNoveltyIf::Novelty_t>(f), pfIn, iBuffLength, fSampleRate, 0, iHopLength));
            CHECK(Error_t::kFunctionInvalidArgsError == CNoveltyIf::create(pCInstance, static_cast<CNoveltyIf::Novelty_t>(f), pfIn, iBuffLength, fSampleRate, iBlockLength, 0));

            CHECK(Error_t::kNoError == CNoveltyIf::create(pCInstance, static_cast<CNoveltyIf::Novelty_t>(f), pfIn, iBuffLength, fSampleRate, iBlockLength, iHopLength));

            CHECK_FALSE(pCInstance == 0);

            CHECK(Error_t::kNoError == pCInstance->getNumBlocks(iDim));
            CHECK(iDim == 188);

            CHECK(Error_t::kFunctionInvalidArgsError == pCInstance->compNovelty(0));

            CHECK(Error_t::kNoError == CNoveltyIf::destroy(pCInstance));

        }
    }

    SECTION("Output")
    {
        CVector::setValue(&pfIn[10 * 512], 1.F, 512);
        CHECK(Error_t::kNoError == CNoveltyIf::create(pCInstance, CNoveltyIf::kNoveltyFlux, pfIn, iBuffLength, fSampleRate));
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

    delete[] pfIn;
    delete[] pfNovelty;
    delete[] pbOnset;

}

#endif //WITH_TESTS
