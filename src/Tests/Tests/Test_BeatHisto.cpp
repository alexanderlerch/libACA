#include "AcaAll.h"

#ifdef WITH_TESTS
#include "catch.hpp"

TEST_CASE("BeatHisto", "[BeatHistoClass]")
{

    CBeatHistoIf* pCInstance = 0;
    float* pfIn = 0;
    float* pfBeatHisto = 0;
    float* pfBeatTicks = 0;
    float fSampleRate = 44100;
    int iBlockLength = 1023,
        iHopLength = 512,
        iLenBuff = static_cast<int>(fSampleRate * 10);
    int iDim = 0;

    pfBeatHisto = new float[65536];
    pfBeatTicks = new float[65536];
    pfIn = new float[iLenBuff];
    CVector::setZero(pfIn, iLenBuff);


    SECTION("Api")
    {
        iLenBuff = 5000;
        CHECK(Error_t::kFunctionInvalidArgsError == CBeatHistoIf::create(pCInstance, 0, iLenBuff, fSampleRate, iBlockLength, iHopLength));
        CHECK(Error_t::kFunctionInvalidArgsError == CBeatHistoIf::create(pCInstance, pfIn, 0, fSampleRate, iBlockLength, iHopLength));
        CHECK(Error_t::kFunctionInvalidArgsError == CBeatHistoIf::create(pCInstance, pfIn, iLenBuff, 0, iBlockLength, iHopLength));
        CHECK(Error_t::kFunctionInvalidArgsError == CBeatHistoIf::create(pCInstance, pfIn, iLenBuff, fSampleRate, 0, iHopLength));
        CHECK(Error_t::kFunctionInvalidArgsError == CBeatHistoIf::create(pCInstance, pfIn, iLenBuff, fSampleRate, iBlockLength, 0));

        CHECK(Error_t::kNoError == CBeatHistoIf::create(pCInstance, pfIn, iLenBuff, fSampleRate, iBlockLength, iHopLength));

        CHECK_FALSE(pCInstance == 0);

        CHECK(Error_t::kNoError == pCInstance->getNumBins(iDim, CBeatHistoIf::kBeatHistoFft));
        CHECK(iDim == 4314);

        CHECK(Error_t::kFunctionInvalidArgsError == pCInstance->getBeatHistoAxisTicks(0, CBeatHistoIf::kBeatHistoFft));
        CHECK(Error_t::kNoError == pCInstance->getBeatHistoAxisTicks(pfBeatTicks, CBeatHistoIf::kBeatHistoFft));

        CHECK(Error_t::kFunctionInvalidArgsError == pCInstance->compBeatHisto(0, CBeatHistoIf::kBeatHistoFft));
        CHECK(Error_t::kNoError == pCInstance->compBeatHisto(pfBeatHisto, CBeatHistoIf::kBeatHistoFft));

        CHECK(Error_t::kNoError == CBeatHistoIf::destroy(pCInstance));
    }

    SECTION("Fft")
    {
        // bursts of 512 at 120BPM
        for (auto s = 0; s < 20; s++)
        {
            int iStart = static_cast<int>(s * fSampleRate / 2);
            CVector::setValue(&pfIn[iStart], 1.F, 512);
        }
        CHECK(Error_t::kNoError == CBeatHistoIf::create(pCInstance, pfIn, iLenBuff, fSampleRate));
        CHECK_FALSE(pCInstance == 0);

        CHECK(Error_t::kNoError == pCInstance->compBeatHisto(pfBeatHisto));
        CHECK(Error_t::kNoError == pCInstance->getBeatHistoAxisTicks(pfBeatTicks, CBeatHistoIf::kBeatHistoFft));

        float fMax = 0;
        long long iMax = -1;
        CVector::findMax(pfBeatHisto, fMax, iMax, pCInstance->getNumBins(CBeatHistoIf::kBeatHistoFft));
        CHECK(1.F == Approx(fMax).margin(1e-6F).epsilon(1e-6F));
        CHECK(37 == iMax);
        CHECK(std::abs(pfBeatTicks[iMax] - 120.F) <= 1.5F);


        CHECK(Error_t::kNoError == CBeatHistoIf::destroy(pCInstance));
    }

    SECTION("Corr")
    {
        // bursts of 512 at 120BPM
        for (auto s = 0; s < 20; s++)
        {
            int iStart = static_cast<int>(s * fSampleRate / 2);
            CVector::setValue(&pfIn[iStart], 1.F, 512);
        }
        CHECK(Error_t::kNoError == CBeatHistoIf::create(pCInstance, pfIn, iLenBuff, fSampleRate));
        CHECK_FALSE(pCInstance == 0);

        CHECK(Error_t::kNoError == pCInstance->compBeatHisto(pfBeatHisto, CBeatHistoIf::kBeatHistoCorr));
        CHECK(Error_t::kNoError == pCInstance->getBeatHistoAxisTicks(pfBeatTicks, CBeatHistoIf::kBeatHistoCorr));

        float fMax = 0;
        long long iMax = -1;
        CVector::findMax(pfBeatHisto, fMax, iMax, pCInstance->getNumBins(CBeatHistoIf::kBeatHistoCorr));
        CHECK(1.F == Approx(fMax).margin(1e-6F).epsilon(1e-6F));
        CHECK(8271 == iMax);
        CHECK(std::abs(pfBeatTicks[iMax] - 120.F) <= 1.F);


        CHECK(Error_t::kNoError == CBeatHistoIf::destroy(pCInstance));
    }

    CHECK(Error_t::kNoError == CBeatHistoIf::destroy(pCInstance));

    delete[] pfIn;
    delete[] pfBeatHisto;
    delete[] pfBeatTicks;
}

#endif //WITH_TESTS
