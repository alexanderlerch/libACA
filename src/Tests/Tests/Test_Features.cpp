#include "AcaAll.h"

#ifdef WITH_TESTS
#include "catch.hpp"

TEST_CASE("Features (static functions)", "[FeaturesStatic]")
{

    float* pfInput = 0;
    float fSampleRate = 1;
    int iBufferLength = 1025;

    pfInput = new float[iBufferLength];
    CVector::setZero(pfInput, iBufferLength);

    SECTION("SpectralCentroid")
    {
        // zero test
        CHECK(0.F == CFeatureFromBlockIf::compFeatureSpectralCentroid(pfInput, iBufferLength));

        // 'sine' test
        pfInput[10] = 1;
        fSampleRate = 2048;
        CHECK(10.F == Approx(CFeatureFromBlockIf::compFeatureSpectralCentroid(pfInput, iBufferLength, fSampleRate)).margin(1e-6F).epsilon(1e-6F));
        fSampleRate = 32;
        CHECK(10.F == Approx(CFeatureFromBlockIf::compFeatureSpectralCentroid(pfInput, 17, fSampleRate)).margin(1e-6F).epsilon(1e-6F));

        // 'noise' test
        CVector::setValue(pfInput, 1.F, iBufferLength);
        CHECK(fSampleRate / 4.F == Approx(CFeatureFromBlockIf::compFeatureSpectralCentroid(pfInput, iBufferLength, fSampleRate)).margin(1e-6F).epsilon(1e-6F));

        // symmetric spectrum test
        CVector::setZero(&pfInput[1], static_cast<long long>(iBufferLength) - 2);
        CHECK(fSampleRate / 4.F == Approx(CFeatureFromBlockIf::compFeatureSpectralCentroid(pfInput, iBufferLength, fSampleRate)).margin(1e-6F).epsilon(1e-6F));
    }

    SECTION("SpectralCrestFactor")
    {
        // zero test
        CHECK(0.F == CFeatureFromBlockIf::compFeatureSpectralCrestFactor(pfInput, iBufferLength));

        // 'sine' test
        pfInput[10] = 1;
        fSampleRate = 2048;
        CHECK(1.F == CFeatureFromBlockIf::compFeatureSpectralCrestFactor(pfInput, iBufferLength, fSampleRate));
        fSampleRate = 32;
        CHECK(1.F == CFeatureFromBlockIf::compFeatureSpectralCrestFactor(pfInput, 17, fSampleRate));

        // 'noise' test
        CVector::setValue(pfInput, 1.F, iBufferLength);
        CHECK(1.F / iBufferLength == Approx(CFeatureFromBlockIf::compFeatureSpectralCrestFactor(pfInput, iBufferLength)).margin(1e-6F).epsilon(1e-6F));

        // symmetric spectrum test
        CVector::setZero(&pfInput[1], static_cast<long long>(iBufferLength) - 2);
        CHECK(.5F == Approx(CFeatureFromBlockIf::compFeatureSpectralCrestFactor(pfInput, iBufferLength, fSampleRate)).margin(1e-6F).epsilon(1e-6F));
    }

    SECTION("SpectralDecrease")
    {
        // zero test
        CHECK(0.F == Approx(CFeatureFromBlockIf::compFeatureSpectralDecrease(pfInput, iBufferLength)).margin(1e-6F).epsilon(1e-6F));

        // increasing spectrum
        for (auto k = 0; k < iBufferLength; k++)
            pfInput[k] = static_cast<float>(k);

        CHECK((iBufferLength - 1) / CVector::getSum(pfInput, iBufferLength) == Approx(CFeatureFromBlockIf::compFeatureSpectralDecrease(pfInput, iBufferLength)).margin(1e-6F).epsilon(1e-6F));
    }

    SECTION("SpectralFlatness")
    {
        // zero test
        CHECK(0.F == Approx(CFeatureFromBlockIf::compFeatureSpectralFlatness(pfInput, iBufferLength)).margin(1e-6F).epsilon(1e-6F));

        // 'sine' test
        pfInput[10] = 1;
        fSampleRate = 2048;
        CHECK(0.F == Approx(CFeatureFromBlockIf::compFeatureSpectralFlatness(pfInput, iBufferLength, fSampleRate)).margin(1e-6F).epsilon(1e-6F));
        fSampleRate = 32;
        CHECK(0.F == Approx(CFeatureFromBlockIf::compFeatureSpectralFlatness(pfInput, 17, fSampleRate)).margin(1e-6F).epsilon(1e-6F));

        // 'noise' test
        CVector::setValue(pfInput, 1.F, iBufferLength);
        CHECK(1.F == CFeatureFromBlockIf::compFeatureSpectralFlatness(pfInput, iBufferLength));

        // one value zero
        pfInput[10] = 0;
        CHECK(0.F == Approx(CFeatureFromBlockIf::compFeatureSpectralFlatness(pfInput, 20, fSampleRate)).margin(1e-6F).epsilon(1e-6F));
        // 'sine w noise' test
        CVector::setValue(pfInput, 1.F, iBufferLength);
        pfInput[2] = 10;
        CHECK(std::sqrt(std::sqrt(10)) / (13.F / 4) == Approx(CFeatureFromBlockIf::compFeatureSpectralFlatness(pfInput, 4)).margin(1e-6F).epsilon(1e-6F));
    }

    SECTION("SpectralFlux")
    {
        int iIdx = iBufferLength / 2;
        // zero test
        CHECK(0.F == CFeatureFromBlockIf::compFeatureSpectralFlux(pfInput, &pfInput[iIdx], iBufferLength / 2));

        // 'sine' test
        pfInput[10] = 1;
        pfInput[iIdx + 10] = 1;
        fSampleRate = 2048;
        CHECK(0.F == Approx(CFeatureFromBlockIf::compFeatureSpectralFlux(pfInput, &pfInput[iIdx], iBufferLength / 2, fSampleRate)).margin(1e-6F).epsilon(1e-6F));
        fSampleRate = 32;
        CHECK(0.F == Approx(CFeatureFromBlockIf::compFeatureSpectralFlux(pfInput, &pfInput[iIdx], iBufferLength / 2, fSampleRate)).margin(1e-6F).epsilon(1e-6F));

        // 'noise' test
        CVector::setValue(pfInput, 1.F, iBufferLength);
        CHECK(0.F == Approx(CFeatureFromBlockIf::compFeatureSpectralFlux(pfInput, &pfInput[iIdx], iBufferLength / 2)).margin(1e-6F).epsilon(1e-6F));

        // one spectrum zero, the other one
        CVector::setZero(pfInput, iBufferLength / 2);
        CHECK(1.F / std::sqrt(iBufferLength / 2.F) == Approx(CFeatureFromBlockIf::compFeatureSpectralFlux(pfInput, &pfInput[iIdx], iBufferLength / 2)).margin(1e-4F).epsilon(1e-4F));
        CHECK(1.F / std::sqrt(iBufferLength / 2.F) == Approx(CFeatureFromBlockIf::compFeatureSpectralFlux(&pfInput[iIdx], pfInput, iBufferLength / 2)).margin(1e-4F).epsilon(1e-4F));

        // one spectrum zero, the other two
        CVector::mulC_I(pfInput, 2.F, iBufferLength);
        CHECK(std::sqrt(4.F * (iBufferLength / 2)) / (iBufferLength / 2) == Approx(CFeatureFromBlockIf::compFeatureSpectralFlux(pfInput, &pfInput[iIdx], iBufferLength / 2)).margin(1e-4F).epsilon(1e-4F));
        CHECK(std::sqrt(4.F * (iBufferLength / 2)) / (iBufferLength / 2) == Approx(CFeatureFromBlockIf::compFeatureSpectralFlux(&pfInput[iIdx], pfInput, iBufferLength / 2)).margin(1e-4F).epsilon(1e-4F));

        // alternating spectral bins
        iIdx = 4;
        iBufferLength = 4;
        pfInput[0] = 1;
        pfInput[2] = 1;
        pfInput[iIdx + 1] = 1;
        pfInput[iIdx + 3] = 1;
        CHECK(1.F / std::sqrt(static_cast<float>(iBufferLength)) == Approx(CFeatureFromBlockIf::compFeatureSpectralFlux(pfInput, &pfInput[iIdx], iBufferLength)).margin(1e-4F).epsilon(1e-4F));
    }

    SECTION("SpectralKurtosis")
    {
        // zero test
        CHECK(0.F == CFeatureFromBlockIf::compFeatureSpectralKurtosis(pfInput, iBufferLength));

        // 'sine' test
        pfInput[10] = 1;
        fSampleRate = 2048;
        CHECK(0.F == CFeatureFromBlockIf::compFeatureSpectralKurtosis(pfInput, iBufferLength, fSampleRate));
        fSampleRate = 32;
        CHECK(0.F == CFeatureFromBlockIf::compFeatureSpectralKurtosis(pfInput, 17, fSampleRate));

        // flat spectrum test
        fSampleRate = 1;
        CVector::setValue(pfInput, 1.F, iBufferLength);
        CHECK(-1.2F == Approx(CFeatureFromBlockIf::compFeatureSpectralKurtosis(pfInput, iBufferLength, fSampleRate)).margin(1e-4F).epsilon(1e-4F));
    }

    SECTION("SpectralRolloff")
    {
        // zero test
        CHECK(0.F == CFeatureFromBlockIf::compFeatureSpectralRolloff(pfInput, iBufferLength));

        // flat spectrum test
        fSampleRate = 200;
        CVector::setValue(pfInput, 1.F, iBufferLength);
        CHECK(85.F == Approx(CFeatureFromBlockIf::compFeatureSpectralRolloff(pfInput, 101, fSampleRate)).margin(1e-4F).epsilon(1e-4F));
        CHECK(75.F == Approx(CFeatureFromBlockIf::compFeatureSpectralRolloff(pfInput, 101, fSampleRate, .75F)).margin(1e-4F).epsilon(1e-4F));
    }

    SECTION("SpectralSkewness")
    {
        // zero test
        CHECK(0.F == CFeatureFromBlockIf::compFeatureSpectralKurtosis(pfInput, iBufferLength));

        // 'sine' test
        pfInput[512] = 1;
        fSampleRate = 2048;
        CHECK(0.F == CFeatureFromBlockIf::compFeatureSpectralSkewness(pfInput, iBufferLength, fSampleRate));
        fSampleRate = 32;
        CHECK(0.F == CFeatureFromBlockIf::compFeatureSpectralSkewness(pfInput, 17, fSampleRate));

        // flat spectrum test
        fSampleRate = 1;
        CVector::setValue(pfInput, 1.F, iBufferLength);
        CHECK(0.F == Approx(CFeatureFromBlockIf::compFeatureSpectralSkewness(pfInput, iBufferLength, fSampleRate)).margin(1e-4F).epsilon(1e-4F));

        // decreasing spectrum -> positive skewness
        for (auto k = 0; k < iBufferLength; k++)
            pfInput[k] = iBufferLength - 1.F - k;
        CHECK(0.F < CFeatureFromBlockIf::compFeatureSpectralSkewness(pfInput, iBufferLength, fSampleRate));

        // increasing spectrum -> negative skewness
        for (auto k = 0; k < iBufferLength; k++)
            pfInput[k] = static_cast<float>(k);
        CHECK_FALSE(0.F < CFeatureFromBlockIf::compFeatureSpectralSkewness(pfInput, iBufferLength, fSampleRate));

        // symmetry
        for (auto k = iBufferLength / 2; k < iBufferLength; k++)
            pfInput[k] = iBufferLength - 1.F - k;
        CHECK(CFeatureFromBlockIf::compFeatureSpectralSkewness(&pfInput[iBufferLength / 2], iBufferLength / 2, fSampleRate) == Approx(-1.F * CFeatureFromBlockIf::compFeatureSpectralSkewness(pfInput, iBufferLength / 2, fSampleRate)).margin(1e-4F).epsilon(1e-4F));
    }

    SECTION("SpectralSlope")
    {
        // zero test
        CHECK(0.F == CFeatureFromBlockIf::compFeatureSpectralSlope(pfInput, iBufferLength));

        // increasing spectrum
        for (auto k = 0; k < iBufferLength; k++)
            pfInput[k] = static_cast<float>(k);
        CHECK(1.F == Approx(CFeatureFromBlockIf::compFeatureSpectralSlope(pfInput, 5)).margin(1e-6F).epsilon(1e-6F));
        CHECK(1.F == Approx(CFeatureFromBlockIf::compFeatureSpectralSlope(pfInput, iBufferLength)).margin(1e-2F).epsilon(1e-2F));

        // flat spectrum
        CVector::setZero(pfInput, iBufferLength);
        CHECK(0.F == Approx(CFeatureFromBlockIf::compFeatureSpectralSlope(pfInput, 5)).margin(1e-6F).epsilon(1e-6F));
        CHECK(0.F == Approx(CFeatureFromBlockIf::compFeatureSpectralSlope(pfInput, iBufferLength)).margin(1e-2F).epsilon(1e-2F));
    }

    SECTION("SpectralSpread")
    {
        // zero test
        CHECK(0.F == CFeatureFromBlockIf::compFeatureSpectralSpread(pfInput, iBufferLength));

        // 'sine' test
        pfInput[10] = 1;
        fSampleRate = 2048;
        CHECK(0.F == CFeatureFromBlockIf::compFeatureSpectralSpread(pfInput, iBufferLength, fSampleRate));
        fSampleRate = 32;
        CHECK(0.F == CFeatureFromBlockIf::compFeatureSpectralSpread(pfInput, 17, fSampleRate));

        // flat spectrum test
        fSampleRate = 1;
        CVector::setZero(pfInput, iBufferLength);
        CVector::setValue(&pfInput[5], 1.F, 5);
        iBufferLength = 17;
        CHECK(std::sqrt(10.F / 5.F) / (2 * (iBufferLength - 1)) == Approx(CFeatureFromBlockIf::compFeatureSpectralSpread(pfInput, iBufferLength, fSampleRate)).margin(1e-4F).epsilon(1e-4F));
    }

    SECTION("SpectralTonalPowerRatio")
    {
        // zero test
        CHECK(0.F == CFeatureFromBlockIf::compFeatureSpectralTonalPowerRatio(pfInput, iBufferLength));

        // 'sine' test
        pfInput[10] = 1;
        fSampleRate = 2048;
        CHECK(1.F == CFeatureFromBlockIf::compFeatureSpectralTonalPowerRatio(pfInput, iBufferLength, fSampleRate));
        fSampleRate = 32;
        CHECK(1.F == CFeatureFromBlockIf::compFeatureSpectralTonalPowerRatio(pfInput, 17, fSampleRate));

        // multiple maxima 
        CVector::setValue(pfInput, .5F, iBufferLength);
        pfInput[100] = 2.F;
        pfInput[200] = 2.F;
        pfInput[300] = 2.F;
        pfInput[400] = 2.F;
        pfInput[500] = 2.F;
        CHECK(5.F * 4.F / (.25 * (iBufferLength - 5) + 5.F * 4.F) == Approx(CFeatureFromBlockIf::compFeatureSpectralTonalPowerRatio(pfInput, iBufferLength, fSampleRate)).margin(1e-4F).epsilon(1e-4F));
    }

    SECTION("TimeAcfCoeff")
    {
        int eta = 0;

        // zero test
        CHECK(0.F == CFeatureFromBlockIf::compFeatureTimeAcfCoeff(pfInput, iBufferLength));

        // dc input
        CVector::setValue(pfInput, 1.F, 20);
        for (eta = 0; eta < 19; eta++)
            CHECK(CFeatureFromBlockIf::compFeatureTimeAcfCoeff(pfInput, 20, fSampleRate, eta) > CFeatureFromBlockIf::compFeatureTimeAcfCoeff(pfInput, 20, fSampleRate, eta + 1));

        // sine wave
        eta = 500;
        fSampleRate = 1000;
        CSynthesis::genSine<float>(pfInput, 2, fSampleRate, iBufferLength, 1.F);
        CHECK(500 * (1.F - eta / 1000.F) == Approx(CFeatureFromBlockIf::compFeatureTimeAcfCoeff(pfInput, 1000, fSampleRate, eta)).margin(1e-4F).epsilon(1e-4F));
    }

    SECTION("TimePeakEnvelope")
    {
        // zero test
        CHECK(0.F == CFeatureFromBlockIf::compFeatureTimePeakEnvelope(pfInput, iBufferLength));

        // offset
        CVector::setValue(pfInput, 1.F, iBufferLength);
        CHECK(1.F == Approx(CFeatureFromBlockIf::compFeatureTimePeakEnvelope(pfInput, iBufferLength)).margin(1e-3F).epsilon(1e-3F));

        // one maximum
        pfInput[117] = 2.F;
        CHECK(2.F == Approx(CFeatureFromBlockIf::compFeatureTimePeakEnvelope(pfInput, iBufferLength)).margin(1e-3F).epsilon(1e-3F));

        // negative maximm
        CVector::addC_I(pfInput, -5.F, iBufferLength);
        CHECK(-3.F == Approx(CFeatureFromBlockIf::compFeatureTimePeakEnvelope(pfInput, iBufferLength)).margin(1e-3F).epsilon(1e-3F));

        // sine wave
        fSampleRate = 200;
        CSynthesis::genSine<float>(pfInput, 1, fSampleRate, iBufferLength, .7F);
        CHECK(.7F == Approx(CFeatureFromBlockIf::compFeatureTimePeakEnvelope(pfInput, iBufferLength)).margin(1e-6F).epsilon(1e-6F));
    }

    SECTION("TimeRms")
    {
        // zero test
        CHECK(0.F == CFeatureFromBlockIf::compFeatureTimeRms(pfInput, iBufferLength));

        // sine wave
        fSampleRate = 200;
        CSynthesis::genSine<float>(pfInput, 1, fSampleRate, iBufferLength);
        CHECK(1.F / std::sqrt(2.F) == Approx(CFeatureFromBlockIf::compFeatureTimeRms(pfInput, 1000)).margin(1e-6F).epsilon(1e-6F));

        // square wave
        fSampleRate = 200;
        CSynthesis::genRect<float>(pfInput, 1, fSampleRate, iBufferLength);
        CHECK(1.F == Approx(CFeatureFromBlockIf::compFeatureTimeRms(pfInput, 1000)).margin(1e-3F).epsilon(1e-3F));

        // square wave with offset
        CVector::addC_I(pfInput, 1.F, iBufferLength);
        CHECK(std::sqrt(2.F) == Approx(CFeatureFromBlockIf::compFeatureTimeRms(pfInput, 1000)).margin(1e-3F).epsilon(1e-3F));
    }

    SECTION("TimeStd")
    {
        // zero test
        CHECK(0.F == CFeatureFromBlockIf::compFeatureTimeStd(pfInput, iBufferLength));

        // sine wave
        fSampleRate = 200;
        CSynthesis::genSine<float>(pfInput, 1, fSampleRate, iBufferLength);
        CHECK(1.F / std::sqrt(2.F) == Approx(CFeatureFromBlockIf::compFeatureTimeStd(pfInput, 1000)).margin(1e-6F).epsilon(1e-6F));

        // square wave
        fSampleRate = 200;
        CSynthesis::genRect<float>(pfInput, 1, fSampleRate, iBufferLength);
        CHECK(1.F == Approx(CFeatureFromBlockIf::compFeatureTimeStd(pfInput, 1000)).margin(1e-3F).epsilon(1e-3F));

        // square wave with offset
        CVector::addC_I(pfInput, 1.F, iBufferLength);
        CHECK(1.F == Approx(CFeatureFromBlockIf::compFeatureTimeStd(pfInput, 1000)).margin(1e-3F).epsilon(1e-3F));
    }

    SECTION("TimeZeroCrossingRate")
    {
        // zero test
        CHECK(0.F == CFeatureFromBlockIf::compFeatureTimeZeroCrossingRate(pfInput, iBufferLength));

        // sine wave
        fSampleRate = 200;
        CSynthesis::genSine<float>(pfInput, 1, fSampleRate, iBufferLength);
        CHECK(19.F / 2000.F == Approx(CFeatureFromBlockIf::compFeatureTimeZeroCrossingRate(pfInput, 1000)).margin(1e-6F).epsilon(1e-6F));

        // square wave
        fSampleRate = 200;
        CSynthesis::genRect<float>(pfInput, 1, fSampleRate, iBufferLength);
        CHECK(19.F / 2000.F == Approx(CFeatureFromBlockIf::compFeatureTimeZeroCrossingRate(pfInput, 1000)).margin(1e-6F).epsilon(1e-6F));
    }

    delete[] pfInput;
}

TEST_CASE("Features (class interface per block)", "[FeaturesBlockClass]")
{
    CFeatureFromBlockIf* pCInstance = 0;
    float* pfInput = 0;
    float fSampleRate = 1;
    int iBufferLength = 1025;
    
    pfInput = new float[iBufferLength];
    CVector::setValue(pfInput, 1.F, iBufferLength);

    SECTION("Api")
    {
        CHECK(Error_t::kFunctionInvalidArgsError == CFeatureFromBlockIf::create(pCInstance, CFeatureIf::kFeatureSpectralCentroid, 0, fSampleRate));
        CHECK(Error_t::kFunctionInvalidArgsError == CFeatureFromBlockIf::create(pCInstance, CFeatureIf::kFeatureSpectralCentroid, -1, fSampleRate));
        CHECK(Error_t::kFunctionInvalidArgsError == CFeatureFromBlockIf::create(pCInstance, CFeatureIf::kFeatureSpectralCentroid, iBufferLength, 0));
        CHECK(Error_t::kFunctionInvalidArgsError == CFeatureFromBlockIf::create(pCInstance, CFeatureIf::kFeatureSpectralCentroid, iBufferLength, -1));

        CHECK(Error_t::kNoError == CFeatureFromBlockIf::create(pCInstance, CFeatureIf::kFeatureSpectralCentroid, iBufferLength, fSampleRate));
        CHECK(1 == pCInstance->getFeatureDimensions());
        CHECK(Error_t::kNoError == CFeatureFromBlockIf::destroy(pCInstance));
    }

    SECTION("FeatureCalc")
    {
        fSampleRate = 16000.F;
        for (auto k = 0; k < CFeatureIf::kNumFeatures; k++)
        {
            float fResult = 0;
            CHECK(Error_t::kNoError == CFeatureFromBlockIf::create(pCInstance, static_cast<CFeatureIf::Feature_t>(k), iBufferLength, fSampleRate));
            CHECK(pCInstance->getFeatureDimensions() >= 1);
            if (pCInstance->getFeatureDimensions() == 1)
                CHECK(Error_t::kNoError == pCInstance->compFeature(&fResult, pfInput));
            CHECK(Error_t::kNoError == CFeatureFromBlockIf::destroy(pCInstance));
        }
    }

    SECTION("TimeMaxAcf")
    {
        float fResult = 0;
        CHECK(Error_t::kNoError == CFeatureFromBlockIf::create(pCInstance, CFeatureIf::kFeatureTimeMaxAcf, iBufferLength, fSampleRate));

        // ones
        CHECK(Error_t::kNoError == pCInstance->compFeature(&fResult, pfInput));
        CHECK(fResult == Approx(1.F).margin(1e-6F).epsilon(1e-6F));

        // zeros
        CVector::setZero(pfInput, iBufferLength);
        CHECK(Error_t::kNoError == pCInstance->compFeature(&fResult, pfInput));
        CHECK(fResult == Approx(0.F).margin(1e-6F).epsilon(1e-6F));

        // sine wave
        int eta = 500;
        fSampleRate = 1000;
        CSynthesis::genSine<float>(pfInput, 2, fSampleRate, iBufferLength, 1.F);
        CHECK(Error_t::kNoError == pCInstance->compFeature(&fResult, pfInput));
        CHECK((1.F - eta / 1000.F) == Approx(fResult).margin(1e-3F).epsilon(1e-3F));
    }

    SECTION("SpectralPitchChroma")
    {
        float afResult[12] = { 0,0,0,0,0,0,0,0,0,0,0,0 };
        fSampleRate = 32000;
        CHECK(Error_t::kNoError == CFeatureFromBlockIf::create(pCInstance, CFeatureIf::kFeatureSpectralPitchChroma, iBufferLength, fSampleRate));
        CHECK(12 == pCInstance->getFeatureDimensions());

        // ones
        CHECK(Error_t::kNoError == pCInstance->compFeature(afResult, pfInput));
        CHECK(1.F == Approx(CVector::getSum(afResult, 12)).margin(1e-6F).epsilon(1e-6F));
        CHECK(1.F / 12.F == Approx(CVector::getMax(afResult, 12)).margin(1e-6F).epsilon(1e-6F));
        CHECK(1.F / 12.F == Approx(CVector::getMin(afResult, 12)).margin(1e-6F).epsilon(1e-6F));

        // zeros
        CVector::setZero(pfInput, iBufferLength);
        CHECK(Error_t::kNoError == pCInstance->compFeature(afResult, pfInput));
        CHECK(0.F == Approx(CVector::getSum(afResult, 12)).margin(1e-6F).epsilon(1e-6F));
        CHECK(0.F == Approx(CVector::getMin(afResult, 12)).margin(1e-6F).epsilon(1e-6F));

        // sine 
        fSampleRate = 44000;
        CHECK(Error_t::kNoError == CFeatureFromBlockIf::destroy(pCInstance));
        CHECK(Error_t::kNoError == CFeatureFromBlockIf::create(pCInstance, CFeatureIf::kFeatureSpectralPitchChroma, 1000, fSampleRate));
        pfInput[static_cast<int>(CConversion::convertFreq2Bin(440, 2000, fSampleRate))] = 1;
        CHECK(Error_t::kNoError == pCInstance->compFeature(afResult, pfInput));
        CHECK(1.F == Approx(afResult[9]).margin(1e-6F).epsilon(1e-6F));

        // sine 
        float fNorm = 0;
        int k0 = static_cast<int>(CConversion::convertFreq2Bin(440, 2000, fSampleRate));
        fSampleRate = 44000;
        CHECK(Error_t::kNoError == CFeatureFromBlockIf::destroy(pCInstance));
        CHECK(Error_t::kNoError == CFeatureFromBlockIf::create(pCInstance, CFeatureIf::kFeatureSpectralPitchChroma, 1000, fSampleRate));
        for (auto k = 0; k < 6; k++)
        {
            pfInput[(k + 1) * k0] = 1.F / std::sqrt(k + 1.F);
            fNorm += 1.F / std::sqrt(k + 1.F);
        }
        CHECK(Error_t::kNoError == pCInstance->compFeature(afResult, pfInput));
        CHECK(afResult[9] == Approx(CVector::getMax(afResult, 12)).margin(1e-6F).epsilon(1e-6F));
        CHECK(afResult[1] > 0);
        CHECK(afResult[4] > 0);
        CHECK(1.F == Approx(CVector::getSum(afResult, 12)).margin(1e-6F).epsilon(1e-6F));
    }

    SECTION("SpectralMfccs")
    {
        float afResult[13] = { 0,0,0,0,0,0,0,0,0,0,0,0,0 };
        fSampleRate = 32000;
        CHECK(Error_t::kNoError == CFeatureFromBlockIf::create(pCInstance, CFeatureIf::kFeatureSpectralMfccs, iBufferLength, fSampleRate));
        CHECK(13 == pCInstance->getFeatureDimensions());

        // ones
        CHECK(Error_t::kNoError == pCInstance->compFeature(afResult, pfInput));
        CHECK(0.F == Approx(CVector::getSum(&afResult[1], 12)).margin(1e-2F).epsilon(1e-2F));
        CHECK(afResult[0] == Approx(CVector::getMin(afResult, 13)).margin(1e-6F).epsilon(1e-6F));
        CHECK(-7.55021658F == Approx(afResult[0]).margin(1e-6F).epsilon(1e-6F));

        // zeros
        CVector::setZero(pfInput, iBufferLength);
        CHECK(Error_t::kNoError == pCInstance->compFeature(afResult, pfInput));
        CHECK(0.F == Approx(CVector::getMean(&afResult[1], 12)).margin(1e-6F).epsilon(1e-6F));
        CHECK(afResult[0] == Approx(CVector::getMin(afResult, 13)).margin(1e-6F).epsilon(1e-6F));
        CHECK(afResult[0] < -100);
    }

    SECTION("TimeRms")
    {
        float afResult[2] = { 0,0 };
        fSampleRate = 32000;
        CHECK(Error_t::kNoError == CFeatureFromBlockIf::create(pCInstance, CFeatureIf::kFeatureTimeRms, iBufferLength, fSampleRate));
        CHECK(2 == pCInstance->getFeatureDimensions());

        // zeros
        CVector::setZero(pfInput, iBufferLength);
        CHECK(Error_t::kNoError == pCInstance->compFeature(afResult, pfInput));
        CHECK(0.F == Approx(afResult[1]).margin(1e-6F).epsilon(1e-6F));

        // ones
        float fTmp = 0;
        CVector::setValue(pfInput, 1.F, iBufferLength);
        for (auto n = 0; n < 2000; n++)
        {
            CHECK(Error_t::kNoError == pCInstance->compFeature(afResult, pfInput));
            CHECK(fTmp <= afResult[1]);
            fTmp = afResult[1];
        }
        CHECK(1.F == Approx(afResult[1]).margin(1e-4F).epsilon(1e-4F));

        // zeros
        CVector::setZero(pfInput, iBufferLength);
        CHECK(Error_t::kNoError == pCInstance->compFeature(afResult, pfInput));
        float fAlpha = CSinglePoleLp::calcFilterParam(.3F, fSampleRate);
        CHECK(std::sqrt(fAlpha) == Approx(afResult[1]).margin(1e-4F).epsilon(1e-4F));
        CHECK(Error_t::kNoError == pCInstance->compFeature(afResult, pfInput));
        CHECK(std::pow(fAlpha, iBufferLength / 2.) == Approx(afResult[1]).margin(1e-3F).epsilon(1e-3F));
    }

    SECTION("TimePeakEnvelope")
    {
        float afResult[2] = { 0,0 };
        fSampleRate = 32000;
        CHECK(Error_t::kNoError == CFeatureFromBlockIf::create(pCInstance, CFeatureIf::kFeatureTimePeakEnvelope, iBufferLength, fSampleRate));
        CHECK(2 == pCInstance->getFeatureDimensions());

        // zeros
        CVector::setZero(pfInput, iBufferLength);
        CHECK(Error_t::kNoError == pCInstance->compFeature(afResult, pfInput));
        CHECK(0.F == Approx(afResult[1]).margin(1e-6F).epsilon(1e-6F));

        // ones
        float fTmp = 0;
        CVector::setValue(pfInput, 1.F, iBufferLength);
        for (auto n = 0; n < 2000; n++)
        {
            CHECK(Error_t::kNoError == pCInstance->compFeature(afResult, pfInput));
            CHECK(fTmp <= afResult[1]);
            fTmp = afResult[1];
        }
        CHECK(1.F == Approx(afResult[1]).margin(1e-4F).epsilon(1e-4F));

        // zeros
        CVector::setZero(pfInput, iBufferLength);
        CHECK(Error_t::kNoError == pCInstance->compFeature(afResult, pfInput));
        float fAlpha = CSinglePoleLp::calcFilterParam(1.5F, fSampleRate);
        CHECK(fAlpha == Approx(afResult[1]).margin(1e-4F).epsilon(1e-4F));
        CHECK(Error_t::kNoError == pCInstance->compFeature(afResult, pfInput));
        CHECK(std::pow(fAlpha, iBufferLength) == Approx(afResult[1]).margin(1e-3F).epsilon(1e-3F));
    }

    CFeatureFromBlockIf::destroy(pCInstance);

    delete[] pfInput;
}

TEST_CASE("Features (per array)", "[FeaturesClass]")
{

    CFeatureIf* pCInstance = 0;
    float* pfInput = 0;
    float fSampleRate = 44100;
    int iBlockLength = 1023,
        iHopLength = 512,
        iBufferLength = 96000;
    
    pfInput = new float[iBufferLength];
    CVector::setValue(pfInput, 1.F, iBufferLength);


    SECTION("Api")
    {
        for (auto f = 0; f < CFeatureIf::kNumFeatures; f++)
        {
            int aiDim[2] = { 0,0 };
            float** ppfFeature = 0;
            CHECK(Error_t::kFunctionInvalidArgsError == CFeatureIf::create(pCInstance, static_cast<CFeatureIf::Feature_t>(f), 0, iBufferLength, fSampleRate, iBlockLength, iHopLength));
            CHECK(Error_t::kFunctionInvalidArgsError == CFeatureIf::create(pCInstance, static_cast<CFeatureIf::Feature_t>(f), pfInput, 0, fSampleRate, iBlockLength, iHopLength));
            CHECK(Error_t::kFunctionInvalidArgsError == CFeatureIf::create(pCInstance, static_cast<CFeatureIf::Feature_t>(f), pfInput, -1, fSampleRate, iBlockLength, iHopLength));
            CHECK(Error_t::kFunctionInvalidArgsError == CFeatureIf::create(pCInstance, static_cast<CFeatureIf::Feature_t>(f), pfInput, iBufferLength, 0, iBlockLength, iHopLength));
            CHECK(Error_t::kFunctionInvalidArgsError == CFeatureIf::create(pCInstance, static_cast<CFeatureIf::Feature_t>(f), pfInput, iBufferLength, fSampleRate, 0, iHopLength));
            CHECK(Error_t::kFunctionInvalidArgsError == CFeatureIf::create(pCInstance, static_cast<CFeatureIf::Feature_t>(f), pfInput, iBufferLength, fSampleRate, iBlockLength, 0));

            CHECK(Error_t::kNoError == CFeatureIf::create(pCInstance, static_cast<CFeatureIf::Feature_t>(f), pfInput, iBufferLength, fSampleRate, iBlockLength, iHopLength));

            CHECK_FALSE(pCInstance == 0);

            CHECK(Error_t::kNoError == pCInstance->getFeatureDimensions(aiDim[0], aiDim[1]));
            CHECK(aiDim[0] > 0);
            CHECK(aiDim[1] > 0);

            ppfFeature = new float* [aiDim[0]];
            for (auto i = 0; i < aiDim[0]; i++)
                ppfFeature[i] = new float[aiDim[1]];

            CHECK(Error_t::kFunctionInvalidArgsError == pCInstance->compFeature1Dim(0));
            CHECK(Error_t::kFunctionInvalidArgsError == pCInstance->compFeatureNDim(0));


            CHECK(Error_t::kNoError == CFeatureIf::destroy(pCInstance));

            if (ppfFeature)
            {
                for (auto i = 0; i < aiDim[0]; i++)
                    delete[] ppfFeature[i];
            }
            delete[] ppfFeature;
        }
    }
    CFeatureIf::destroy(pCInstance);

    delete[] pfInput;
}

#endif //WITH_TESTS
