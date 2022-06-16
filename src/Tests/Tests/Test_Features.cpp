#include "AcaAll.h"

#ifdef WITH_TESTS
#include "catch.hpp"

TEST_CASE("Features (static functions)", "[FeaturesStatic]")
{

    float* pfIn = 0;
    float fSampleRate = 1;
    int iLenBuff = 1025;

    pfIn = new float[iLenBuff];
    CVector::setZero(pfIn, iLenBuff);

    SECTION("SpectralCentroid")
    {
        // zero test
        CHECK(0.F == CFeatureFromBlockIf::compFeatureSpectralCentroid(pfIn, iLenBuff));

        // 'sine' test
        pfIn[10] = 1;
        fSampleRate = 2048;
        CHECK(10.F == Approx(CFeatureFromBlockIf::compFeatureSpectralCentroid(pfIn, iLenBuff, fSampleRate)).margin(1e-6F).epsilon(1e-6F));
        fSampleRate = 32;
        CHECK(10.F == Approx(CFeatureFromBlockIf::compFeatureSpectralCentroid(pfIn, 17, fSampleRate)).margin(1e-6F).epsilon(1e-6F));

        // 'noise' test
        CVector::setValue(pfIn, 1.F, iLenBuff);
        CHECK(fSampleRate / 4.F == Approx(CFeatureFromBlockIf::compFeatureSpectralCentroid(pfIn, iLenBuff, fSampleRate)).margin(1e-6F).epsilon(1e-6F));

        // symmetric spectrum test
        CVector::setZero(&pfIn[1], static_cast<long long>(iLenBuff) - 2);
        CHECK(fSampleRate / 4.F == Approx(CFeatureFromBlockIf::compFeatureSpectralCentroid(pfIn, iLenBuff, fSampleRate)).margin(1e-6F).epsilon(1e-6F));
    }

    SECTION("SpectralCrestFactor")
    {
        // zero test
        CHECK(0.F == CFeatureFromBlockIf::compFeatureSpectralCrestFactor(pfIn, iLenBuff));

        // 'sine' test
        pfIn[10] = 1;
        fSampleRate = 2048;
        CHECK(1.F == CFeatureFromBlockIf::compFeatureSpectralCrestFactor(pfIn, iLenBuff, fSampleRate));
        fSampleRate = 32;
        CHECK(1.F == CFeatureFromBlockIf::compFeatureSpectralCrestFactor(pfIn, 17, fSampleRate));

        // 'noise' test
        CVector::setValue(pfIn, 1.F, iLenBuff);
        CHECK(1.F / iLenBuff == Approx(CFeatureFromBlockIf::compFeatureSpectralCrestFactor(pfIn, iLenBuff)).margin(1e-6F).epsilon(1e-6F));

        // symmetric spectrum test
        CVector::setZero(&pfIn[1], static_cast<long long>(iLenBuff) - 2);
        CHECK(.5F == Approx(CFeatureFromBlockIf::compFeatureSpectralCrestFactor(pfIn, iLenBuff, fSampleRate)).margin(1e-6F).epsilon(1e-6F));
    }

    SECTION("SpectralDecrease")
    {
        // zero test
        CHECK(0.F == Approx(CFeatureFromBlockIf::compFeatureSpectralDecrease(pfIn, iLenBuff)).margin(1e-6F).epsilon(1e-6F));

        // increasing spectrum
        for (auto k = 0; k < iLenBuff; k++)
            pfIn[k] = static_cast<float>(k);

        CHECK((iLenBuff - 1) / CVector::getSum(pfIn, iLenBuff) == Approx(CFeatureFromBlockIf::compFeatureSpectralDecrease(pfIn, iLenBuff)).margin(1e-6F).epsilon(1e-6F));
    }

    SECTION("SpectralFlatness")
    {
        // zero test
        CHECK(0.F == Approx(CFeatureFromBlockIf::compFeatureSpectralFlatness(pfIn, iLenBuff)).margin(1e-6F).epsilon(1e-6F));

        // 'sine' test
        pfIn[10] = 1;
        fSampleRate = 2048;
        CHECK(0.F == Approx(CFeatureFromBlockIf::compFeatureSpectralFlatness(pfIn, iLenBuff, fSampleRate)).margin(1e-6F).epsilon(1e-6F));
        fSampleRate = 32;
        CHECK(0.F == Approx(CFeatureFromBlockIf::compFeatureSpectralFlatness(pfIn, 17, fSampleRate)).margin(1e-6F).epsilon(1e-6F));

        // 'noise' test
        CVector::setValue(pfIn, 1.F, iLenBuff);
        CHECK(1.F == CFeatureFromBlockIf::compFeatureSpectralFlatness(pfIn, iLenBuff));

        // one value zero
        pfIn[10] = 0;
        CHECK(0.F == Approx(CFeatureFromBlockIf::compFeatureSpectralFlatness(pfIn, 20, fSampleRate)).margin(1e-6F).epsilon(1e-6F));
        // 'sine w noise' test
        CVector::setValue(pfIn, 1.F, iLenBuff);
        pfIn[2] = 10;
        CHECK(std::sqrt(std::sqrt(10)) / (13.F / 4) == Approx(CFeatureFromBlockIf::compFeatureSpectralFlatness(pfIn, 4)).margin(1e-6F).epsilon(1e-6F));
    }

    SECTION("SpectralFlux")
    {
        int iIdx = iLenBuff / 2;
        // zero test
        CHECK(0.F == CFeatureFromBlockIf::compFeatureSpectralFlux(pfIn, &pfIn[iIdx], iLenBuff / 2));

        // 'sine' test
        pfIn[10] = 1;
        pfIn[iIdx + 10] = 1;
        fSampleRate = 2048;
        CHECK(0.F == Approx(CFeatureFromBlockIf::compFeatureSpectralFlux(pfIn, &pfIn[iIdx], iLenBuff / 2, fSampleRate)).margin(1e-6F).epsilon(1e-6F));
        fSampleRate = 32;
        CHECK(0.F == Approx(CFeatureFromBlockIf::compFeatureSpectralFlux(pfIn, &pfIn[iIdx], iLenBuff / 2, fSampleRate)).margin(1e-6F).epsilon(1e-6F));

        // 'noise' test
        CVector::setValue(pfIn, 1.F, iLenBuff);
        CHECK(0.F == Approx(CFeatureFromBlockIf::compFeatureSpectralFlux(pfIn, &pfIn[iIdx], iLenBuff / 2)).margin(1e-6F).epsilon(1e-6F));

        // one spectrum zero, the other one
        CVector::setZero(pfIn, iLenBuff / 2);
        CHECK(1.F / std::sqrt(iLenBuff / 2.F) == Approx(CFeatureFromBlockIf::compFeatureSpectralFlux(pfIn, &pfIn[iIdx], iLenBuff / 2)).margin(1e-4F).epsilon(1e-4F));
        CHECK(1.F / std::sqrt(iLenBuff / 2.F) == Approx(CFeatureFromBlockIf::compFeatureSpectralFlux(&pfIn[iIdx], pfIn, iLenBuff / 2)).margin(1e-4F).epsilon(1e-4F));

        // one spectrum zero, the other two
        CVector::mulC_I(pfIn, 2.F, iLenBuff);
        CHECK(std::sqrt(4.F * (iLenBuff / 2)) / (iLenBuff / 2) == Approx(CFeatureFromBlockIf::compFeatureSpectralFlux(pfIn, &pfIn[iIdx], iLenBuff / 2)).margin(1e-4F).epsilon(1e-4F));
        CHECK(std::sqrt(4.F * (iLenBuff / 2)) / (iLenBuff / 2) == Approx(CFeatureFromBlockIf::compFeatureSpectralFlux(&pfIn[iIdx], pfIn, iLenBuff / 2)).margin(1e-4F).epsilon(1e-4F));

        // alternating spectral bins
        iIdx = 4;
        iLenBuff = 4;
        pfIn[0] = 1;
        pfIn[2] = 1;
        pfIn[iIdx + 1] = 1;
        pfIn[iIdx + 3] = 1;
        CHECK(1.F / std::sqrt(static_cast<float>(iLenBuff)) == Approx(CFeatureFromBlockIf::compFeatureSpectralFlux(pfIn, &pfIn[iIdx], iLenBuff)).margin(1e-4F).epsilon(1e-4F));
    }

    SECTION("SpectralKurtosis")
    {
        // zero test
        CHECK(0.F == CFeatureFromBlockIf::compFeatureSpectralKurtosis(pfIn, iLenBuff));

        // 'sine' test
        pfIn[10] = 1;
        fSampleRate = 2048;
        CHECK(0.F == CFeatureFromBlockIf::compFeatureSpectralKurtosis(pfIn, iLenBuff, fSampleRate));
        fSampleRate = 32;
        CHECK(0.F == CFeatureFromBlockIf::compFeatureSpectralKurtosis(pfIn, 17, fSampleRate));

        // flat spectrum test
        fSampleRate = 1;
        CVector::setValue(pfIn, 1.F, iLenBuff);
        CHECK(-1.2F == Approx(CFeatureFromBlockIf::compFeatureSpectralKurtosis(pfIn, iLenBuff, fSampleRate)).margin(1e-4F).epsilon(1e-4F));
    }

    SECTION("SpectralRolloff")
    {
        // zero test
        CHECK(0.F == CFeatureFromBlockIf::compFeatureSpectralRolloff(pfIn, iLenBuff));

        // flat spectrum test
        fSampleRate = 200;
        CVector::setValue(pfIn, 1.F, iLenBuff);
        CHECK(85.F == Approx(CFeatureFromBlockIf::compFeatureSpectralRolloff(pfIn, 101, fSampleRate)).margin(1e-4F).epsilon(1e-4F));
        CHECK(75.F == Approx(CFeatureFromBlockIf::compFeatureSpectralRolloff(pfIn, 101, fSampleRate, .75F)).margin(1e-4F).epsilon(1e-4F));
    }

    SECTION("SpectralSkewness")
    {
        // zero test
        CHECK(0.F == CFeatureFromBlockIf::compFeatureSpectralKurtosis(pfIn, iLenBuff));

        // 'sine' test
        pfIn[512] = 1;
        fSampleRate = 2048;
        CHECK(0.F == CFeatureFromBlockIf::compFeatureSpectralSkewness(pfIn, iLenBuff, fSampleRate));
        fSampleRate = 32;
        CHECK(0.F == CFeatureFromBlockIf::compFeatureSpectralSkewness(pfIn, 17, fSampleRate));

        // flat spectrum test
        fSampleRate = 1;
        CVector::setValue(pfIn, 1.F, iLenBuff);
        CHECK(0.F == Approx(CFeatureFromBlockIf::compFeatureSpectralSkewness(pfIn, iLenBuff, fSampleRate)).margin(1e-4F).epsilon(1e-4F));

        // decreasing spectrum -> positive skewness
        for (auto k = 0; k < iLenBuff; k++)
            pfIn[k] = iLenBuff - 1.F - k;
        CHECK(0.F < CFeatureFromBlockIf::compFeatureSpectralSkewness(pfIn, iLenBuff, fSampleRate));

        // increasing spectrum -> negative skewness
        for (auto k = 0; k < iLenBuff; k++)
            pfIn[k] = static_cast<float>(k);
        CHECK_FALSE(0.F < CFeatureFromBlockIf::compFeatureSpectralSkewness(pfIn, iLenBuff, fSampleRate));

        // symmetry
        for (auto k = iLenBuff / 2; k < iLenBuff; k++)
            pfIn[k] = iLenBuff - 1.F - k;
        CHECK(CFeatureFromBlockIf::compFeatureSpectralSkewness(&pfIn[iLenBuff / 2], iLenBuff / 2, fSampleRate) == Approx(-1.F * CFeatureFromBlockIf::compFeatureSpectralSkewness(pfIn, iLenBuff / 2, fSampleRate)).margin(1e-4F).epsilon(1e-4F));
    }

    SECTION("SpectralSlope")
    {
        // zero test
        CHECK(0.F == CFeatureFromBlockIf::compFeatureSpectralSlope(pfIn, iLenBuff));

        // increasing spectrum
        for (auto k = 0; k < iLenBuff; k++)
            pfIn[k] = static_cast<float>(k);
        CHECK(1.F == Approx(CFeatureFromBlockIf::compFeatureSpectralSlope(pfIn, 5)).margin(1e-6F).epsilon(1e-6F));
        CHECK(1.F == Approx(CFeatureFromBlockIf::compFeatureSpectralSlope(pfIn, iLenBuff)).margin(1e-2F).epsilon(1e-2F));

        // flat spectrum
        CVector::setZero(pfIn, iLenBuff);
        CHECK(0.F == Approx(CFeatureFromBlockIf::compFeatureSpectralSlope(pfIn, 5)).margin(1e-6F).epsilon(1e-6F));
        CHECK(0.F == Approx(CFeatureFromBlockIf::compFeatureSpectralSlope(pfIn, iLenBuff)).margin(1e-2F).epsilon(1e-2F));
    }

    SECTION("SpectralSpread")
    {
        // zero test
        CHECK(0.F == CFeatureFromBlockIf::compFeatureSpectralSpread(pfIn, iLenBuff));

        // 'sine' test
        pfIn[10] = 1;
        fSampleRate = 2048;
        CHECK(0.F == CFeatureFromBlockIf::compFeatureSpectralSpread(pfIn, iLenBuff, fSampleRate));
        fSampleRate = 32;
        CHECK(0.F == CFeatureFromBlockIf::compFeatureSpectralSpread(pfIn, 17, fSampleRate));

        // flat spectrum test
        fSampleRate = 1;
        CVector::setZero(pfIn, iLenBuff);
        CVector::setValue(&pfIn[5], 1.F, 5);
        iLenBuff = 17;
        CHECK(std::sqrt(10.F / 5.F) / (2 * (iLenBuff - 1)) == Approx(CFeatureFromBlockIf::compFeatureSpectralSpread(pfIn, iLenBuff, fSampleRate)).margin(1e-4F).epsilon(1e-4F));
    }

    SECTION("SpectralTonalPowerRatio")
    {
        // zero test
        CHECK(0.F == CFeatureFromBlockIf::compFeatureSpectralTonalPowerRatio(pfIn, iLenBuff));

        // 'sine' test
        pfIn[10] = 1;
        fSampleRate = 2048;
        CHECK(1.F == CFeatureFromBlockIf::compFeatureSpectralTonalPowerRatio(pfIn, iLenBuff, fSampleRate));
        fSampleRate = 32;
        CHECK(1.F == CFeatureFromBlockIf::compFeatureSpectralTonalPowerRatio(pfIn, 17, fSampleRate));

        // multiple maxima 
        CVector::setValue(pfIn, .5F, iLenBuff);
        pfIn[100] = 2.F;
        pfIn[200] = 2.F;
        pfIn[300] = 2.F;
        pfIn[400] = 2.F;
        pfIn[500] = 2.F;
        CHECK(5.F * 4.F / (.25 * (iLenBuff - 5) + 5.F * 4.F) == Approx(CFeatureFromBlockIf::compFeatureSpectralTonalPowerRatio(pfIn, iLenBuff, fSampleRate)).margin(1e-4F).epsilon(1e-4F));
    }

    SECTION("TimeAcfCoeff")
    {
        int eta = 0;

        // zero test
        CHECK(0.F == CFeatureFromBlockIf::compFeatureTimeAcfCoeff(pfIn, iLenBuff));

        // dc input
        CVector::setValue(pfIn, 1.F, 20);
        for (eta = 0; eta < 19; eta++)
            CHECK(CFeatureFromBlockIf::compFeatureTimeAcfCoeff(pfIn, 20, fSampleRate, eta) > CFeatureFromBlockIf::compFeatureTimeAcfCoeff(pfIn, 20, fSampleRate, eta + 1));

        // sine wave
        eta = 500;
        fSampleRate = 1000;
        CSynthesis::genSine<float>(pfIn, 2, fSampleRate, iLenBuff, 1.F);
        CHECK(500 * (1.F - eta / 1000.F) == Approx(CFeatureFromBlockIf::compFeatureTimeAcfCoeff(pfIn, 1000, fSampleRate, eta)).margin(1e-4F).epsilon(1e-4F));
    }

    SECTION("TimePeakEnvelope")
    {
        // zero test
        CHECK(0.F == CFeatureFromBlockIf::compFeatureTimePeakEnvelope(pfIn, iLenBuff));

        // offset
        CVector::setValue(pfIn, 1.F, iLenBuff);
        CHECK(1.F == Approx(CFeatureFromBlockIf::compFeatureTimePeakEnvelope(pfIn, iLenBuff)).margin(1e-3F).epsilon(1e-3F));

        // one maximum
        pfIn[117] = 2.F;
        CHECK(2.F == Approx(CFeatureFromBlockIf::compFeatureTimePeakEnvelope(pfIn, iLenBuff)).margin(1e-3F).epsilon(1e-3F));

        // negative maximm
        CVector::addC_I(pfIn, -5.F, iLenBuff);
        CHECK(-3.F == Approx(CFeatureFromBlockIf::compFeatureTimePeakEnvelope(pfIn, iLenBuff)).margin(1e-3F).epsilon(1e-3F));

        // sine wave
        fSampleRate = 200;
        CSynthesis::genSine<float>(pfIn, 1, fSampleRate, iLenBuff, .7F);
        CHECK(.7F == Approx(CFeatureFromBlockIf::compFeatureTimePeakEnvelope(pfIn, iLenBuff)).margin(1e-6F).epsilon(1e-6F));
    }

    SECTION("TimeRms")
    {
        // zero test
        CHECK(0.F == CFeatureFromBlockIf::compFeatureTimeRms(pfIn, iLenBuff));

        // sine wave
        fSampleRate = 200;
        CSynthesis::genSine<float>(pfIn, 1, fSampleRate, iLenBuff);
        CHECK(1.F / std::sqrt(2.F) == Approx(CFeatureFromBlockIf::compFeatureTimeRms(pfIn, 1000)).margin(1e-6F).epsilon(1e-6F));

        // square wave
        fSampleRate = 200;
        CSynthesis::genRect<float>(pfIn, 1, fSampleRate, iLenBuff);
        CHECK(1.F == Approx(CFeatureFromBlockIf::compFeatureTimeRms(pfIn, 1000)).margin(1e-3F).epsilon(1e-3F));

        // square wave with offset
        CVector::addC_I(pfIn, 1.F, iLenBuff);
        CHECK(std::sqrt(2.F) == Approx(CFeatureFromBlockIf::compFeatureTimeRms(pfIn, 1000)).margin(1e-3F).epsilon(1e-3F));
    }

    SECTION("TimeStd")
    {
        // zero test
        CHECK(0.F == CFeatureFromBlockIf::compFeatureTimeStd(pfIn, iLenBuff));

        // sine wave
        fSampleRate = 200;
        CSynthesis::genSine<float>(pfIn, 1, fSampleRate, iLenBuff);
        CHECK(1.F / std::sqrt(2.F) == Approx(CFeatureFromBlockIf::compFeatureTimeStd(pfIn, 1000)).margin(1e-6F).epsilon(1e-6F));

        // square wave
        fSampleRate = 200;
        CSynthesis::genRect<float>(pfIn, 1, fSampleRate, iLenBuff);
        CHECK(1.F == Approx(CFeatureFromBlockIf::compFeatureTimeStd(pfIn, 1000)).margin(1e-3F).epsilon(1e-3F));

        // square wave with offset
        CVector::addC_I(pfIn, 1.F, iLenBuff);
        CHECK(1.F == Approx(CFeatureFromBlockIf::compFeatureTimeStd(pfIn, 1000)).margin(1e-3F).epsilon(1e-3F));
    }

    SECTION("TimeZeroCrossingRate")
    {
        // zero test
        CHECK(0.F == CFeatureFromBlockIf::compFeatureTimeZeroCrossingRate(pfIn, iLenBuff));

        // sine wave
        fSampleRate = 200;
        CSynthesis::genSine<float>(pfIn, 1, fSampleRate, iLenBuff);
        CHECK(19.F / 2000.F == Approx(CFeatureFromBlockIf::compFeatureTimeZeroCrossingRate(pfIn, 1000)).margin(1e-6F).epsilon(1e-6F));

        // square wave
        fSampleRate = 200;
        CSynthesis::genRect<float>(pfIn, 1, fSampleRate, iLenBuff);
        CHECK(19.F / 2000.F == Approx(CFeatureFromBlockIf::compFeatureTimeZeroCrossingRate(pfIn, 1000)).margin(1e-6F).epsilon(1e-6F));
    }

    delete[] pfIn;
}

TEST_CASE("Features (class interface per block)", "[FeaturesBlockClass]")
{
    CFeatureFromBlockIf* pCInstance = 0;
    float* pfIn = 0;
    float fSampleRate = 1;
    int iLenBuff = 1025;
    
    pfIn = new float[iLenBuff];
    CVector::setValue(pfIn, 1.F, iLenBuff);

    SECTION("Api")
    {
        CHECK(Error_t::kFunctionInvalidArgsError == CFeatureFromBlockIf::create(pCInstance, CFeatureIf::kFeatureSpectralCentroid, 0, fSampleRate));
        CHECK(Error_t::kFunctionInvalidArgsError == CFeatureFromBlockIf::create(pCInstance, CFeatureIf::kFeatureSpectralCentroid, -1, fSampleRate));
        CHECK(Error_t::kFunctionInvalidArgsError == CFeatureFromBlockIf::create(pCInstance, CFeatureIf::kFeatureSpectralCentroid, iLenBuff, 0));
        CHECK(Error_t::kFunctionInvalidArgsError == CFeatureFromBlockIf::create(pCInstance, CFeatureIf::kFeatureSpectralCentroid, iLenBuff, -1));

        CHECK(Error_t::kNoError == CFeatureFromBlockIf::create(pCInstance, CFeatureIf::kFeatureSpectralCentroid, iLenBuff, fSampleRate));
        CHECK(1 == pCInstance->getFeatureDimensions());
        CHECK(Error_t::kNoError == CFeatureFromBlockIf::destroy(pCInstance));
    }

    SECTION("FeatureCalc")
    {
        fSampleRate = 16000.F;
        for (auto k = 0; k < CFeatureIf::kNumFeatures; k++)
        {
            float fResult = 0;
            CHECK(Error_t::kNoError == CFeatureFromBlockIf::create(pCInstance, static_cast<CFeatureIf::Feature_t>(k), iLenBuff, fSampleRate));
            CHECK(pCInstance->getFeatureDimensions() >= 1);
            if (pCInstance->getFeatureDimensions() == 1)
                CHECK(Error_t::kNoError == pCInstance->compFeature(&fResult, pfIn));
            CHECK(Error_t::kNoError == CFeatureFromBlockIf::destroy(pCInstance));
        }
    }

    SECTION("TimeMaxAcf")
    {
        float fResult = 0;
        CHECK(Error_t::kNoError == CFeatureFromBlockIf::create(pCInstance, CFeatureIf::kFeatureTimeMaxAcf, iLenBuff, fSampleRate));

        // ones
        CHECK(Error_t::kNoError == pCInstance->compFeature(&fResult, pfIn));
        CHECK(fResult == Approx(1.F).margin(1e-6F).epsilon(1e-6F));

        // zeros
        CVector::setZero(pfIn, iLenBuff);
        CHECK(Error_t::kNoError == pCInstance->compFeature(&fResult, pfIn));
        CHECK(fResult == Approx(0.F).margin(1e-6F).epsilon(1e-6F));

        // sine wave
        int eta = 500;
        fSampleRate = 1000;
        CSynthesis::genSine<float>(pfIn, 2, fSampleRate, iLenBuff, 1.F);
        CHECK(Error_t::kNoError == pCInstance->compFeature(&fResult, pfIn));
        CHECK((1.F - eta / 1000.F) == Approx(fResult).margin(1e-3F).epsilon(1e-3F));
    }

    SECTION("SpectralPitchChroma")
    {
        float afResult[12] = { 0,0,0,0,0,0,0,0,0,0,0,0 };
        fSampleRate = 32000;
        CHECK(Error_t::kNoError == CFeatureFromBlockIf::create(pCInstance, CFeatureIf::kFeatureSpectralPitchChroma, iLenBuff, fSampleRate));
        CHECK(12 == pCInstance->getFeatureDimensions());

        // ones
        CHECK(Error_t::kNoError == pCInstance->compFeature(afResult, pfIn));
        CHECK(1.F == Approx(CVector::getSum(afResult, 12)).margin(1e-6F).epsilon(1e-6F));
        CHECK(1.F / 12.F == Approx(CVector::getMax(afResult, 12)).margin(1e-6F).epsilon(1e-6F));
        CHECK(1.F / 12.F == Approx(CVector::getMin(afResult, 12)).margin(1e-6F).epsilon(1e-6F));

        // zeros
        CVector::setZero(pfIn, iLenBuff);
        CHECK(Error_t::kNoError == pCInstance->compFeature(afResult, pfIn));
        CHECK(0.F == Approx(CVector::getSum(afResult, 12)).margin(1e-6F).epsilon(1e-6F));
        CHECK(0.F == Approx(CVector::getMin(afResult, 12)).margin(1e-6F).epsilon(1e-6F));

        // sine 
        fSampleRate = 44000;
        CHECK(Error_t::kNoError == CFeatureFromBlockIf::destroy(pCInstance));
        CHECK(Error_t::kNoError == CFeatureFromBlockIf::create(pCInstance, CFeatureIf::kFeatureSpectralPitchChroma, 1000, fSampleRate));
        pfIn[static_cast<int>(CConversion::convertFreq2Bin(440, 2000, fSampleRate))] = 1;
        CHECK(Error_t::kNoError == pCInstance->compFeature(afResult, pfIn));
        CHECK(1.F == Approx(afResult[9]).margin(1e-6F).epsilon(1e-6F));

        // sine 
        float fNorm = 0;
        int k0 = static_cast<int>(CConversion::convertFreq2Bin(440, 2000, fSampleRate));
        fSampleRate = 44000;
        CHECK(Error_t::kNoError == CFeatureFromBlockIf::destroy(pCInstance));
        CHECK(Error_t::kNoError == CFeatureFromBlockIf::create(pCInstance, CFeatureIf::kFeatureSpectralPitchChroma, 1000, fSampleRate));
        for (auto k = 0; k < 6; k++)
        {
            pfIn[(k + 1) * k0] = 1.F / std::sqrt(k + 1.F);
            fNorm += 1.F / std::sqrt(k + 1.F);
        }
        CHECK(Error_t::kNoError == pCInstance->compFeature(afResult, pfIn));
        CHECK(afResult[9] == Approx(CVector::getMax(afResult, 12)).margin(1e-6F).epsilon(1e-6F));
        CHECK(afResult[1] > 0);
        CHECK(afResult[4] > 0);
        CHECK(1.F == Approx(CVector::getSum(afResult, 12)).margin(1e-6F).epsilon(1e-6F));
    }

    SECTION("SpectralMfccs")
    {
        float afResult[13] = { 0,0,0,0,0,0,0,0,0,0,0,0,0 };
        fSampleRate = 32000;
        CHECK(Error_t::kNoError == CFeatureFromBlockIf::create(pCInstance, CFeatureIf::kFeatureSpectralMfccs, iLenBuff, fSampleRate));
        CHECK(13 == pCInstance->getFeatureDimensions());

        // ones
        CHECK(Error_t::kNoError == pCInstance->compFeature(afResult, pfIn));
        CHECK(0.F == Approx(CVector::getSum(&afResult[1], 12)).margin(1e-2F).epsilon(1e-2F));
        CHECK(afResult[0] == Approx(CVector::getMin(afResult, 13)).margin(1e-6F).epsilon(1e-6F));
        CHECK(-7.55021658F == Approx(afResult[0]).margin(1e-6F).epsilon(1e-6F));

        // zeros
        CVector::setZero(pfIn, iLenBuff);
        CHECK(Error_t::kNoError == pCInstance->compFeature(afResult, pfIn));
        CHECK(0.F == Approx(CVector::getMean(&afResult[1], 12)).margin(1e-6F).epsilon(1e-6F));
        CHECK(afResult[0] == Approx(CVector::getMin(afResult, 13)).margin(1e-6F).epsilon(1e-6F));
        CHECK(afResult[0] < -100);
    }

    SECTION("TimeRms")
    {
        float afResult[2] = { 0,0 };
        fSampleRate = 32000;
        CHECK(Error_t::kNoError == CFeatureFromBlockIf::create(pCInstance, CFeatureIf::kFeatureTimeRms, iLenBuff, fSampleRate));
        CHECK(2 == pCInstance->getFeatureDimensions());

        // zeros
        CVector::setZero(pfIn, iLenBuff);
        CHECK(Error_t::kNoError == pCInstance->compFeature(afResult, pfIn));
        CHECK(0.F == Approx(afResult[1]).margin(1e-6F).epsilon(1e-6F));

        // ones
        float fTmp = 0;
        CVector::setValue(pfIn, 1.F, iLenBuff);
        for (auto n = 0; n < 2000; n++)
        {
            CHECK(Error_t::kNoError == pCInstance->compFeature(afResult, pfIn));
            CHECK(fTmp <= afResult[1]);
            fTmp = afResult[1];
        }
        CHECK(1.F == Approx(afResult[1]).margin(1e-4F).epsilon(1e-4F));

        // zeros
        CVector::setZero(pfIn, iLenBuff);
        CHECK(Error_t::kNoError == pCInstance->compFeature(afResult, pfIn));
        float fAlpha = CSinglePoleLp::calcFilterParam(.3F, fSampleRate);
        CHECK(std::sqrt(fAlpha) == Approx(afResult[1]).margin(1e-4F).epsilon(1e-4F));
        CHECK(Error_t::kNoError == pCInstance->compFeature(afResult, pfIn));
        CHECK(std::pow(fAlpha, iLenBuff / 2.) == Approx(afResult[1]).margin(1e-3F).epsilon(1e-3F));
    }

    SECTION("TimePeakEnvelope")
    {
        float afResult[2] = { 0,0 };
        fSampleRate = 32000;
        CHECK(Error_t::kNoError == CFeatureFromBlockIf::create(pCInstance, CFeatureIf::kFeatureTimePeakEnvelope, iLenBuff, fSampleRate));
        CHECK(2 == pCInstance->getFeatureDimensions());

        // zeros
        CVector::setZero(pfIn, iLenBuff);
        CHECK(Error_t::kNoError == pCInstance->compFeature(afResult, pfIn));
        CHECK(0.F == Approx(afResult[1]).margin(1e-6F).epsilon(1e-6F));

        // ones
        float fTmp = 0;
        CVector::setValue(pfIn, 1.F, iLenBuff);
        for (auto n = 0; n < 2000; n++)
        {
            CHECK(Error_t::kNoError == pCInstance->compFeature(afResult, pfIn));
            CHECK(fTmp <= afResult[1]);
            fTmp = afResult[1];
        }
        CHECK(1.F == Approx(afResult[1]).margin(1e-4F).epsilon(1e-4F));

        // zeros
        CVector::setZero(pfIn, iLenBuff);
        CHECK(Error_t::kNoError == pCInstance->compFeature(afResult, pfIn));
        float fAlpha = CSinglePoleLp::calcFilterParam(1.5F, fSampleRate);
        CHECK(fAlpha == Approx(afResult[1]).margin(1e-4F).epsilon(1e-4F));
        CHECK(Error_t::kNoError == pCInstance->compFeature(afResult, pfIn));
        CHECK(std::pow(fAlpha, iLenBuff) == Approx(afResult[1]).margin(1e-3F).epsilon(1e-3F));
    }

    CFeatureFromBlockIf::destroy(pCInstance);

    delete[] pfIn;
}

TEST_CASE("Features (per array)", "[FeaturesClass]")
{

    CFeatureIf* pCInstance = 0;
    float* pfIn = 0;
    float fSampleRate = 44100;
    int iBlockLength = 1023,
        iHopLength = 512,
        iLenBuff = 96000;
    
    pfIn = new float[iLenBuff];
    CVector::setValue(pfIn, 1.F, iLenBuff);


    SECTION("Api")
    {
        for (auto f = 0; f < CFeatureIf::kNumFeatures; f++)
        {
            int aiDim[2] = { 0,0 };
            float** ppfFeature = 0;
            CHECK(Error_t::kFunctionInvalidArgsError == CFeatureIf::create(pCInstance, static_cast<CFeatureIf::Feature_t>(f), 0, iLenBuff, fSampleRate, iBlockLength, iHopLength));
            CHECK(Error_t::kFunctionInvalidArgsError == CFeatureIf::create(pCInstance, static_cast<CFeatureIf::Feature_t>(f), pfIn, 0, fSampleRate, iBlockLength, iHopLength));
            CHECK(Error_t::kFunctionInvalidArgsError == CFeatureIf::create(pCInstance, static_cast<CFeatureIf::Feature_t>(f), pfIn, -1, fSampleRate, iBlockLength, iHopLength));
            CHECK(Error_t::kFunctionInvalidArgsError == CFeatureIf::create(pCInstance, static_cast<CFeatureIf::Feature_t>(f), pfIn, iLenBuff, 0, iBlockLength, iHopLength));
            CHECK(Error_t::kFunctionInvalidArgsError == CFeatureIf::create(pCInstance, static_cast<CFeatureIf::Feature_t>(f), pfIn, iLenBuff, fSampleRate, 0, iHopLength));
            CHECK(Error_t::kFunctionInvalidArgsError == CFeatureIf::create(pCInstance, static_cast<CFeatureIf::Feature_t>(f), pfIn, iLenBuff, fSampleRate, iBlockLength, 0));

            CHECK(Error_t::kNoError == CFeatureIf::create(pCInstance, static_cast<CFeatureIf::Feature_t>(f), pfIn, iLenBuff, fSampleRate, iBlockLength, iHopLength));

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

    delete[] pfIn;
}

#endif //WITH_TESTS
