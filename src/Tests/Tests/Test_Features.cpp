#include "ACAConfig.h"

#ifdef WITH_TESTS

#include "Vector.h"
#include "Synthesis.h"
#include "ToolConversion.h"
#include "ToolLowPass.h"

#include "FeatureFromBlock.h"
#include "catch.hpp"

TEST_CASE("Features (static functions)", "[FeaturesStatic]")
{

    float* m_pfInput = 0;
    float m_fSampleRate = 1;
    int m_iBufferLength = 1025;

    m_pfInput = new float[m_iBufferLength];
    CVectorFloat::setZero(m_pfInput, m_iBufferLength);

    SECTION("SpectralCentroid")
    {
        // zero test
        CHECK(0.F == CFeatureFromBlockIf::compFeatureSpectralCentroid(m_pfInput, m_iBufferLength));

        // 'sine' test
        m_pfInput[10] = 1;
        m_fSampleRate = 2048;
        CHECK(10.F == Approx(CFeatureFromBlockIf::compFeatureSpectralCentroid(m_pfInput, m_iBufferLength, m_fSampleRate)).margin(1e-6F).epsilon(1e-6F));
        m_fSampleRate = 32;
        CHECK(10.F == Approx(CFeatureFromBlockIf::compFeatureSpectralCentroid(m_pfInput, 17, m_fSampleRate)).margin(1e-6F).epsilon(1e-6F));

        // 'noise' test
        CVectorFloat::setValue(m_pfInput, 1, m_iBufferLength);
        CHECK(m_fSampleRate / 4.F == Approx(CFeatureFromBlockIf::compFeatureSpectralCentroid(m_pfInput, m_iBufferLength, m_fSampleRate)).margin(1e-6F).epsilon(1e-6F));

        // symmetric spectrum test
        CVectorFloat::setZero(&m_pfInput[1], m_iBufferLength - 2);
        CHECK(m_fSampleRate / 4.F == Approx(CFeatureFromBlockIf::compFeatureSpectralCentroid(m_pfInput, m_iBufferLength, m_fSampleRate)).margin(1e-6F).epsilon(1e-6F));
    }

    SECTION("SpectralCrestFactor")
    {
        // zero test
        CHECK(0.F == CFeatureFromBlockIf::compFeatureSpectralCrestFactor(m_pfInput, m_iBufferLength));

        // 'sine' test
        m_pfInput[10] = 1;
        m_fSampleRate = 2048;
        CHECK(1.F == CFeatureFromBlockIf::compFeatureSpectralCrestFactor(m_pfInput, m_iBufferLength, m_fSampleRate));
        m_fSampleRate = 32;
        CHECK(1.F == CFeatureFromBlockIf::compFeatureSpectralCrestFactor(m_pfInput, 17, m_fSampleRate));

        // 'noise' test
        CVectorFloat::setValue(m_pfInput, 1, m_iBufferLength);
        CHECK(1.F / m_iBufferLength == Approx(CFeatureFromBlockIf::compFeatureSpectralCrestFactor(m_pfInput, m_iBufferLength)).margin(1e-6F).epsilon(1e-6F));

        // symmetric spectrum test
        CVectorFloat::setZero(&m_pfInput[1], m_iBufferLength - 2);
        CHECK(.5F == Approx(CFeatureFromBlockIf::compFeatureSpectralCrestFactor(m_pfInput, m_iBufferLength, m_fSampleRate)).margin(1e-6F).epsilon(1e-6F));
    }

    SECTION("SpectralDecrease")
    {
        // zero test
        CHECK(0.F == Approx(CFeatureFromBlockIf::compFeatureSpectralDecrease(m_pfInput, m_iBufferLength)).margin(1e-6F).epsilon(1e-6F));

        // increasing spectrum
        for (auto k = 0; k < m_iBufferLength; k++)
            m_pfInput[k] = static_cast<float>(k);

        CHECK((m_iBufferLength - 1) / CVectorFloat::getSum(m_pfInput, m_iBufferLength) == Approx(CFeatureFromBlockIf::compFeatureSpectralDecrease(m_pfInput, m_iBufferLength)).margin(1e-6F).epsilon(1e-6F));
    }

    SECTION("SpectralFlatness")
    {
        // zero test
        CHECK(0.F == Approx(CFeatureFromBlockIf::compFeatureSpectralFlatness(m_pfInput, m_iBufferLength)).margin(1e-6F).epsilon(1e-6F));

        // 'sine' test
        m_pfInput[10] = 1;
        m_fSampleRate = 2048;
        CHECK(0.F == Approx(CFeatureFromBlockIf::compFeatureSpectralFlatness(m_pfInput, m_iBufferLength, m_fSampleRate)).margin(1e-6F).epsilon(1e-6F));
        m_fSampleRate = 32;
        CHECK(0.F == Approx(CFeatureFromBlockIf::compFeatureSpectralFlatness(m_pfInput, 17, m_fSampleRate)).margin(1e-6F).epsilon(1e-6F));

        // 'noise' test
        CVectorFloat::setValue(m_pfInput, 1, m_iBufferLength);
        CHECK(1.F == CFeatureFromBlockIf::compFeatureSpectralFlatness(m_pfInput, m_iBufferLength));

        // one value zero
        m_pfInput[10] = 0;
        CHECK(0.F == Approx(CFeatureFromBlockIf::compFeatureSpectralFlatness(m_pfInput, 20, m_fSampleRate)).margin(1e-6F).epsilon(1e-6F));
        // 'sine w noise' test
        CVectorFloat::setValue(m_pfInput, 1, m_iBufferLength);
        m_pfInput[2] = 10;
        CHECK(std::sqrt(std::sqrt(10)) / (13.F / 4) == Approx(CFeatureFromBlockIf::compFeatureSpectralFlatness(m_pfInput, 4)).margin(1e-6F).epsilon(1e-6F));
    }

    SECTION("SpectralFlux")
    {
        int iIdx = m_iBufferLength / 2;
        // zero test
        CHECK(0.F == CFeatureFromBlockIf::compFeatureSpectralFlux(m_pfInput, &m_pfInput[iIdx], m_iBufferLength / 2));

        // 'sine' test
        m_pfInput[10] = 1;
        m_pfInput[iIdx + 10] = 1;
        m_fSampleRate = 2048;
        CHECK(0.F == Approx(CFeatureFromBlockIf::compFeatureSpectralFlux(m_pfInput, &m_pfInput[iIdx], m_iBufferLength / 2, m_fSampleRate)).margin(1e-6F).epsilon(1e-6F));
        m_fSampleRate = 32;
        CHECK(0.F == Approx(CFeatureFromBlockIf::compFeatureSpectralFlux(m_pfInput, &m_pfInput[iIdx], m_iBufferLength / 2, m_fSampleRate)).margin(1e-6F).epsilon(1e-6F));

        // 'noise' test
        CVectorFloat::setValue(m_pfInput, 1, m_iBufferLength);
        CHECK(0.F == Approx(CFeatureFromBlockIf::compFeatureSpectralFlux(m_pfInput, &m_pfInput[iIdx], m_iBufferLength / 2)).margin(1e-6F).epsilon(1e-6F));

        // one spectrum zero, the other one
        CVectorFloat::setZero(m_pfInput, m_iBufferLength / 2);
        CHECK(1.F / std::sqrt(m_iBufferLength / 2.F) == Approx(CFeatureFromBlockIf::compFeatureSpectralFlux(m_pfInput, &m_pfInput[iIdx], m_iBufferLength / 2)).margin(1e-4F).epsilon(1e-4F));
        CHECK(1.F / std::sqrt(m_iBufferLength / 2.F) == Approx(CFeatureFromBlockIf::compFeatureSpectralFlux(&m_pfInput[iIdx], m_pfInput, m_iBufferLength / 2)).margin(1e-4F).epsilon(1e-4F));

        // one spectrum zero, the other two
        CVectorFloat::mulC_I(m_pfInput, 2.F, m_iBufferLength);
        CHECK(std::sqrt(4.F * (m_iBufferLength / 2)) / (m_iBufferLength / 2) == Approx(CFeatureFromBlockIf::compFeatureSpectralFlux(m_pfInput, &m_pfInput[iIdx], m_iBufferLength / 2)).margin(1e-4F).epsilon(1e-4F));
        CHECK(std::sqrt(4.F * (m_iBufferLength / 2)) / (m_iBufferLength / 2) == Approx(CFeatureFromBlockIf::compFeatureSpectralFlux(&m_pfInput[iIdx], m_pfInput, m_iBufferLength / 2)).margin(1e-4F).epsilon(1e-4F));

        // alternating spectral bins
        iIdx = 4;
        m_iBufferLength = 4;
        m_pfInput[0] = 1;
        m_pfInput[2] = 1;
        m_pfInput[iIdx + 1] = 1;
        m_pfInput[iIdx + 3] = 1;
        CHECK(1.F / std::sqrt(static_cast<float>(m_iBufferLength)) == Approx(CFeatureFromBlockIf::compFeatureSpectralFlux(m_pfInput, &m_pfInput[iIdx], m_iBufferLength)).margin(1e-4F).epsilon(1e-4F));
    }

    SECTION("SpectralKurtosis")
    {
        // zero test
        CHECK(0.F == CFeatureFromBlockIf::compFeatureSpectralKurtosis(m_pfInput, m_iBufferLength));

        // 'sine' test
        m_pfInput[10] = 1;
        m_fSampleRate = 2048;
        CHECK(0.F == CFeatureFromBlockIf::compFeatureSpectralKurtosis(m_pfInput, m_iBufferLength, m_fSampleRate));
        m_fSampleRate = 32;
        CHECK(0.F == CFeatureFromBlockIf::compFeatureSpectralKurtosis(m_pfInput, 17, m_fSampleRate));

        // flat spectrum test
        m_fSampleRate = 1;
        CVectorFloat::setValue(m_pfInput, 1.F, m_iBufferLength);
        CHECK(-1.2F == Approx(CFeatureFromBlockIf::compFeatureSpectralKurtosis(m_pfInput, m_iBufferLength, m_fSampleRate)).margin(1e-4F).epsilon(1e-4F));
    }

    SECTION("SpectralRolloff")
    {
        // zero test
        CHECK(0.F == CFeatureFromBlockIf::compFeatureSpectralRolloff(m_pfInput, m_iBufferLength));

        // flat spectrum test
        m_fSampleRate = 200;
        CVectorFloat::setValue(m_pfInput, 1.F, m_iBufferLength);
        CHECK(85.F == Approx(CFeatureFromBlockIf::compFeatureSpectralRolloff(m_pfInput, 101, m_fSampleRate)).margin(1e-4F).epsilon(1e-4F));
        CHECK(75.F == Approx(CFeatureFromBlockIf::compFeatureSpectralRolloff(m_pfInput, 101, m_fSampleRate, .75F)).margin(1e-4F).epsilon(1e-4F));
    }

    SECTION("SpectralSkewness")
    {
        // zero test
        CHECK(0.F == CFeatureFromBlockIf::compFeatureSpectralKurtosis(m_pfInput, m_iBufferLength));

        // 'sine' test
        m_pfInput[512] = 1;
        m_fSampleRate = 2048;
        CHECK(0.F == CFeatureFromBlockIf::compFeatureSpectralSkewness(m_pfInput, m_iBufferLength, m_fSampleRate));
        m_fSampleRate = 32;
        CHECK(0.F == CFeatureFromBlockIf::compFeatureSpectralSkewness(m_pfInput, 17, m_fSampleRate));

        // flat spectrum test
        m_fSampleRate = 1;
        CVectorFloat::setValue(m_pfInput, 1.F, m_iBufferLength);
        CHECK(0.F == Approx(CFeatureFromBlockIf::compFeatureSpectralSkewness(m_pfInput, m_iBufferLength, m_fSampleRate)).margin(1e-4F).epsilon(1e-4F));

        // decreasing spectrum -> positive skewness
        for (auto k = 0; k < m_iBufferLength; k++)
            m_pfInput[k] = m_iBufferLength - 1.F - k;
        CHECK(0.F < CFeatureFromBlockIf::compFeatureSpectralSkewness(m_pfInput, m_iBufferLength, m_fSampleRate));

        // increasing spectrum -> negative skewness
        for (auto k = 0; k < m_iBufferLength; k++)
            m_pfInput[k] = static_cast<float>(k);
        CHECK_FALSE(0.F < CFeatureFromBlockIf::compFeatureSpectralSkewness(m_pfInput, m_iBufferLength, m_fSampleRate));

        // symmetry
        for (auto k = m_iBufferLength / 2; k < m_iBufferLength; k++)
            m_pfInput[k] = m_iBufferLength - 1.F - k;
        CHECK(CFeatureFromBlockIf::compFeatureSpectralSkewness(&m_pfInput[m_iBufferLength / 2], m_iBufferLength / 2, m_fSampleRate) == Approx(-1.F * CFeatureFromBlockIf::compFeatureSpectralSkewness(m_pfInput, m_iBufferLength / 2, m_fSampleRate)).margin(1e-4F).epsilon(1e-4F));
    }

    SECTION("SpectralSlope")
    {
        // zero test
        CHECK(0.F == CFeatureFromBlockIf::compFeatureSpectralSlope(m_pfInput, m_iBufferLength));

        // increasing spectrum
        for (auto k = 0; k < m_iBufferLength; k++)
            m_pfInput[k] = static_cast<float>(k);
        CHECK(1.F == Approx(CFeatureFromBlockIf::compFeatureSpectralSlope(m_pfInput, 5)).margin(1e-6F).epsilon(1e-6F));
        CHECK(1.F == Approx(CFeatureFromBlockIf::compFeatureSpectralSlope(m_pfInput, m_iBufferLength)).margin(1e-2F).epsilon(1e-2F));

        // flat spectrum
        CVectorFloat::setZero(m_pfInput, m_iBufferLength);
        CHECK(0.F == Approx(CFeatureFromBlockIf::compFeatureSpectralSlope(m_pfInput, 5)).margin(1e-6F).epsilon(1e-6F));
        CHECK(0.F == Approx(CFeatureFromBlockIf::compFeatureSpectralSlope(m_pfInput, m_iBufferLength)).margin(1e-2F).epsilon(1e-2F));
    }

    SECTION("SpectralSpread")
    {
        // zero test
        CHECK(0.F == CFeatureFromBlockIf::compFeatureSpectralSpread(m_pfInput, m_iBufferLength));

        // 'sine' test
        m_pfInput[10] = 1;
        m_fSampleRate = 2048;
        CHECK(0.F == CFeatureFromBlockIf::compFeatureSpectralSpread(m_pfInput, m_iBufferLength, m_fSampleRate));
        m_fSampleRate = 32;
        CHECK(0.F == CFeatureFromBlockIf::compFeatureSpectralSpread(m_pfInput, 17, m_fSampleRate));

        // flat spectrum test
        m_fSampleRate = 1;
        CVectorFloat::setZero(m_pfInput, m_iBufferLength);
        CVectorFloat::setValue(&m_pfInput[5], 1.F, 5);
        m_iBufferLength = 17;
        CHECK(std::sqrt(10.F / 5.F) / (2 * (m_iBufferLength - 1)) == Approx(CFeatureFromBlockIf::compFeatureSpectralSpread(m_pfInput, m_iBufferLength, m_fSampleRate)).margin(1e-4F).epsilon(1e-4F));
    }

    SECTION("SpectralTonalPowerRatio")
    {
        // zero test
        CHECK(0.F == CFeatureFromBlockIf::compFeatureSpectralTonalPowerRatio(m_pfInput, m_iBufferLength));

        // 'sine' test
        m_pfInput[10] = 1;
        m_fSampleRate = 2048;
        CHECK(1.F == CFeatureFromBlockIf::compFeatureSpectralTonalPowerRatio(m_pfInput, m_iBufferLength, m_fSampleRate));
        m_fSampleRate = 32;
        CHECK(1.F == CFeatureFromBlockIf::compFeatureSpectralTonalPowerRatio(m_pfInput, 17, m_fSampleRate));

        // multiple maxima 
        CVectorFloat::setValue(m_pfInput, .5F, m_iBufferLength);
        m_pfInput[100] = 2.F;
        m_pfInput[200] = 2.F;
        m_pfInput[300] = 2.F;
        m_pfInput[400] = 2.F;
        m_pfInput[500] = 2.F;
        CHECK(5.F * 4.F / (.25 * (m_iBufferLength - 5) + 5.F * 4.F) == Approx(CFeatureFromBlockIf::compFeatureSpectralTonalPowerRatio(m_pfInput, m_iBufferLength, m_fSampleRate)).margin(1e-4F).epsilon(1e-4F));
    }

    SECTION("TimeAcfCoeff")
    {
        int eta = 0;

        // zero test
        CHECK(0.F == CFeatureFromBlockIf::compFeatureTimeAcfCoeff(m_pfInput, m_iBufferLength));

        // dc input
        CVectorFloat::setValue(m_pfInput, 1.F, 20);
        for (eta = 0; eta < 19; eta++)
            CHECK(CFeatureFromBlockIf::compFeatureTimeAcfCoeff(m_pfInput, 20, m_fSampleRate, eta) > CFeatureFromBlockIf::compFeatureTimeAcfCoeff(m_pfInput, 20, m_fSampleRate, eta + 1));

        // sine wave
        eta = 500;
        m_fSampleRate = 1000;
        CSynthesis::genSine<float>(m_pfInput, 2, m_fSampleRate, m_iBufferLength, 1.F);
        CHECK(500 * (1.F - eta / 1000.F) == Approx(CFeatureFromBlockIf::compFeatureTimeAcfCoeff(m_pfInput, 1000, m_fSampleRate, eta)).margin(1e-4F).epsilon(1e-4F));
    }

    SECTION("TimePeakEnvelope")
    {
        // zero test
        CHECK(0.F == CFeatureFromBlockIf::compFeatureTimePeakEnvelope(m_pfInput, m_iBufferLength));

        // offset
        CVectorFloat::setValue(m_pfInput, 1.F, m_iBufferLength);
        CHECK(1.F == Approx(CFeatureFromBlockIf::compFeatureTimePeakEnvelope(m_pfInput, m_iBufferLength)).margin(1e-3F).epsilon(1e-3F));

        // one maximum
        m_pfInput[117] = 2.F;
        CHECK(2.F == Approx(CFeatureFromBlockIf::compFeatureTimePeakEnvelope(m_pfInput, m_iBufferLength)).margin(1e-3F).epsilon(1e-3F));

        // negative maximm
        CVectorFloat::addC_I(m_pfInput, -5.F, m_iBufferLength);
        CHECK(-3.F == Approx(CFeatureFromBlockIf::compFeatureTimePeakEnvelope(m_pfInput, m_iBufferLength)).margin(1e-3F).epsilon(1e-3F));

        // sine wave
        m_fSampleRate = 200;
        CSynthesis::genSine<float>(m_pfInput, 1, m_fSampleRate, m_iBufferLength, .7F);
        CHECK(.7F == Approx(CFeatureFromBlockIf::compFeatureTimePeakEnvelope(m_pfInput, m_iBufferLength)).margin(1e-6F).epsilon(1e-6F));
    }

    SECTION("TimeRms")
    {
        // zero test
        CHECK(0.F == CFeatureFromBlockIf::compFeatureTimeRms(m_pfInput, m_iBufferLength));

        // sine wave
        m_fSampleRate = 200;
        CSynthesis::genSine<float>(m_pfInput, 1, m_fSampleRate, m_iBufferLength);
        CHECK(1.F / std::sqrt(2.F) == Approx(CFeatureFromBlockIf::compFeatureTimeRms(m_pfInput, 1000)).margin(1e-6F).epsilon(1e-6F));

        // square wave
        m_fSampleRate = 200;
        CSynthesis::genRect<float>(m_pfInput, 1, m_fSampleRate, m_iBufferLength);
        CHECK(1.F == Approx(CFeatureFromBlockIf::compFeatureTimeRms(m_pfInput, 1000)).margin(1e-3F).epsilon(1e-3F));

        // square wave with offset
        CVectorFloat::addC_I(m_pfInput, 1, m_iBufferLength);
        CHECK(std::sqrt(2.F) == Approx(CFeatureFromBlockIf::compFeatureTimeRms(m_pfInput, 1000)).margin(1e-3F).epsilon(1e-3F));
    }

    SECTION("TimeStd")
    {
        // zero test
        CHECK(0.F == CFeatureFromBlockIf::compFeatureTimeStd(m_pfInput, m_iBufferLength));

        // sine wave
        m_fSampleRate = 200;
        CSynthesis::genSine<float>(m_pfInput, 1, m_fSampleRate, m_iBufferLength);
        CHECK(1.F / std::sqrt(2.F) == Approx(CFeatureFromBlockIf::compFeatureTimeStd(m_pfInput, 1000)).margin(1e-6F).epsilon(1e-6F));

        // square wave
        m_fSampleRate = 200;
        CSynthesis::genRect<float>(m_pfInput, 1, m_fSampleRate, m_iBufferLength);
        CHECK(1.F == Approx(CFeatureFromBlockIf::compFeatureTimeStd(m_pfInput, 1000)).margin(1e-3F).epsilon(1e-3F));

        // square wave with offset
        CVectorFloat::addC_I(m_pfInput, 1, m_iBufferLength);
        CHECK(1.F == Approx(CFeatureFromBlockIf::compFeatureTimeStd(m_pfInput, 1000)).margin(1e-3F).epsilon(1e-3F));
    }

    SECTION("TimeZeroCrossingRate")
    {
        // zero test
        CHECK(0.F == CFeatureFromBlockIf::compFeatureTimeZeroCrossingRate(m_pfInput, m_iBufferLength));

        // sine wave
        m_fSampleRate = 200;
        CSynthesis::genSine<float>(m_pfInput, 1, m_fSampleRate, m_iBufferLength);
        CHECK(19.F / 2000.F == Approx(CFeatureFromBlockIf::compFeatureTimeZeroCrossingRate(m_pfInput, 1000)).margin(1e-6F).epsilon(1e-6F));

        // square wave
        m_fSampleRate = 200;
        CSynthesis::genRect<float>(m_pfInput, 1, m_fSampleRate, m_iBufferLength);
        CHECK(19.F / 2000.F == Approx(CFeatureFromBlockIf::compFeatureTimeZeroCrossingRate(m_pfInput, 1000)).margin(1e-6F).epsilon(1e-6F));
    }

    delete[] m_pfInput;
}

TEST_CASE("Features (class interface per block)", "[FeaturesBlockClass]")
{
    CFeatureFromBlockIf* m_pCInstance = 0;
    float* m_pfInput = 0;
    float m_fSampleRate = 1;
    int m_iBufferLength = 1025;
    
    m_pfInput = new float[m_iBufferLength];
    CVectorFloat::setValue(m_pfInput, 1.F, m_iBufferLength);

    SECTION("Api")
    {
        CHECK(Error_t::kFunctionInvalidArgsError == CFeatureFromBlockIf::create(m_pCInstance, CFeatureIf::kFeatureSpectralCentroid, 0, m_fSampleRate));
        CHECK(Error_t::kFunctionInvalidArgsError == CFeatureFromBlockIf::create(m_pCInstance, CFeatureIf::kFeatureSpectralCentroid, -1, m_fSampleRate));
        CHECK(Error_t::kFunctionInvalidArgsError == CFeatureFromBlockIf::create(m_pCInstance, CFeatureIf::kFeatureSpectralCentroid, m_iBufferLength, 0));
        CHECK(Error_t::kFunctionInvalidArgsError == CFeatureFromBlockIf::create(m_pCInstance, CFeatureIf::kFeatureSpectralCentroid, m_iBufferLength, -1));

        CHECK(Error_t::kNoError == CFeatureFromBlockIf::create(m_pCInstance, CFeatureIf::kFeatureSpectralCentroid, m_iBufferLength, m_fSampleRate));
        CHECK(1 == m_pCInstance->getFeatureDimensions());
        CHECK(Error_t::kNoError == CFeatureFromBlockIf::destroy(m_pCInstance));
    }

    SECTION("FeatureCalc")
    {
        m_fSampleRate = 16000.F;
        for (auto k = 0; k < CFeatureIf::kNumFeatures; k++)
        {
            float fResult = 0;
            CHECK(Error_t::kNoError == CFeatureFromBlockIf::create(m_pCInstance, static_cast<CFeatureIf::Feature_t>(k), m_iBufferLength, m_fSampleRate));
            CHECK(m_pCInstance->getFeatureDimensions() >= 1);
            if (m_pCInstance->getFeatureDimensions() == 1)
                CHECK(Error_t::kNoError == m_pCInstance->compFeature(&fResult, m_pfInput));
            CHECK(Error_t::kNoError == CFeatureFromBlockIf::destroy(m_pCInstance));
        }
    }

    SECTION("TimeMaxAcf")
    {
        float fResult = 0;
        CHECK(Error_t::kNoError == CFeatureFromBlockIf::create(m_pCInstance, CFeatureIf::kFeatureTimeMaxAcf, m_iBufferLength, m_fSampleRate));

        // ones
        CHECK(Error_t::kNoError == m_pCInstance->compFeature(&fResult, m_pfInput));
        CHECK(fResult == Approx(1.F).margin(1e-6F).epsilon(1e-6F));

        // zeros
        CVectorFloat::setZero(m_pfInput, m_iBufferLength);
        CHECK(Error_t::kNoError == m_pCInstance->compFeature(&fResult, m_pfInput));
        CHECK(fResult == Approx(0.F).margin(1e-6F).epsilon(1e-6F));

        // sine wave
        int eta = 500;
        m_fSampleRate = 1000;
        CSynthesis::genSine<float>(m_pfInput, 2, m_fSampleRate, m_iBufferLength, 1.F);
        CHECK(Error_t::kNoError == m_pCInstance->compFeature(&fResult, m_pfInput));
        CHECK((1.F - eta / 1000.F) == Approx(fResult).margin(1e-3F).epsilon(1e-3F));
    }

    SECTION("SpectralPitchChroma")
    {
        float afResult[12] = { 0,0,0,0,0,0,0,0,0,0,0,0 };
        m_fSampleRate = 32000;
        CHECK(Error_t::kNoError == CFeatureFromBlockIf::create(m_pCInstance, CFeatureIf::kFeatureSpectralPitchChroma, m_iBufferLength, m_fSampleRate));
        CHECK(12 == m_pCInstance->getFeatureDimensions());

        // ones
        CHECK(Error_t::kNoError == m_pCInstance->compFeature(afResult, m_pfInput));
        CHECK(1.F == Approx(CVectorFloat::getSum(afResult, 12)).margin(1e-6F).epsilon(1e-6F));
        CHECK(1.F / 12.F == Approx(CVectorFloat::getMax(afResult, 12)).margin(1e-6F).epsilon(1e-6F));
        CHECK(1.F / 12.F == Approx(CVectorFloat::getMin(afResult, 12)).margin(1e-6F).epsilon(1e-6F));

        // zeros
        CVectorFloat::setZero(m_pfInput, m_iBufferLength);
        CHECK(Error_t::kNoError == m_pCInstance->compFeature(afResult, m_pfInput));
        CHECK(0.F == Approx(CVectorFloat::getSum(afResult, 12)).margin(1e-6F).epsilon(1e-6F));
        CHECK(0.F == Approx(CVectorFloat::getMin(afResult, 12)).margin(1e-6F).epsilon(1e-6F));

        // sine 
        m_fSampleRate = 44000;
        CHECK(Error_t::kNoError == CFeatureFromBlockIf::destroy(m_pCInstance));
        CHECK(Error_t::kNoError == CFeatureFromBlockIf::create(m_pCInstance, CFeatureIf::kFeatureSpectralPitchChroma, 1000, m_fSampleRate));
        m_pfInput[static_cast<int>(CConversion::convertFreq2Bin(440, 2000, m_fSampleRate))] = 1;
        CHECK(Error_t::kNoError == m_pCInstance->compFeature(afResult, m_pfInput));
        CHECK(1.F == Approx(afResult[9]).margin(1e-6F).epsilon(1e-6F));

        // sine 
        float fNorm = 0;
        int k0 = static_cast<int>(CConversion::convertFreq2Bin(440, 2000, m_fSampleRate));
        m_fSampleRate = 44000;
        CHECK(Error_t::kNoError == CFeatureFromBlockIf::destroy(m_pCInstance));
        CHECK(Error_t::kNoError == CFeatureFromBlockIf::create(m_pCInstance, CFeatureIf::kFeatureSpectralPitchChroma, 1000, m_fSampleRate));
        for (auto k = 0; k < 6; k++)
        {
            m_pfInput[(k + 1) * k0] = 1.F / std::sqrt(k + 1.F);
            fNorm += 1.F / std::sqrt(k + 1.F);
        }
        CHECK(Error_t::kNoError == m_pCInstance->compFeature(afResult, m_pfInput));
        CHECK(afResult[9] == Approx(CVectorFloat::getMax(afResult, 12)).margin(1e-6F).epsilon(1e-6F));
        CHECK(afResult[1] > 0);
        CHECK(afResult[4] > 0);
        CHECK(1.F == Approx(CVectorFloat::getSum(afResult, 12)).margin(1e-6F).epsilon(1e-6F));
    }

    SECTION("SpectralMfccs")
    {
        float afResult[13] = { 0,0,0,0,0,0,0,0,0,0,0,0,0 };
        m_fSampleRate = 32000;
        CHECK(Error_t::kNoError == CFeatureFromBlockIf::create(m_pCInstance, CFeatureIf::kFeatureSpectralMfccs, m_iBufferLength, m_fSampleRate));
        CHECK(13 == m_pCInstance->getFeatureDimensions());

        // ones
        CHECK(Error_t::kNoError == m_pCInstance->compFeature(afResult, m_pfInput));
        CHECK(0.F == Approx(CVectorFloat::getSum(&afResult[1], 12)).margin(1e-2F).epsilon(1e-2F));
        CHECK(afResult[0] == Approx(CVectorFloat::getMin(afResult, 13)).margin(1e-6F).epsilon(1e-6F));
        CHECK(-7.55021658F == Approx(afResult[0]).margin(1e-6F).epsilon(1e-6F));

        // zeros
        CVectorFloat::setZero(m_pfInput, m_iBufferLength);
        CHECK(Error_t::kNoError == m_pCInstance->compFeature(afResult, m_pfInput));
        CHECK(0.F == Approx(CVectorFloat::getMean(&afResult[1], 12)).margin(1e-6F).epsilon(1e-6F));
        CHECK(afResult[0] == Approx(CVectorFloat::getMin(afResult, 13)).margin(1e-6F).epsilon(1e-6F));
        CHECK(afResult[0] < -100);
    }

    SECTION("TimeRms")
    {
        float afResult[2] = { 0,0 };
        m_fSampleRate = 32000;
        CHECK(Error_t::kNoError == CFeatureFromBlockIf::create(m_pCInstance, CFeatureIf::kFeatureTimeRms, m_iBufferLength, m_fSampleRate));
        CHECK(2 == m_pCInstance->getFeatureDimensions());

        // zeros
        CVectorFloat::setZero(m_pfInput, m_iBufferLength);
        CHECK(Error_t::kNoError == m_pCInstance->compFeature(afResult, m_pfInput));
        CHECK(0.F == Approx(afResult[1]).margin(1e-6F).epsilon(1e-6F));

        // ones
        float fTmp = 0;
        CVectorFloat::setValue(m_pfInput, 1.F, m_iBufferLength);
        for (auto n = 0; n < 2000; n++)
        {
            CHECK(Error_t::kNoError == m_pCInstance->compFeature(afResult, m_pfInput));
            CHECK(fTmp <= afResult[1]);
            fTmp = afResult[1];
        }
        CHECK(1.F == Approx(afResult[1]).margin(1e-4F).epsilon(1e-4F));

        // zeros
        CVectorFloat::setZero(m_pfInput, m_iBufferLength);
        CHECK(Error_t::kNoError == m_pCInstance->compFeature(afResult, m_pfInput));
        float fAlpha = CSinglePoleLp::calcFilterParam(.3F, m_fSampleRate);
        CHECK(std::sqrt(fAlpha) == Approx(afResult[1]).margin(1e-4F).epsilon(1e-4F));
        CHECK(Error_t::kNoError == m_pCInstance->compFeature(afResult, m_pfInput));
        CHECK(std::pow(fAlpha, m_iBufferLength / 2.) == Approx(afResult[1]).margin(1e-3F).epsilon(1e-3F));
    }

    SECTION("TimePeakEnvelope")
    {
        float afResult[2] = { 0,0 };
        m_fSampleRate = 32000;
        CHECK(Error_t::kNoError == CFeatureFromBlockIf::create(m_pCInstance, CFeatureIf::kFeatureTimePeakEnvelope, m_iBufferLength, m_fSampleRate));
        CHECK(2 == m_pCInstance->getFeatureDimensions());

        // zeros
        CVectorFloat::setZero(m_pfInput, m_iBufferLength);
        CHECK(Error_t::kNoError == m_pCInstance->compFeature(afResult, m_pfInput));
        CHECK(0.F == Approx(afResult[1]).margin(1e-6F).epsilon(1e-6F));

        // ones
        float fTmp = 0;
        CVectorFloat::setValue(m_pfInput, 1.F, m_iBufferLength);
        for (auto n = 0; n < 2000; n++)
        {
            CHECK(Error_t::kNoError == m_pCInstance->compFeature(afResult, m_pfInput));
            CHECK(fTmp <= afResult[1]);
            fTmp = afResult[1];
        }
        CHECK(1.F == Approx(afResult[1]).margin(1e-4F).epsilon(1e-4F));

        // zeros
        CVectorFloat::setZero(m_pfInput, m_iBufferLength);
        CHECK(Error_t::kNoError == m_pCInstance->compFeature(afResult, m_pfInput));
        float fAlpha = CSinglePoleLp::calcFilterParam(1.5F, m_fSampleRate);
        CHECK(fAlpha == Approx(afResult[1]).margin(1e-4F).epsilon(1e-4F));
        CHECK(Error_t::kNoError == m_pCInstance->compFeature(afResult, m_pfInput));
        CHECK(std::pow(fAlpha, m_iBufferLength) == Approx(afResult[1]).margin(1e-3F).epsilon(1e-3F));
    }

    CFeatureFromBlockIf::destroy(m_pCInstance);

    delete[] m_pfInput;
}

TEST_CASE("Features (per array)", "[FeaturesClass]")
{

    CFeatureIf* m_pCInstance = 0;
    float* m_pfInput = 0;
    float m_fSampleRate = 44100;
    int m_iBlockLength = 1023,
        m_iHopLength = 512,
        m_iBufferLength = 96000;
    
    m_pfInput = new float[m_iBufferLength];
    CVectorFloat::setValue(m_pfInput, 1.F, m_iBufferLength);


    SECTION("Api")
    {
        for (auto f = 0; f < CFeatureIf::kNumFeatures; f++)
        {
            int aiDim[2] = { 0,0 };
            float** ppfFeature = 0;
            CHECK(Error_t::kFunctionInvalidArgsError == CFeatureIf::create(m_pCInstance, static_cast<CFeatureIf::Feature_t>(f), 0, m_iBufferLength, m_fSampleRate, m_iBlockLength, m_iHopLength));
            CHECK(Error_t::kFunctionInvalidArgsError == CFeatureIf::create(m_pCInstance, static_cast<CFeatureIf::Feature_t>(f), m_pfInput, 0, m_fSampleRate, m_iBlockLength, m_iHopLength));
            CHECK(Error_t::kFunctionInvalidArgsError == CFeatureIf::create(m_pCInstance, static_cast<CFeatureIf::Feature_t>(f), m_pfInput, -1, m_fSampleRate, m_iBlockLength, m_iHopLength));
            CHECK(Error_t::kFunctionInvalidArgsError == CFeatureIf::create(m_pCInstance, static_cast<CFeatureIf::Feature_t>(f), m_pfInput, m_iBufferLength, 0, m_iBlockLength, m_iHopLength));
            CHECK(Error_t::kFunctionInvalidArgsError == CFeatureIf::create(m_pCInstance, static_cast<CFeatureIf::Feature_t>(f), m_pfInput, m_iBufferLength, m_fSampleRate, 0, m_iHopLength));
            CHECK(Error_t::kFunctionInvalidArgsError == CFeatureIf::create(m_pCInstance, static_cast<CFeatureIf::Feature_t>(f), m_pfInput, m_iBufferLength, m_fSampleRate, m_iBlockLength, 0));

            CHECK(Error_t::kNoError == CFeatureIf::create(m_pCInstance, static_cast<CFeatureIf::Feature_t>(f), m_pfInput, m_iBufferLength, m_fSampleRate, m_iBlockLength, m_iHopLength));

            CHECK_FALSE(m_pCInstance == 0);

            CHECK(Error_t::kNoError == m_pCInstance->getFeatureDimensions(aiDim[0], aiDim[1]));
            CHECK(aiDim[0] > 0);
            CHECK(aiDim[1] > 0);

            ppfFeature = new float* [aiDim[0]];
            for (auto i = 0; i < aiDim[0]; i++)
                ppfFeature[i] = new float[aiDim[1]];

            CHECK(Error_t::kFunctionInvalidArgsError == m_pCInstance->compFeature1Dim(0));
            CHECK(Error_t::kFunctionInvalidArgsError == m_pCInstance->compFeatureNDim(0));


            CHECK(Error_t::kNoError == CFeatureIf::destroy(m_pCInstance));

            if (ppfFeature)
            {
                for (auto i = 0; i < aiDim[0]; i++)
                    delete[] ppfFeature[i];
            }
            delete[] ppfFeature;
        }
    }
    CFeatureIf::destroy(m_pCInstance);

    delete[] m_pfInput;
}

#endif //WITH_TESTS
