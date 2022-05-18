#include "ACAConfig.h"

#ifdef WITH_TESTS

#include "Vector.h"
#include "Synthesis.h"
#include "FeatureFromBlock.h"

#include "gtest/gtest.h"


namespace {
    void CHECK_ARRAY_CLOSE(float* buffer1, float* buffer2, int iLength, float fTolerance)
    {
        for (int i = 0; i < iLength; i++)
        {
            EXPECT_NEAR(buffer1[i], buffer2[i], fTolerance);
        }
    }

    class FeaturesStatic : public testing::Test
    {
    protected:
        void SetUp() override
        {
            m_pfInput = new float[m_iBufferLength];
            CVectorFloat::setZero(m_pfInput, m_iBufferLength);

        }

        virtual void TearDown()
        {
            delete[] m_pfInput;
        }

        float* m_pfInput = 0;

        float m_fSampleRate = 1;

        int m_iBufferLength = 1025;
    };

    class FeaturesClass : public testing::Test
    {
    protected:
        void SetUp() override
        {
            m_pfInput = new float[m_iBufferLength];
            CVectorFloat::setValue(m_pfInput, 1.F, m_iBufferLength);
            m_pfInput[m_iBufferLength / 2] = 2;
        }

        virtual void TearDown()
        {
            delete[] m_pfInput;
        }

        CFeatureFromBlockIf* pCInstance = 0;

        float* m_pfInput = 0;

        float m_fSampleRate = 1;

        int m_iBufferLength = 1025;
    };
}

TEST_F(FeaturesStatic, SpectralCentroid)
{
    // zero test
    EXPECT_EQ(0.F, CFeatureFromBlockIf::compFeatureSpectralCentroid(m_pfInput, m_iBufferLength));

    // 'sine' test
    m_pfInput[10] = 1;
    m_fSampleRate = 2048;
    EXPECT_EQ(10.F, CFeatureFromBlockIf::compFeatureSpectralCentroid(m_pfInput, m_iBufferLength, m_fSampleRate));
    m_fSampleRate = 32;
    EXPECT_EQ(10.F, CFeatureFromBlockIf::compFeatureSpectralCentroid(m_pfInput, 17, m_fSampleRate));

    // 'noise' test
    CVectorFloat::setValue(m_pfInput, 1, m_iBufferLength);
    EXPECT_EQ(m_fSampleRate / 4.F, CFeatureFromBlockIf::compFeatureSpectralCentroid(m_pfInput, m_iBufferLength, m_fSampleRate));

    // symmetric spectrum test
    CVectorFloat::setZero(&m_pfInput[1], m_iBufferLength - 2);
    EXPECT_EQ(m_fSampleRate / 4.F, CFeatureFromBlockIf::compFeatureSpectralCentroid(m_pfInput, m_iBufferLength, m_fSampleRate));
}

TEST_F(FeaturesStatic, SpectralCrestFactor)
{
    // zero test
    EXPECT_EQ(0.F, CFeatureFromBlockIf::compFeatureSpectralCrestFactor(m_pfInput, m_iBufferLength));

    // 'sine' test
    m_pfInput[10] = 1;
    m_fSampleRate = 2048;
    EXPECT_EQ(1.F, CFeatureFromBlockIf::compFeatureSpectralCrestFactor(m_pfInput, m_iBufferLength, m_fSampleRate));
    m_fSampleRate = 32;
    EXPECT_EQ(1.F, CFeatureFromBlockIf::compFeatureSpectralCrestFactor(m_pfInput, 17, m_fSampleRate));

    // 'noise' test
    CVectorFloat::setValue(m_pfInput, 1, m_iBufferLength);
    EXPECT_EQ(1.F / m_iBufferLength, CFeatureFromBlockIf::compFeatureSpectralCrestFactor(m_pfInput, m_iBufferLength));

    // symmetric spectrum test
    CVectorFloat::setZero(&m_pfInput[1], m_iBufferLength - 2);
    EXPECT_EQ(.5F, CFeatureFromBlockIf::compFeatureSpectralCrestFactor(m_pfInput, m_iBufferLength, m_fSampleRate));
}

TEST_F(FeaturesStatic, SpectralDecrease)
{
    // zero test
    EXPECT_EQ(0.F, CFeatureFromBlockIf::compFeatureSpectralDecrease(m_pfInput, m_iBufferLength));

    // increasing spectrum
    for (auto k = 0; k < m_iBufferLength; k++)
        m_pfInput[k] = static_cast<float>(k);

    EXPECT_NEAR((m_iBufferLength-1)/CVectorFloat::getSum(m_pfInput, m_iBufferLength), CFeatureFromBlockIf::compFeatureSpectralDecrease(m_pfInput, m_iBufferLength), 1e-6F);
}

TEST_F(FeaturesStatic, SpectralFlatness)
{
    // zero test
    EXPECT_EQ(0.F, CFeatureFromBlockIf::compFeatureSpectralFlatness(m_pfInput, m_iBufferLength));

    // 'sine' test
    m_pfInput[10] = 1;
    m_fSampleRate = 2048;
    EXPECT_NEAR(0.F, CFeatureFromBlockIf::compFeatureSpectralFlatness(m_pfInput, m_iBufferLength, m_fSampleRate), 1e-6F);
    m_fSampleRate = 32;
    EXPECT_NEAR(0.F, CFeatureFromBlockIf::compFeatureSpectralFlatness(m_pfInput, 17, m_fSampleRate), 1e-6F);

    // 'noise' test
    CVectorFloat::setValue(m_pfInput, 1, m_iBufferLength);
    EXPECT_EQ(1.F, CFeatureFromBlockIf::compFeatureSpectralFlatness(m_pfInput, m_iBufferLength));

    // one value zero
    m_pfInput[10] = 0;
    EXPECT_NEAR(0.F, CFeatureFromBlockIf::compFeatureSpectralFlatness(m_pfInput, 20, m_fSampleRate), 1e-6F);

    // 'sine w noise' test
    CVectorFloat::setValue(m_pfInput, 1, m_iBufferLength);
    m_pfInput[2] = 10;
    EXPECT_NEAR(std::sqrt(std::sqrt(10)) / (13.F / 4), CFeatureFromBlockIf::compFeatureSpectralFlatness(m_pfInput, 4), 1e-6F);
}

TEST_F(FeaturesStatic, SpectralFlux)
{
    int iIdx = m_iBufferLength / 2;
    // zero test
    EXPECT_EQ(0.F, CFeatureFromBlockIf::compFeatureSpectralFlux(m_pfInput, &m_pfInput[iIdx], m_iBufferLength / 2));

    // 'sine' test
    m_pfInput[10] = 1;
    m_pfInput[iIdx + 10] = 1;
    m_fSampleRate = 2048;
    EXPECT_NEAR(0.F, CFeatureFromBlockIf::compFeatureSpectralFlux(m_pfInput, &m_pfInput[iIdx], m_iBufferLength / 2, m_fSampleRate), 1e-6F);
    m_fSampleRate = 32;
    EXPECT_NEAR(0.F, CFeatureFromBlockIf::compFeatureSpectralFlux(m_pfInput, &m_pfInput[iIdx], m_iBufferLength / 2, m_fSampleRate), 1e-6F);

    // 'noise' test
    CVectorFloat::setValue(m_pfInput, 1, m_iBufferLength);
    EXPECT_NEAR(0.F, CFeatureFromBlockIf::compFeatureSpectralFlux(m_pfInput, &m_pfInput[iIdx], m_iBufferLength / 2), 1e-6F);

    // one spectrum zero, the other one
    CVectorFloat::setZero(m_pfInput, m_iBufferLength / 2);
    EXPECT_NEAR(1.F / std::sqrt(m_iBufferLength / 2.F), CFeatureFromBlockIf::compFeatureSpectralFlux(m_pfInput, &m_pfInput[iIdx], m_iBufferLength / 2), 1e-4F);
    EXPECT_NEAR(1.F / std::sqrt(m_iBufferLength / 2.F), CFeatureFromBlockIf::compFeatureSpectralFlux(&m_pfInput[iIdx], m_pfInput, m_iBufferLength / 2), 1e-4F);

    // one spectrum zero, the other two
    CVectorFloat::mulC_I(m_pfInput, 2.F, m_iBufferLength);
    EXPECT_NEAR(std::sqrt(4.F * (m_iBufferLength / 2)) / (m_iBufferLength / 2), CFeatureFromBlockIf::compFeatureSpectralFlux(m_pfInput, &m_pfInput[iIdx], m_iBufferLength / 2), 1e-4F);
    EXPECT_NEAR(std::sqrt(4.F * (m_iBufferLength /2)) / (m_iBufferLength / 2), CFeatureFromBlockIf::compFeatureSpectralFlux(&m_pfInput[iIdx], m_pfInput, m_iBufferLength / 2), 1e-4F);

    // alternating spectral bins
    iIdx = 4;
    m_iBufferLength = 4;
    m_pfInput[0] = 1;
    m_pfInput[2] = 1;
    m_pfInput[iIdx + 1] = 1;
    m_pfInput[iIdx + 3] = 1;
    EXPECT_NEAR(1.F / std::sqrt(static_cast<float>(m_iBufferLength)), CFeatureFromBlockIf::compFeatureSpectralFlux(m_pfInput, &m_pfInput[iIdx], m_iBufferLength), 1e-4F);
}

TEST_F(FeaturesStatic, SpectralKurtosis)
{
    // zero test
    EXPECT_EQ(0.F, CFeatureFromBlockIf::compFeatureSpectralKurtosis(m_pfInput, m_iBufferLength));

    // 'sine' test
    m_pfInput[10] = 1;
    m_fSampleRate = 2048;
    EXPECT_EQ(0.F, CFeatureFromBlockIf::compFeatureSpectralKurtosis(m_pfInput, m_iBufferLength, m_fSampleRate));
    m_fSampleRate = 32;
    EXPECT_EQ(0.F, CFeatureFromBlockIf::compFeatureSpectralKurtosis(m_pfInput, 17, m_fSampleRate));

    // flat spectrum test
    m_fSampleRate = 1;
    CVectorFloat::setValue(m_pfInput, 1.F, m_iBufferLength);
    EXPECT_NEAR(-1.2F, CFeatureFromBlockIf::compFeatureSpectralKurtosis(m_pfInput, m_iBufferLength, m_fSampleRate), 1e-4F);
}

TEST_F(FeaturesStatic, SpectralRolloff)
{
    // zero test
    EXPECT_EQ(0.F, CFeatureFromBlockIf::compFeatureSpectralRolloff(m_pfInput, m_iBufferLength));

    // flat spectrum test
    m_fSampleRate = 200;
    CVectorFloat::setValue(m_pfInput, 1.F, m_iBufferLength);
    EXPECT_NEAR(85.F, CFeatureFromBlockIf::compFeatureSpectralRolloff(m_pfInput, 101, m_fSampleRate), 1e-4F);
    EXPECT_NEAR(75.F, CFeatureFromBlockIf::compFeatureSpectralRolloff(m_pfInput, 101, m_fSampleRate, .75F), 1e-4F);
}

TEST_F(FeaturesStatic, SpectralSkewness)
{
    // zero test
    EXPECT_EQ(0.F, CFeatureFromBlockIf::compFeatureSpectralKurtosis(m_pfInput, m_iBufferLength));

    // 'sine' test
    m_pfInput[512] = 1;
    m_fSampleRate = 2048;
    EXPECT_EQ(0.F, CFeatureFromBlockIf::compFeatureSpectralSkewness(m_pfInput, m_iBufferLength, m_fSampleRate));
    m_fSampleRate = 32;
    EXPECT_EQ(0.F, CFeatureFromBlockIf::compFeatureSpectralSkewness(m_pfInput, 17, m_fSampleRate));

    // flat spectrum test
    m_fSampleRate = 1;
    CVectorFloat::setValue(m_pfInput, 1.F, m_iBufferLength);
    EXPECT_NEAR(0.F, CFeatureFromBlockIf::compFeatureSpectralSkewness(m_pfInput, m_iBufferLength, m_fSampleRate), 1e-4F);

    // decreasing spectrum -> positive skewness
    for (auto k = 0; k < m_iBufferLength; k++)
        m_pfInput[k] = m_iBufferLength - 1.F - k;
    EXPECT_EQ(true, 0.F < CFeatureFromBlockIf::compFeatureSpectralSkewness(m_pfInput, m_iBufferLength, m_fSampleRate));

    // increasing spectrum -> negative skewness
    for (auto k = 0; k < m_iBufferLength; k++)
        m_pfInput[k] = static_cast<float>(k);
    EXPECT_EQ(false, 0.F < CFeatureFromBlockIf::compFeatureSpectralSkewness(m_pfInput, m_iBufferLength, m_fSampleRate));

    // symmetry
    for (auto k = m_iBufferLength/2; k < m_iBufferLength; k++)
        m_pfInput[k] = m_iBufferLength - 1.F - k;
    EXPECT_NEAR(CFeatureFromBlockIf::compFeatureSpectralSkewness(&m_pfInput[m_iBufferLength / 2], m_iBufferLength / 2, m_fSampleRate), -1.F * CFeatureFromBlockIf::compFeatureSpectralSkewness(m_pfInput, m_iBufferLength / 2, m_fSampleRate), 1e-4F);
}

TEST_F(FeaturesStatic, SpectralSlope)
{
    // zero test
    EXPECT_EQ(0.F, CFeatureFromBlockIf::compFeatureSpectralSlope(m_pfInput, m_iBufferLength));

    // increasing spectrum
    for (auto k = 0; k < m_iBufferLength; k++)
        m_pfInput[k] = static_cast<float>(k);
    EXPECT_NEAR(1.F, CFeatureFromBlockIf::compFeatureSpectralSlope(m_pfInput, 5), 1e-6F);
    EXPECT_NEAR(1.F, CFeatureFromBlockIf::compFeatureSpectralSlope(m_pfInput, m_iBufferLength), 1e-2F);

    // flat spectrum
    CVectorFloat::setZero(m_pfInput, m_iBufferLength);
    EXPECT_NEAR(0.F, CFeatureFromBlockIf::compFeatureSpectralSlope(m_pfInput, 5), 1e-6F);
    EXPECT_NEAR(0.F, CFeatureFromBlockIf::compFeatureSpectralSlope(m_pfInput, m_iBufferLength), 1e-2F);
}

TEST_F(FeaturesStatic, SpectralSpread)
{
    // zero test
    EXPECT_EQ(0.F, CFeatureFromBlockIf::compFeatureSpectralSpread(m_pfInput, m_iBufferLength));

    // 'sine' test
    m_pfInput[10] = 1;
    m_fSampleRate = 2048;
    EXPECT_EQ(0.F, CFeatureFromBlockIf::compFeatureSpectralSpread(m_pfInput, m_iBufferLength, m_fSampleRate));
    m_fSampleRate = 32;
    EXPECT_EQ(0.F, CFeatureFromBlockIf::compFeatureSpectralSpread(m_pfInput, 17, m_fSampleRate));

    // flat spectrum test
    m_fSampleRate = 1;
    CVectorFloat::setZero(m_pfInput, m_iBufferLength);
    CVectorFloat::setValue(&m_pfInput[5], 1.F, 5);
    m_iBufferLength = 17;
    EXPECT_NEAR(std::sqrt(10.F / 5.F) / (2 * (m_iBufferLength - 1)), CFeatureFromBlockIf::compFeatureSpectralSpread(m_pfInput, m_iBufferLength, m_fSampleRate), 1e-4F);
}

TEST_F(FeaturesStatic, SpectralTonalPowerRatio)
{
    // zero test
    EXPECT_EQ(0.F, CFeatureFromBlockIf::compFeatureSpectralTonalPowerRatio(m_pfInput, m_iBufferLength));

    // 'sine' test
    m_pfInput[10] = 1;
    m_fSampleRate = 2048;
    EXPECT_EQ(1.F, CFeatureFromBlockIf::compFeatureSpectralTonalPowerRatio(m_pfInput, m_iBufferLength, m_fSampleRate));
    m_fSampleRate = 32;
    EXPECT_EQ(1.F, CFeatureFromBlockIf::compFeatureSpectralTonalPowerRatio(m_pfInput, 17, m_fSampleRate));

    // multiple maxima 
    CVectorFloat::setValue(m_pfInput, .5F, m_iBufferLength);
    m_pfInput[100] = 2.F;
    m_pfInput[200] = 2.F;
    m_pfInput[300] = 2.F;
    m_pfInput[400] = 2.F;
    m_pfInput[500] = 2.F;
    EXPECT_NEAR(5.F * 4.F / (.25 * (m_iBufferLength - 5) + 5.F * 4.F), CFeatureFromBlockIf::compFeatureSpectralTonalPowerRatio(m_pfInput, m_iBufferLength, m_fSampleRate), 1e-4F);
}

TEST_F(FeaturesStatic, TimeAcfCoeff)
{
    int eta = 0;

    // zero test
    EXPECT_EQ(0.F, CFeatureFromBlockIf::compFeatureTimeAcfCoeff(m_pfInput, m_iBufferLength));

    // dc input
    CVectorFloat::setValue(m_pfInput, 1.F, 20);
    for (eta = 0; eta < 19; eta++)
        EXPECT_EQ(true, CFeatureFromBlockIf::compFeatureTimeAcfCoeff(m_pfInput, 20, m_fSampleRate, eta) > CFeatureFromBlockIf::compFeatureTimeAcfCoeff(m_pfInput, 20, m_fSampleRate, eta + 1));

    // sine wave
    eta = 500;
    m_fSampleRate = 1000;
    CSynthesis::generateSine(m_pfInput, 2, m_fSampleRate, m_iBufferLength, 1.F);
    EXPECT_NEAR(500*(1.F-eta/1000.F), CFeatureFromBlockIf::compFeatureTimeAcfCoeff(m_pfInput, 1000, m_fSampleRate, eta), 1e-4F);
}

TEST_F(FeaturesStatic, TimePeakEnvelope)
{
    // zero test
    EXPECT_EQ(0.F, CFeatureFromBlockIf::compFeatureTimePeakEnvelope(m_pfInput, m_iBufferLength));

    // offset
    CVectorFloat::setValue(m_pfInput, 1.F, m_iBufferLength);
    EXPECT_NEAR(1.F, CFeatureFromBlockIf::compFeatureTimePeakEnvelope(m_pfInput, m_iBufferLength), 1e-3F);

    // one maximum
    m_pfInput[117] = 2.F;
    EXPECT_NEAR(2.F, CFeatureFromBlockIf::compFeatureTimePeakEnvelope(m_pfInput, m_iBufferLength), 1e-3F);

    // negative maximm
    CVectorFloat::addC_I(m_pfInput, -5.F, m_iBufferLength);
    EXPECT_NEAR(-3.F, CFeatureFromBlockIf::compFeatureTimePeakEnvelope(m_pfInput, m_iBufferLength), 1e-3F);

    // sine wave
    m_fSampleRate = 200;
    CSynthesis::generateSine(m_pfInput, 1, m_fSampleRate, m_iBufferLength, .7F);
    EXPECT_NEAR(.7F, CFeatureFromBlockIf::compFeatureTimePeakEnvelope(m_pfInput, m_iBufferLength), 1e-6F);
}

TEST_F(FeaturesStatic, TimeRms)
{
    // zero test
    EXPECT_EQ(0.F, CFeatureFromBlockIf::compFeatureTimeRms(m_pfInput, m_iBufferLength));

    // sine wave
    m_fSampleRate = 200;
    CSynthesis::generateSine(m_pfInput, 1, m_fSampleRate, m_iBufferLength);
    EXPECT_NEAR(1.F / std::sqrt(2.F), CFeatureFromBlockIf::compFeatureTimeRms(m_pfInput, 1000), 1e-6F);

    // square wave
    m_fSampleRate = 200;
    CSynthesis::generateRect(m_pfInput, 1, m_fSampleRate, m_iBufferLength);
    EXPECT_NEAR(1.F, CFeatureFromBlockIf::compFeatureTimeRms(m_pfInput, 1000), 1e-3F);

    // square wave with offset
    CVectorFloat::addC_I(m_pfInput, 1, m_iBufferLength);
    EXPECT_NEAR(std::sqrt(2.F), CFeatureFromBlockIf::compFeatureTimeRms(m_pfInput, 1000), 1e-3F);
}
TEST_F(FeaturesStatic, TimeStd)
{
    // zero test
    EXPECT_EQ(0.F, CFeatureFromBlockIf::compFeatureTimeStd(m_pfInput, m_iBufferLength));

    // sine wave
    m_fSampleRate = 200;
    CSynthesis::generateSine(m_pfInput, 1, m_fSampleRate, m_iBufferLength);
    EXPECT_NEAR(1.F / std::sqrt(2.F), CFeatureFromBlockIf::compFeatureTimeStd(m_pfInput, 1000), 1e-6F);

    // square wave
    m_fSampleRate = 200;
    CSynthesis::generateRect(m_pfInput, 1, m_fSampleRate, m_iBufferLength);
    EXPECT_NEAR(1.F, CFeatureFromBlockIf::compFeatureTimeStd(m_pfInput, 1000), 1e-3F);

    // square wave with offset
    CVectorFloat::addC_I(m_pfInput, 1, m_iBufferLength);
    EXPECT_NEAR(1.F, CFeatureFromBlockIf::compFeatureTimeStd(m_pfInput, 1000), 1e-3F);
}

TEST_F(FeaturesStatic, TimeZeroCrossingRate)
{
    // zero test
    EXPECT_EQ(0.F, CFeatureFromBlockIf::compFeatureTimeZeroCrossingRate(m_pfInput, m_iBufferLength));

    // sine wave
    m_fSampleRate = 200;
    CSynthesis::generateSine(m_pfInput, 1, m_fSampleRate, m_iBufferLength);
    EXPECT_NEAR(19.F / 2000.F, CFeatureFromBlockIf::compFeatureTimeZeroCrossingRate(m_pfInput, 1000), 1e-6F);

    // square wave
    m_fSampleRate = 200;
    CSynthesis::generateRect(m_pfInput, 1, m_fSampleRate, m_iBufferLength);
    EXPECT_NEAR(19.F / 2000.F, CFeatureFromBlockIf::compFeatureTimeZeroCrossingRate(m_pfInput, 1000), 1e-6F);
}


TEST_F(FeaturesClass, Api)
{
    EXPECT_EQ(Error_t::kFunctionInvalidArgsError, CFeatureFromBlockIf::create(pCInstance, CFeatureFromBlockIf::kFeatureSpectralCentroid, 0, m_fSampleRate));
    EXPECT_EQ(Error_t::kFunctionInvalidArgsError, CFeatureFromBlockIf::create(pCInstance, CFeatureFromBlockIf::kFeatureSpectralCentroid, -1, m_fSampleRate));
    EXPECT_EQ(Error_t::kFunctionInvalidArgsError, CFeatureFromBlockIf::create(pCInstance, CFeatureFromBlockIf::kFeatureSpectralCentroid, m_iBufferLength, 0));
    EXPECT_EQ(Error_t::kFunctionInvalidArgsError, CFeatureFromBlockIf::create(pCInstance, CFeatureFromBlockIf::kFeatureSpectralCentroid, m_iBufferLength, -1));

    EXPECT_EQ(Error_t::kNoError, CFeatureFromBlockIf::create(pCInstance, CFeatureFromBlockIf::kFeatureSpectralCentroid, m_iBufferLength, m_fSampleRate));
    EXPECT_EQ(1, pCInstance->getFeatureDimensions());
    EXPECT_EQ(Error_t::kNoError, CFeatureFromBlockIf::destroy(pCInstance));
}

TEST_F(FeaturesClass, FeatureCalc)
{
    for (auto k = 0; k < CFeatureFromBlockIf::kNumFeatures; k++)
    {
        //implement me later
        if (k == CFeatureFromBlockIf::kFeatureSpectralMfccs ||
            k == CFeatureFromBlockIf::kFeatureSpectralPitchChroma ||
            k == CFeatureFromBlockIf::kFeatureTimeAcfCoeff ||
            k == CFeatureFromBlockIf::kFeatureTimeMaxAcf)
            continue;

        float fResult = 0;
        EXPECT_EQ(Error_t::kNoError, CFeatureFromBlockIf::create(pCInstance, static_cast<CFeatureFromBlockIf::Feature_t>(k), m_iBufferLength, m_fSampleRate));
        EXPECT_EQ(1, pCInstance->getFeatureDimensions());
        EXPECT_EQ(Error_t::kNoError, pCInstance->calcFeatureFromBlock(&fResult, m_pfInput));
        EXPECT_EQ(Error_t::kNoError, CFeatureFromBlockIf::destroy(pCInstance));
    }
}


#endif //WITH_TESTS
