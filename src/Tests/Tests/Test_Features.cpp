#include "ACAConfig.h"

#ifdef WITH_TESTS

#include "Vector.h"
#include "Synthesis.h"
#include "FeaturefromBlock.h"

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
        m_pfInput[k] = k;

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
    EXPECT_EQ(std::sqrtf(std::sqrtf(10)) / (13.F / 4), CFeatureFromBlockIf::compFeatureSpectralFlatness(m_pfInput, 4));
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
    EXPECT_NEAR(1.F / std::sqrtf(m_iBufferLength / 2.F), CFeatureFromBlockIf::compFeatureSpectralFlux(m_pfInput, &m_pfInput[iIdx], m_iBufferLength / 2), 1e-4F);
    EXPECT_NEAR(1.F / std::sqrtf(m_iBufferLength / 2.F), CFeatureFromBlockIf::compFeatureSpectralFlux(&m_pfInput[iIdx], m_pfInput, m_iBufferLength / 2), 1e-4F);

    // one spectrum zero, the other two
    CVectorFloat::mulC_I(m_pfInput, 2.F, m_iBufferLength);
    EXPECT_NEAR(std::sqrtf(4.F * (m_iBufferLength / 2)) / (m_iBufferLength / 2), CFeatureFromBlockIf::compFeatureSpectralFlux(m_pfInput, &m_pfInput[iIdx], m_iBufferLength / 2), 1e-4F);
    EXPECT_NEAR(std::sqrtf(4.F * (m_iBufferLength /2)) / (m_iBufferLength / 2), CFeatureFromBlockIf::compFeatureSpectralFlux(&m_pfInput[iIdx], m_pfInput, m_iBufferLength / 2), 1e-4F);

    // alternating spectral bins
    iIdx = 4;
    m_iBufferLength = 4;
    m_pfInput[0] = 1;
    m_pfInput[2] = 1;
    m_pfInput[iIdx + 1] = 1;
    m_pfInput[iIdx + 3] = 1;
    EXPECT_NEAR(1.F / std::sqrtf(m_iBufferLength), CFeatureFromBlockIf::compFeatureSpectralFlux(m_pfInput, &m_pfInput[iIdx], m_iBufferLength), 1e-4F);
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
    EXPECT_NEAR(std::sqrtf(10.F / 5.F) / (2 * (m_iBufferLength - 1)), CFeatureFromBlockIf::compFeatureSpectralSpread(m_pfInput, m_iBufferLength, m_fSampleRate), 1e-4F);
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
        m_pfInput[k] = m_iBufferLength - 1 - k;
    EXPECT_EQ(true, 0.F < CFeatureFromBlockIf::compFeatureSpectralSkewness(m_pfInput, m_iBufferLength, m_fSampleRate));

    // increasing spectrum -> negative skewness
    for (auto k = 0; k < m_iBufferLength; k++)
        m_pfInput[k] = k;
    EXPECT_EQ(false, 0.F < CFeatureFromBlockIf::compFeatureSpectralSkewness(m_pfInput, m_iBufferLength, m_fSampleRate));

    // symmetry
    for (auto k = m_iBufferLength/2; k < m_iBufferLength; k++)
        m_pfInput[k] = m_iBufferLength - 1 - k;
    EXPECT_NEAR(CFeatureFromBlockIf::compFeatureSpectralSkewness(&m_pfInput[m_iBufferLength / 2], m_iBufferLength / 2, m_fSampleRate), -1.F * CFeatureFromBlockIf::compFeatureSpectralSkewness(m_pfInput, m_iBufferLength / 2, m_fSampleRate), 1e-4F);
}

TEST_F(FeaturesStatic, SpectralSlope)
{
    // zero test
    EXPECT_EQ(0.F, CFeatureFromBlockIf::compFeatureSpectralSlope(m_pfInput, m_iBufferLength));

    // increasing spectrum
    for (auto k = 0; k < m_iBufferLength; k++)
        m_pfInput[k] = k;
    EXPECT_NEAR(1.F, CFeatureFromBlockIf::compFeatureSpectralSlope(m_pfInput, 5), 1e-6F);
    EXPECT_NEAR(1.F, CFeatureFromBlockIf::compFeatureSpectralSlope(m_pfInput, m_iBufferLength), 1e-2F);

    // flat spectrum
    CVectorFloat::setZero(m_pfInput, m_iBufferLength);
    EXPECT_NEAR(0.F, CFeatureFromBlockIf::compFeatureSpectralSlope(m_pfInput, 5), 1e-6F);
    EXPECT_NEAR(0.F, CFeatureFromBlockIf::compFeatureSpectralSlope(m_pfInput, m_iBufferLength), 1e-2F);
}

TEST_F(FeaturesStatic, TimeStd)
{
    // zero test
    EXPECT_EQ(0.F, CFeatureFromBlockIf::compFeatureTimeStd(m_pfInput, m_iBufferLength));

    // sine wave
    m_fSampleRate = 200;
    CSynthesis::generateSine(m_pfInput, 1, m_fSampleRate, m_iBufferLength);
    EXPECT_NEAR(1.F / std::sqrtf(2.F), CFeatureFromBlockIf::compFeatureTimeStd(m_pfInput, 1000), 1e-6F);

    // square wave
    m_fSampleRate = 200;
    CSynthesis::generateRect(m_pfInput, 1, m_fSampleRate, m_iBufferLength);
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



#endif //WITH_TESTS
