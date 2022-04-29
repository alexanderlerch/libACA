#include "ACAConfig.h"

#ifdef WITH_TESTS

#include "Synthesis.h"
#include "Vector.h"
#include "Spectrogram.h"

#include "gtest/gtest.h"


namespace {

    class Spectrogram : public testing::Test
    {
    protected:
        void SetUp() override 
        {
            m_pfInput = new float [static_cast<int>(m_fs)];
            m_pfTimeStamps = new float[static_cast<int>(m_fs)];
        }

        virtual void TearDown()
        {
            if (m_ppfSpecGram)
            {
                for (int i = 0; i < m_aiSpecGramDimension[0]; i++)
                {
                    delete[] m_ppfSpecGram[i];
                }
                delete[] m_ppfSpecGram;
            }

            delete[] m_pfInput;
            m_pfInput = 0;

            CSpectrogramIf::destroy(m_pCSpecGram);

            delete[] m_pfTimeStamps;
        }

        CSpectrogramIf* m_pCSpecGram = 0;

        float** m_ppfSpecGram = 0;
        float* m_pfInput = 0;
        float* m_pfTimeStamps = 0;

        int  m_iBlockLength = 1024,
            m_iHopLength = 512;

        float m_fs = 40000,
            m_f0 = 400;

        int     m_aiSpecGramDimension[2] = { 0, 0 };

    };
}


TEST_F(Spectrogram, Api)
{
    EXPECT_EQ(false, Error_t::kNoError == CSpectrogramIf::create(m_pCSpecGram, static_cast<float*>(0), 1, m_fs, m_iBlockLength, m_iHopLength));
    EXPECT_EQ(false, Error_t::kNoError == CSpectrogramIf::create(m_pCSpecGram, m_pfInput, 0, m_fs, m_iBlockLength, m_iHopLength));
    EXPECT_EQ(false, Error_t::kNoError == CSpectrogramIf::create(m_pCSpecGram, m_pfInput, 1, 0, m_iBlockLength, m_iHopLength));
    EXPECT_EQ(false, Error_t::kNoError == CSpectrogramIf::create(m_pCSpecGram, m_pfInput, 1, m_fs, 0, m_iHopLength));
    EXPECT_EQ(false, Error_t::kNoError == CSpectrogramIf::create(m_pCSpecGram, m_pfInput, 1, m_fs, m_iBlockLength, 0));
    EXPECT_EQ(false, Error_t::kNoError == CSpectrogramIf::create(m_pCSpecGram, m_pfInput, 1, -1, m_iBlockLength, m_iHopLength));
    EXPECT_EQ(false, Error_t::kNoError == CSpectrogramIf::create(m_pCSpecGram, m_pfInput, 1, m_fs, -1, m_iHopLength));
    EXPECT_EQ(false, Error_t::kNoError == CSpectrogramIf::create(m_pCSpecGram, m_pfInput, 1, m_fs, m_iBlockLength, -1));
    EXPECT_EQ(false, Error_t::kNoError == CSpectrogramIf::create(m_pCSpecGram, m_pfInput, 1, m_fs, 3, m_iHopLength));

    EXPECT_EQ(Error_t::kNoError, CSpectrogramIf::create(m_pCSpecGram, m_pfInput, 5, m_fs, m_iBlockLength, m_iHopLength));
    EXPECT_EQ(false, Error_t::kNoError == m_pCSpecGram->getAxisVectors(0, CSpectrogramIf::kFrequencyInHz));
    EXPECT_EQ(Error_t::kNoError, CSpectrogramIf::destroy(m_pCSpecGram));
}


TEST_F(Spectrogram, Dimensions)
{
    float* pfVector = 0;

    m_f0 = 400;

    CSynthesis::generateSine(m_pfInput, m_f0, m_fs, static_cast<int>(m_fs));

    EXPECT_EQ(Error_t::kNoError, CSpectrogramIf::create(m_pCSpecGram, m_pfInput, static_cast<int>(m_fs), m_fs, m_iBlockLength, m_iHopLength));

    EXPECT_EQ(Error_t::kNoError, m_pCSpecGram->getSpectrogramDimensions(m_aiSpecGramDimension[0], m_aiSpecGramDimension[1]));

    EXPECT_EQ(m_iBlockLength / 2 + 1, m_aiSpecGramDimension[0]);
    EXPECT_EQ(static_cast<int>(m_fs) / m_iHopLength + 1, m_aiSpecGramDimension[1]);

    pfVector = new float[std::max(m_aiSpecGramDimension[0], m_aiSpecGramDimension[1])];

    EXPECT_EQ(Error_t::kNoError, m_pCSpecGram->getAxisVectors(pfVector, CSpectrogramIf::kFrequencyInHz));
    EXPECT_EQ(pfVector[0], 0);
    EXPECT_NEAR(pfVector[1], m_fs / m_iBlockLength, 1e-6);
    EXPECT_NEAR(pfVector[m_iBlockLength / 2], m_fs / 2, 1e-6);

    EXPECT_EQ(Error_t::kNoError, m_pCSpecGram->getAxisVectors(pfVector, CSpectrogramIf::kTimeInS));
    EXPECT_NEAR(pfVector[0], m_iBlockLength / (2 * m_fs), 1e-6);
    EXPECT_NEAR(pfVector[1], pfVector[0] + m_iHopLength / m_fs, 1e-6);

    EXPECT_EQ(Error_t::kNoError, CSpectrogramIf::destroy(m_pCSpecGram));

    delete[] pfVector;
}

TEST_F(Spectrogram, Values)
{
    m_f0 = 80;
    m_fs = 8192;
    m_iBlockLength = 1024;
    m_iHopLength = 512;

    CSynthesis::generateSine(m_pfInput, m_f0, m_fs, static_cast<int>(m_fs));

    EXPECT_EQ(Error_t::kNoError, CSpectrogramIf::create(m_pCSpecGram, m_pfInput, static_cast<int>(m_fs), m_fs, m_iBlockLength, m_iHopLength));

    EXPECT_EQ(Error_t::kNoError, m_pCSpecGram->getSpectrogramDimensions(m_aiSpecGramDimension[0], m_aiSpecGramDimension[1]));

    m_ppfSpecGram = new float* [m_aiSpecGramDimension[0]];
    for (auto k = 0; k < m_aiSpecGramDimension[0]; k++)
        m_ppfSpecGram[k] = new float[m_aiSpecGramDimension[1]];

    m_pCSpecGram->process(m_ppfSpecGram);

    EXPECT_NEAR(m_ppfSpecGram[10][10] - m_ppfSpecGram[20][10], m_ppfSpecGram[10][10], 1e-4F);
    EXPECT_NEAR(m_ppfSpecGram[115][10], 0, 1e-6F);
    EXPECT_NEAR(m_ppfSpecGram[116][10], 0, 1e-6F);

    EXPECT_EQ(Error_t::kNoError, CSpectrogramIf::destroy(m_pCSpecGram));
    
    m_f0 = 84;
    m_fs = 8192;
    m_iBlockLength = 1024;
    m_iHopLength = 512;

    CSynthesis::generateSine(m_pfInput, m_f0, m_fs, static_cast<int>(m_fs));

    EXPECT_EQ(Error_t::kNoError, CSpectrogramIf::create(m_pCSpecGram, m_pfInput, static_cast<int>(m_fs), m_fs, m_iBlockLength, m_iHopLength));

    EXPECT_EQ(Error_t::kNoError, m_pCSpecGram->getSpectrogramDimensions(m_aiSpecGramDimension[0], m_aiSpecGramDimension[1]));

    m_ppfSpecGram = new float* [m_aiSpecGramDimension[0]];
    for (auto k = 0; k < m_aiSpecGramDimension[0]; k++)
        m_ppfSpecGram[k] = new float[m_aiSpecGramDimension[1]];

    m_pCSpecGram->process(m_ppfSpecGram);

    EXPECT_NEAR(m_ppfSpecGram[10][10] - m_ppfSpecGram[11][10], 0, 1e-4F);

    EXPECT_EQ(Error_t::kNoError, CSpectrogramIf::destroy(m_pCSpecGram));

    m_f0 = 4;
    m_fs = 16;
    float fAmp = .5;
    m_iBlockLength = 16;
    m_iHopLength = 16;

    //reuse buffer for window setting
    CVectorFloat::setValue(m_pfTimeStamps, 1.F, m_iBlockLength);

    CSynthesis::generateSine(m_pfInput, m_f0, m_fs, static_cast<int>(m_fs), fAmp);

    EXPECT_EQ(Error_t::kNoError, CSpectrogramIf::create(m_pCSpecGram, m_pfInput, static_cast<int>(m_fs), m_fs, m_iBlockLength, m_iHopLength, false, m_pfTimeStamps));

    EXPECT_EQ(Error_t::kNoError, m_pCSpecGram->getSpectrogramDimensions(m_aiSpecGramDimension[0], m_aiSpecGramDimension[1]));

    m_ppfSpecGram = new float* [m_aiSpecGramDimension[0]];
    for (auto k = 0; k < m_aiSpecGramDimension[0]; k++)
        m_ppfSpecGram[k] = new float[m_aiSpecGramDimension[1]];

    m_pCSpecGram->process(m_ppfSpecGram);

    EXPECT_NEAR(m_ppfSpecGram[4][0], fAmp, 1e-6F);
    EXPECT_NEAR(m_ppfSpecGram[0][0], 0, 1e-4F);

    EXPECT_EQ(Error_t::kNoError, CSpectrogramIf::destroy(m_pCSpecGram));
    EXPECT_EQ(Error_t::kNoError, CSpectrogramIf::create(m_pCSpecGram, m_pfInput, static_cast<int>(m_fs), m_fs, m_iBlockLength, m_iHopLength, true, m_pfTimeStamps));

    m_pCSpecGram->process(m_ppfSpecGram);

    EXPECT_NEAR(m_ppfSpecGram[4][0], 1, 1e-6F);
    EXPECT_NEAR(m_ppfSpecGram[0][0], 0, 1e-6F);

    EXPECT_EQ(Error_t::kNoError, CSpectrogramIf::destroy(m_pCSpecGram));
}
#endif //WITH_TESTS
