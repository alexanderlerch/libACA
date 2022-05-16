#include "ACAConfig.h"

#ifdef WITH_TESTS

#include "Vector.h"
#include "ToolBlockAudio.h"
#include "ToolConversion.h"

#include "gtest/gtest.h"


namespace {
    void CHECK_ARRAY_CLOSE(float* buffer1, float* buffer2, int iLength, float fTolerance)
    {
        for (int i = 0; i < iLength; i++)
        {
            EXPECT_NEAR(buffer1[i], buffer2[i], fTolerance);
        }
    }

    class ToolsBlockAudio : public testing::Test
    {
    protected:
        void SetUp() override
        {
            m_pfAudio = new float[m_iBufferLength];
            m_pfBlock = new float[1024];
            for (auto i = 0; i < m_iBufferLength; i++)
                m_pfAudio[i] = static_cast<float>(i);

        }

        virtual void TearDown()
        {
            CBlockAudioIf::destroy(m_pCBlockAudio);

            delete[] m_pfBlock;
            delete[] m_pfAudio;
        }

        CBlockAudioIf* m_pCBlockAudio = 0;

        float* m_pfAudio = 0;
        float* m_pfBlock = 0;

        float m_fSampleRate = 0;

        int m_iBlockLength = 0,
            m_iHopLength = 0,
            m_iAudioLength = 0,
            m_iBufferLength = 40000;
    };

    class ToolsConversion : public testing::Test
    {
    protected:
        void SetUp() override
        {
            m_pfMel = new float[m_iNumValues];
            m_pfFreq = new float[m_iNumValues];
            m_pfOut = new float[m_iNumValues];

            for (auto m = 0; m < m_iNumValues; m++)
                m_pfMel[m] = static_cast<float>(m);
        }

        virtual void TearDown()
        {
            CFreq2Mel2Freq::destroy(m_pCFreq2Mel2Freq);

            delete[] m_pfMel;
            delete[] m_pfFreq;
            delete[] m_pfOut;
        }

        CFreq2Mel2Freq* m_pCFreq2Mel2Freq = 0;
        float* m_pfMel = 0,
            * m_pfFreq = 0,
            * m_pfOut = 0;
        int m_iNumValues = 1024;
    };
}

TEST_F(ToolsConversion, Freq2Mel2Freq)
{
    // Mel (Fant)
    EXPECT_EQ(Error_t::kNoError, CFreq2Mel2Freq::create(m_pCFreq2Mel2Freq, CFreq2Mel2Freq::kFant));

    EXPECT_NEAR(1000.F, m_pCFreq2Mel2Freq->convertFreq2Mel(1000.F), 1e-6F);
    EXPECT_NEAR(1000.F, m_pCFreq2Mel2Freq->convertMel2Freq(1000.F), 1e-6F);

    m_pCFreq2Mel2Freq->convertMel2Freq(m_pfFreq, m_pfMel, m_iNumValues);
    m_pCFreq2Mel2Freq->convertFreq2Mel(m_pfOut, m_pfFreq, m_iNumValues);

    CHECK_ARRAY_CLOSE(m_pfMel, m_pfOut, m_iNumValues, 1e-3F);
    EXPECT_EQ(Error_t::kNoError, CFreq2Mel2Freq::destroy(m_pCFreq2Mel2Freq));

    // Mel (Shaughnessy)
    EXPECT_EQ(Error_t::kNoError, CFreq2Mel2Freq::create(m_pCFreq2Mel2Freq, CFreq2Mel2Freq::kShaughnessy));

    EXPECT_NEAR(1000.F, m_pCFreq2Mel2Freq->convertFreq2Mel(1000.F), 1e-1F);
    EXPECT_NEAR(1000.F, m_pCFreq2Mel2Freq->convertMel2Freq(1000.F), 1e-1F);

    m_pCFreq2Mel2Freq->convertMel2Freq(m_pfFreq, m_pfMel, m_iNumValues);
    m_pCFreq2Mel2Freq->convertFreq2Mel(m_pfOut, m_pfFreq, m_iNumValues);

    CHECK_ARRAY_CLOSE(m_pfMel, m_pfOut, m_iNumValues, 1e-3F);
    EXPECT_EQ(Error_t::kNoError, CFreq2Mel2Freq::destroy(m_pCFreq2Mel2Freq));

    // Mel (Umesh)
    EXPECT_EQ(Error_t::kNoError, CFreq2Mel2Freq::create(m_pCFreq2Mel2Freq, CFreq2Mel2Freq::kUmesh));

    EXPECT_NEAR(1000.F, m_pCFreq2Mel2Freq->convertFreq2Mel(1000.F), 25.F);
    EXPECT_NEAR(1000.F, m_pCFreq2Mel2Freq->convertMel2Freq(1000.F), 25.F);

    m_pCFreq2Mel2Freq->convertMel2Freq(m_pfFreq, m_pfMel, m_iNumValues);
    m_pCFreq2Mel2Freq->convertFreq2Mel(m_pfOut, m_pfFreq, m_iNumValues);

    CHECK_ARRAY_CLOSE(m_pfMel, m_pfOut, m_iNumValues, 1e-3F);
    EXPECT_EQ(Error_t::kNoError, CFreq2Mel2Freq::destroy(m_pCFreq2Mel2Freq));
}

TEST_F(ToolsConversion, Freq2Midi2Freq)
{
    EXPECT_NEAR(69.F, CFreq2Midi2Freq::convertFreq2Midi(440.F), 1e-6F);
    EXPECT_NEAR(57.F, CFreq2Midi2Freq::convertFreq2Midi(440.F, 880.F), 1e-6F);
    EXPECT_NEAR(81.F, CFreq2Midi2Freq::convertFreq2Midi(440.F, 220.F), 1e-6F);
    EXPECT_NEAR(70.F, CFreq2Midi2Freq::convertFreq2Midi(440.F * 1.0594630943593F), 1e-6F);

    EXPECT_NEAR(440.F, CFreq2Midi2Freq::convertMidi2Freq(69.F), 1e-6F);
    EXPECT_NEAR(440.F, CFreq2Midi2Freq::convertMidi2Freq(57.F, 880.F), 1e-6F);
    EXPECT_NEAR(440.F, CFreq2Midi2Freq::convertMidi2Freq(81.F, 220.F), 1e-6F);
    EXPECT_NEAR(440.F, CFreq2Midi2Freq::convertMidi2Freq(70.F) / 1.0594630943593F, 1e-6F);

    CFreq2Midi2Freq::convertMidi2Freq(m_pfFreq, m_pfMel, 128);
    CFreq2Midi2Freq::convertFreq2Midi(m_pfOut, m_pfFreq, 128);

    CHECK_ARRAY_CLOSE(m_pfMel, m_pfOut, 128, 1e-3F);
}

TEST_F(ToolsConversion, Freq2Bin2Freq)
{
    float fSampleRate = 48000.f;
    int iFftLength = 16;

    EXPECT_NEAR(0.F, CFreq2Bin2Freq::convertFreq2Bin(0.F, iFftLength, fSampleRate), 1e-6F);
    EXPECT_NEAR(iFftLength / 2.F, CFreq2Bin2Freq::convertFreq2Bin(fSampleRate / 2, iFftLength, fSampleRate), 1e-6F);
    EXPECT_NEAR(1.F, CFreq2Bin2Freq::convertFreq2Bin(fSampleRate / iFftLength, iFftLength, fSampleRate), 1e-6F);

    EXPECT_NEAR(0.F, CFreq2Bin2Freq::convertBin2Freq(0.F, iFftLength, fSampleRate), 1e-6F);
    EXPECT_NEAR(fSampleRate / 2, CFreq2Bin2Freq::convertBin2Freq(iFftLength / 2.F, iFftLength, fSampleRate), 1e-6F);
    EXPECT_NEAR(fSampleRate / iFftLength, CFreq2Bin2Freq::convertBin2Freq(1.F, iFftLength, fSampleRate), 1e-6F);

    CFreq2Bin2Freq::convertBin2Freq(m_pfFreq, m_pfMel, iFftLength, iFftLength, fSampleRate);
    CFreq2Bin2Freq::convertFreq2Bin(m_pfOut, m_pfFreq, iFftLength, iFftLength, fSampleRate);

    CHECK_ARRAY_CLOSE(m_pfMel, m_pfOut, iFftLength, 1e-3F);
}

TEST_F(ToolsBlockAudio, Dimensions)
{
    m_iBlockLength = 20;
    m_iHopLength = 10;
    m_fSampleRate = 1;
    m_iAudioLength = 101;

    CBlockAudioIf::create(m_pCBlockAudio, m_pfAudio, m_iAudioLength, m_iBlockLength, m_iHopLength, m_fSampleRate);

    EXPECT_EQ(m_pCBlockAudio->getNumBlocks(), m_iAudioLength / m_iHopLength + 1);

    CBlockAudioIf::destroy(m_pCBlockAudio);

    m_iBlockLength = 1024;
    m_iHopLength = 512;
    m_fSampleRate = 40000;
    m_iAudioLength = 40000;

    CBlockAudioIf::create(m_pCBlockAudio, m_pfAudio, m_iAudioLength, m_iBlockLength, m_iHopLength, m_fSampleRate);

    EXPECT_EQ(79, m_pCBlockAudio->getNumBlocks());
}

TEST_F(ToolsBlockAudio, Content)
{
    m_iBlockLength = 20;
    m_iHopLength = 10;
    m_fSampleRate = 1;
    m_iAudioLength = 101;

    CBlockAudioIf::create(m_pCBlockAudio, m_pfAudio, m_iAudioLength, m_iBlockLength, m_iHopLength, m_fSampleRate);

    // check block 9
    float fTestTimeStamp = 0,
        fTargetTimeStamp = m_iBlockLength / m_fSampleRate * .5F;
    int iBlockIdx = 9;
    for (auto n = 0; n <= iBlockIdx; n++)
    {
        m_pCBlockAudio->getNextBlock(m_pfBlock, &fTestTimeStamp);
        EXPECT_NEAR(fTestTimeStamp, fTargetTimeStamp, 1e-6F);
        fTargetTimeStamp += m_iHopLength / m_fSampleRate;
        EXPECT_EQ(false, m_pCBlockAudio->IsEndOfData());
    }

    for (auto i = 0; i < m_iHopLength+1; i++)
        EXPECT_EQ(m_pfBlock[i], iBlockIdx*m_iHopLength + i);
}


TEST_F(ToolsBlockAudio, Api)
{
    m_iBlockLength = 20;
    m_iHopLength = 10;
    m_fSampleRate = 1;
    m_iAudioLength = 101;

    EXPECT_EQ(false, Error_t::kNoError == CBlockAudioIf::create(m_pCBlockAudio, 0, m_iAudioLength, m_iBlockLength, m_iHopLength, m_fSampleRate));
    EXPECT_EQ(false, Error_t::kNoError == CBlockAudioIf::create(m_pCBlockAudio, m_pfAudio, 0, m_iBlockLength, m_iHopLength, m_fSampleRate));
    EXPECT_EQ(false, Error_t::kNoError == CBlockAudioIf::create(m_pCBlockAudio, m_pfAudio, m_iAudioLength, 0, m_iHopLength, m_fSampleRate));
    EXPECT_EQ(false, Error_t::kNoError == CBlockAudioIf::create(m_pCBlockAudio, m_pfAudio, m_iAudioLength, m_iBlockLength, 0, m_fSampleRate));
    EXPECT_EQ(false, Error_t::kNoError == CBlockAudioIf::create(m_pCBlockAudio, m_pfAudio, m_iAudioLength, m_iBlockLength, m_iHopLength, 0));
    EXPECT_EQ(false, Error_t::kNoError == CBlockAudioIf::create(m_pCBlockAudio, m_pfAudio, -1, m_iBlockLength, m_iHopLength, m_fSampleRate));
    EXPECT_EQ(false, Error_t::kNoError == CBlockAudioIf::create(m_pCBlockAudio, m_pfAudio, m_iAudioLength, -1, m_iHopLength, m_fSampleRate));
    EXPECT_EQ(false, Error_t::kNoError == CBlockAudioIf::create(m_pCBlockAudio, m_pfAudio, m_iAudioLength, m_iBlockLength, -1, m_fSampleRate));
    EXPECT_EQ(false, Error_t::kNoError == CBlockAudioIf::create(m_pCBlockAudio, m_pfAudio, m_iAudioLength, m_iBlockLength, m_iHopLength, -1));
    EXPECT_EQ(false, Error_t::kNoError == CBlockAudioIf::create(m_pCBlockAudio, m_pfAudio, m_iAudioLength, m_iBlockLength, m_iBlockLength<<1, m_fSampleRate));
}



#endif //WITH_TESTS
