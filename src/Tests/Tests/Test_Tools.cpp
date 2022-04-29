#include "ACAConfig.h"

#ifdef WITH_TESTS

#include "Vector.h"
#include "ToolBlockAudio.h"

#include "gtest/gtest.h"


namespace {
    void CHECK_ARRAY_EQUAL(int *buffer1, int *buffer2, int iLength)
    {
        for (int i = 0; i < iLength; i++)
        {
            EXPECT_EQ(buffer1[i], buffer2[i]);
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
                m_pfAudio[i] = i;

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
