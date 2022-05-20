#include "ACAConfig.h"

#ifdef WITH_TESTS

#include "Synthesis.h"
#include "Vector.h"
#include "ToolCcf.h"
#include "ToolBlockAudio.h"
#include "ToolConversion.h"
#include "ToolLowPass.h"

#include "gtest/gtest.h"


namespace {
    void CHECK_ARRAY_CLOSE(float* buffer1, float* buffer2, int iLength, float fTolerance = 1e-6F)
    {
        for (int i = 0; i < iLength; i++)
        {
            EXPECT_NEAR(buffer1[i], buffer2[i], fTolerance);
        }
    }

    class ToolsCcf : public testing::Test
    {
    protected:
        void SetUp() override
        {

            m_pCCcf = new CCcf();
            m_pfInput = new float[m_iNumValues];
            m_pfOut = new float[2*m_iNumValues];

            CVectorFloat::setZero(m_pfInput, m_iNumValues);            
            CVectorFloat::setZero(m_pfOut, 2 * m_iNumValues);
        }

        virtual void TearDown()
        {
            delete m_pCCcf;

            delete[] m_pfInput;
            delete[] m_pfOut;
        }

        CCcf* m_pCCcf = 0;
        float* m_pfInput = 0,
            * m_pfOut = 0;
        int m_iNumValues = 1024;
    };

    class ToolsSinglePole : public testing::Test
    {
    protected:
        void SetUp() override
        {

            CSinglePoleLp::create(m_pCLowPass);
            m_pfInput = new float[m_iNumValues];
            m_pfOut = new float[2 * m_iNumValues];

            CVectorFloat::setZero(m_pfInput, m_iNumValues);
            CVectorFloat::setZero(m_pfOut, m_iNumValues);
        }

        virtual void TearDown()
        {
            CSinglePoleLp::destroy(m_pCLowPass);

            delete[] m_pfInput;
            delete[] m_pfOut;
        }

        CSinglePoleLp* m_pCLowPass = 0;
        float* m_pfInput = 0,
            * m_pfOut = 0;
        int m_iNumValues = 1024;
    };

    class ToolsMovingAverage : public testing::Test
    {
    protected:
        void SetUp() override
        {

            CMovingAverage::create(m_pCLowPass);
            m_pfInput = new float[m_iNumValues];
            m_pfOut = new float[2 * m_iNumValues];

            CVectorFloat::setZero(m_pfInput, m_iNumValues);
            CVectorFloat::setZero(m_pfOut, m_iNumValues);
        }

        virtual void TearDown()
        {
            CMovingAverage::destroy(m_pCLowPass);

            delete[] m_pfInput;
            delete[] m_pfOut;
        }

        CMovingAverage* m_pCLowPass = 0;
        float* m_pfInput = 0,
            * m_pfOut = 0;
        int m_iNumValues = 1024;
    };

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
            delete[] m_pfMel;
            delete[] m_pfFreq;
            delete[] m_pfOut;
        }

        float* m_pfMel = 0,
            * m_pfFreq = 0,
            * m_pfOut = 0;
        int m_iNumValues = 1024;
    };
}


TEST_F(ToolsSinglePole, Api)
{
    EXPECT_EQ(Error_t::kFunctionInvalidArgsError, m_pCLowPass->setFilterParam(-1));
    EXPECT_EQ(Error_t::kFunctionInvalidArgsError, m_pCLowPass->setFilterParam(1));
    EXPECT_EQ(Error_t::kFunctionInvalidArgsError, m_pCLowPass->setFilterParam(1.1));

    EXPECT_EQ(Error_t::kNoError, m_pCLowPass->reset());

    EXPECT_EQ(Error_t::kNoError, m_pCLowPass->setFilterParam(.01F));
    EXPECT_NEAR(.01F, m_pCLowPass->getFilterParam(), 1e-6F);
    EXPECT_EQ(Error_t::kNoError, m_pCLowPass->setFilterParam(.5F));
    EXPECT_NEAR(.5F, m_pCLowPass->getFilterParam(), 1e-6F);
    EXPECT_EQ(Error_t::kNoError, m_pCLowPass->setFilterParam(.99F));
    EXPECT_NEAR(.99F, m_pCLowPass->getFilterParam(), 1e-6F);

    EXPECT_EQ(Error_t::kFunctionInvalidArgsError, m_pCLowPass->process(0, m_pfInput, m_iNumValues));
    EXPECT_EQ(Error_t::kFunctionInvalidArgsError, m_pCLowPass->process(m_pfOut, 0, m_iNumValues));
    EXPECT_EQ(Error_t::kFunctionInvalidArgsError, m_pCLowPass->process(m_pfOut, m_pfInput, 0));
    EXPECT_EQ(Error_t::kNoError, m_pCLowPass->process(m_pfOut, m_pfInput, m_iNumValues));
}

TEST_F(ToolsSinglePole, Process)
{
    // zeros
    for (auto i = 0; i < m_iNumValues; i++)
    {
        EXPECT_EQ(Error_t::kNoError, m_pCLowPass->process(&m_pfOut[i], &m_pfInput[i], 1));
        EXPECT_NEAR(0.F, m_pfOut[i], 1e-6F);
    }

    // ones
    CVectorFloat::setValue(m_pfInput, 1.F, m_iNumValues);
    for (auto c = 0; c < 10; c++)
    {
        float fAlpha = c / 10.1F;
        m_pCLowPass->reset();
        m_pCLowPass->setFilterParam(fAlpha);
        for (auto i = 0; i < m_iNumValues; i++)
        {
            EXPECT_EQ(Error_t::kNoError, m_pCLowPass->process(&m_pfOut[i], &m_pfInput[i], 1));
            EXPECT_NEAR(1.F - std::pow(m_pCLowPass->getFilterParam(), i + 1), m_pfOut[i], 1e-6F);
        }
    }
}

TEST_F(ToolsMovingAverage, Api)
{
    EXPECT_EQ(Error_t::kFunctionInvalidArgsError, m_pCLowPass->setFilterParam(0));
    EXPECT_EQ(Error_t::kFunctionInvalidArgsError, m_pCLowPass->setFilterParam(-5));

    EXPECT_EQ(Error_t::kNoError, m_pCLowPass->reset());

    EXPECT_EQ(Error_t::kNoError, m_pCLowPass->setFilterParam(5));
    EXPECT_NEAR(5, m_pCLowPass->getFilterParam(), 1e-6F);
    EXPECT_EQ(Error_t::kNoError, m_pCLowPass->setFilterParam(1000));
    EXPECT_NEAR(1000, m_pCLowPass->getFilterParam(), 1e-6F);
    EXPECT_EQ(Error_t::kNoError, m_pCLowPass->setFilterParam(1));
    EXPECT_NEAR(1, m_pCLowPass->getFilterParam(), 1e-6F);

    EXPECT_EQ(Error_t::kFunctionInvalidArgsError, m_pCLowPass->process(0, m_pfInput, m_iNumValues));
    EXPECT_EQ(Error_t::kFunctionInvalidArgsError, m_pCLowPass->process(m_pfOut, 0, m_iNumValues));
    EXPECT_EQ(Error_t::kFunctionInvalidArgsError, m_pCLowPass->process(m_pfOut, m_pfInput, 0));
    EXPECT_EQ(Error_t::kNoError, m_pCLowPass->process(m_pfOut, m_pfInput, m_iNumValues));
}

TEST_F(ToolsMovingAverage, Process)
{
    // zeros
    for (auto i = 0; i < m_iNumValues; i++)
    {
        EXPECT_EQ(Error_t::kNoError, m_pCLowPass->process(&m_pfOut[i], &m_pfInput[i], 1));
        EXPECT_NEAR(0.F, m_pfOut[i], 1e-6F);
    }

    // ones
    CVectorFloat::setValue(m_pfInput, 1.F, m_iNumValues);
    m_pCLowPass->reset();
    m_pCLowPass->setFilterParam(1);
    EXPECT_EQ(Error_t::kNoError, m_pCLowPass->process(m_pfOut, m_pfInput, m_iNumValues));
    CHECK_ARRAY_CLOSE(m_pfInput, m_pfOut, m_iNumValues);

    for (auto c = 0; c < 10; c++)
    {
        int iLength = c * 10.F + 2;
        m_pCLowPass->reset();
        m_pCLowPass->setFilterParam(iLength);
        for (auto i = 0; i < iLength; i++)
        {
            EXPECT_EQ(Error_t::kNoError, m_pCLowPass->process(&m_pfOut[i], &m_pfInput[i], 1));
            EXPECT_NEAR((i+1.F)/iLength, m_pfOut[i], 1e-6F);
        }
    }
}

TEST_F(ToolsCcf, Api)
{
    // not initialized
    EXPECT_EQ(Error_t::kFunctionInvalidArgsError, m_pCCcf->init(-1));
    EXPECT_EQ(Error_t::kFunctionInvalidArgsError, m_pCCcf->init(0));
    EXPECT_EQ(Error_t::kFunctionIllegalCallError, m_pCCcf->calcCcf(m_pfInput, m_pfInput));
    EXPECT_EQ(-1, m_pCCcf->getCcfLength());
    EXPECT_EQ(Error_t::kFunctionIllegalCallError, m_pCCcf->getCcf(m_pfOut));
    EXPECT_EQ(-1, m_pCCcf->getCcfMax());
    EXPECT_EQ(-1, m_pCCcf->getCcfMaxIdx());

    // initialized
    EXPECT_EQ(Error_t::kNoError, m_pCCcf->init(m_iNumValues));
    EXPECT_EQ(2 * m_iNumValues - 1, m_pCCcf->getCcfLength());
    EXPECT_EQ(Error_t::kFunctionIllegalCallError, m_pCCcf->getCcf(m_pfOut));
    EXPECT_EQ(-1, m_pCCcf->getCcfMax());
    EXPECT_EQ(-1, m_pCCcf->getCcfMaxIdx());

    EXPECT_EQ(Error_t::kFunctionInvalidArgsError, m_pCCcf->calcCcf(0, m_pfInput));
    EXPECT_EQ(Error_t::kFunctionInvalidArgsError, m_pCCcf->calcCcf(m_pfInput, 0));
    EXPECT_EQ(Error_t::kNoError, m_pCCcf->calcCcf(m_pfInput, m_pfInput));

    EXPECT_EQ(Error_t::kNoError, m_pCCcf->reset());
}

TEST_F(ToolsCcf, Acf)
{
    // zero
    EXPECT_EQ(Error_t::kNoError, m_pCCcf->init(6));
    EXPECT_EQ(Error_t::kNoError, m_pCCcf->calcCcf(m_pfInput, m_pfInput));
    EXPECT_EQ(11, m_pCCcf->getCcfLength());
    EXPECT_EQ(Error_t::kNoError, m_pCCcf->getCcf(m_pfOut));
    EXPECT_EQ(0, CVectorFloat::getSum(m_pfOut, 6));
    EXPECT_EQ(0, m_pCCcf->getCcfMax());
    EXPECT_EQ(0, m_pCCcf->getCcfMaxIdx());

    // dc input
    CVectorFloat::setValue(m_pfInput, 1.F, 16);
    EXPECT_EQ(Error_t::kNoError, m_pCCcf->init(16));
    EXPECT_EQ(Error_t::kNoError, m_pCCcf->calcCcf(m_pfInput, m_pfInput, false));
    EXPECT_EQ(16, m_pCCcf->getCcfLength(true));
    EXPECT_EQ(Error_t::kNoError, m_pCCcf->getCcf(m_pfOut, true));
    EXPECT_NEAR(16, m_pCCcf->getCcfMax(true), 1e-6F);
    EXPECT_NEAR(16, m_pCCcf->getCcfMax(false), 1e-6F);
    EXPECT_EQ(0, m_pCCcf->getCcfMaxIdx(true));
    EXPECT_EQ(16 - 1, m_pCCcf->getCcfMaxIdx(false));

    // normalized
    EXPECT_EQ(Error_t::kNoError, m_pCCcf->calcCcf(m_pfInput, m_pfInput, true));
    EXPECT_EQ(16, m_pCCcf->getCcfLength(true));
    EXPECT_EQ(Error_t::kNoError, m_pCCcf->getCcf(m_pfOut, true));
    EXPECT_NEAR(1.F, m_pCCcf->getCcfMax(true), 1e-6F);
    EXPECT_EQ(0, m_pCCcf->getCcfMaxIdx(true));

    // sine wave
    CSynthesis::generateSine(m_pfInput, 4, 512, m_iNumValues);
    EXPECT_EQ(Error_t::kNoError, m_pCCcf->init(m_iNumValues));
    EXPECT_EQ(Error_t::kNoError, m_pCCcf->calcCcf(m_pfInput, m_pfInput, false));
    EXPECT_EQ(m_iNumValues, m_pCCcf->getCcfLength(true));
    EXPECT_EQ(Error_t::kNoError, m_pCCcf->getCcf(m_pfOut, true));
    EXPECT_NEAR(m_iNumValues / 2, m_pCCcf->getCcfMax(), 1e-6F);
    EXPECT_EQ(m_iNumValues - 1, m_pCCcf->getCcfMaxIdx());

    // normalized
    EXPECT_EQ(Error_t::kNoError, m_pCCcf->calcCcf(m_pfInput, m_pfInput, true));
    EXPECT_EQ(Error_t::kNoError, m_pCCcf->getCcf(m_pfOut, true));
    EXPECT_NEAR(1.F, m_pCCcf->getCcfMax(), 1e-6F);

    // local maximum
    EXPECT_NEAR((1024 - 128) / 1024.F, m_pfOut[128], 1e-6F);
    EXPECT_EQ(true, m_pfOut[128] > m_pfOut[127] && m_pfOut[128] > m_pfOut[129]);
}
TEST_F(ToolsCcf, Ccf)
{
    int iBlockLength = 8;

    // sine wave w impulse
    CSynthesis::generateSine(m_pfInput, 1, iBlockLength, iBlockLength);
    m_pfInput[iBlockLength] = 1;
    EXPECT_EQ(Error_t::kNoError, m_pCCcf->init(iBlockLength));
    EXPECT_EQ(Error_t::kNoError, m_pCCcf->calcCcf(m_pfInput, &m_pfInput[iBlockLength], false));
    EXPECT_EQ(2 * iBlockLength - 1, m_pCCcf->getCcfLength(false));
    EXPECT_EQ(Error_t::kNoError, m_pCCcf->getCcf(m_pfOut, false));
    CHECK_ARRAY_CLOSE(m_pfInput, &m_pfOut[iBlockLength - 1], iBlockLength);

    // impulse w sine wave 
    CSynthesis::generateSine(m_pfInput, 1, iBlockLength, iBlockLength);
    m_pfInput[iBlockLength] = 1;
    EXPECT_EQ(Error_t::kNoError, m_pCCcf->init(iBlockLength));
    EXPECT_EQ(Error_t::kNoError, m_pCCcf->calcCcf(&m_pfInput[iBlockLength], m_pfInput, false));
    EXPECT_EQ(2 * iBlockLength - 1, m_pCCcf->getCcfLength(false));
    EXPECT_EQ(Error_t::kNoError, m_pCCcf->getCcf(m_pfOut, false));

    for (int eta = 0, i = iBlockLength-1; eta < iBlockLength; eta++, i--)
         EXPECT_NEAR(m_pfOut[eta], m_pfInput[i], 1e-6F);
}

TEST_F(ToolsConversion, Freq2Mel2Freq)
{
    // Mel (Fant)
    EXPECT_NEAR(1000.F, CConversion::convertFreq2Mel(1000.F, CConversion::kFant), 1e-6F);
    EXPECT_NEAR(1000.F, CConversion::convertMel2Freq(1000.F), 1e-6F);

    CConversion::convertMel2Freq(m_pfFreq, m_pfMel, m_iNumValues);
    CConversion::convertFreq2Mel(m_pfOut, m_pfFreq, m_iNumValues);

    CHECK_ARRAY_CLOSE(m_pfMel, m_pfOut, m_iNumValues, 1e-3F);

    // Mel (Shaughnessy)
    EXPECT_NEAR(1000.F, CConversion::convertFreq2Mel(1000.F, CConversion::kShaughnessy), 1e-1F);
    EXPECT_NEAR(1000.F, CConversion::convertMel2Freq(1000.F, CConversion::kShaughnessy), 1e-1F);

    CConversion::convertMel2Freq(m_pfFreq, m_pfMel, m_iNumValues);
    CConversion::convertFreq2Mel(m_pfOut, m_pfFreq, m_iNumValues);

    CHECK_ARRAY_CLOSE(m_pfMel, m_pfOut, m_iNumValues, 1e-3F);

    // Mel (Umesh)
    EXPECT_NEAR(1000.F, CConversion::convertFreq2Mel(1000.F, CConversion::kUmesh), 25.F);
    EXPECT_NEAR(1000.F, CConversion::convertMel2Freq(1000.F, CConversion::kUmesh), 25.F);

    CConversion::convertMel2Freq(m_pfFreq, m_pfMel, m_iNumValues);
    CConversion::convertFreq2Mel(m_pfOut, m_pfFreq, m_iNumValues);

    CHECK_ARRAY_CLOSE(m_pfMel, m_pfOut, m_iNumValues, 1e-3F);
}

TEST_F(ToolsConversion, Freq2Midi2Freq)
{
    EXPECT_NEAR(69.F, CConversion::convertFreq2Midi(440.F), 1e-6F);
    EXPECT_NEAR(57.F, CConversion::convertFreq2Midi(440.F, 880.F), 1e-6F);
    EXPECT_NEAR(81.F, CConversion::convertFreq2Midi(440.F, 220.F), 1e-6F);
    EXPECT_NEAR(70.F, CConversion::convertFreq2Midi(440.F * 1.0594630943593F), 1e-6F);

    EXPECT_NEAR(440.F, CConversion::convertMidi2Freq(69.F), 1e-6F);
    EXPECT_NEAR(440.F, CConversion::convertMidi2Freq(57.F, 880.F), 1e-6F);
    EXPECT_NEAR(440.F, CConversion::convertMidi2Freq(81.F, 220.F), 1e-6F);
    EXPECT_NEAR(440.F, CConversion::convertMidi2Freq(70.F) / 1.0594630943593F, 1e-6F);

    CConversion::convertMidi2Freq(m_pfFreq, m_pfMel, 128);
    CConversion::convertFreq2Midi(m_pfOut, m_pfFreq, 128);

    CHECK_ARRAY_CLOSE(m_pfMel, m_pfOut, 128, 1e-3F);
}

TEST_F(ToolsConversion, Freq2Bin2Freq)
{
    float fSampleRate = 48000.f;
    int iFftLength = 16;

    EXPECT_NEAR(0.F, CConversion::convertFreq2Bin(0.F, iFftLength, fSampleRate), 1e-6F);
    EXPECT_NEAR(iFftLength / 2.F, CConversion::convertFreq2Bin(fSampleRate / 2, iFftLength, fSampleRate), 1e-6F);
    EXPECT_NEAR(1.F, CConversion::convertFreq2Bin(fSampleRate / iFftLength, iFftLength, fSampleRate), 1e-6F);

    EXPECT_NEAR(0.F, CConversion::convertBin2Freq(0.F, iFftLength, fSampleRate), 1e-6F);
    EXPECT_NEAR(fSampleRate / 2, CConversion::convertBin2Freq(iFftLength / 2.F, iFftLength, fSampleRate), 1e-6F);
    EXPECT_NEAR(fSampleRate / iFftLength, CConversion::convertBin2Freq(1.F, iFftLength, fSampleRate), 1e-6F);

    CConversion::convertBin2Freq(m_pfFreq, m_pfMel, iFftLength, iFftLength, fSampleRate);
    CConversion::convertFreq2Bin(m_pfOut, m_pfFreq, iFftLength, iFftLength, fSampleRate);

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
