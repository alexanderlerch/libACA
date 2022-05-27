#include "ACAConfig.h"

#ifdef WITH_TESTS

#include "Synthesis.h"
#include "Vector.h"
#include "ToolCcf.h"
#include "ToolBlockAudio.h"
#include "ToolConversion.h"
#include "ToolLowPass.h"

#include "catch.hpp"


TEST_CASE("ToolsBlockAudio", "[ToolsSinglePole]")
{
    CBlockAudioIf* m_pCBlockAudio = 0;

    float* m_pfAudio = 0;
    float* m_pfBlock = 0;

    float m_fSampleRate = 0;

    int m_iBlockLength = 0,
        m_iHopLength = 0,
        m_iAudioLength = 0,
        m_iBufferLength = 40000;

    m_pfAudio = new float[m_iBufferLength];
    m_pfBlock = new float[1024];
    for (auto i = 0; i < m_iBufferLength; i++)
        m_pfAudio[i] = static_cast<float>(i);

    SECTION("Dimensions")
    {
        m_iBlockLength = 20;
        m_iHopLength = 10;
        m_fSampleRate = 1;
        m_iAudioLength = 101;

        CBlockAudioIf::create(m_pCBlockAudio, m_pfAudio, m_iAudioLength, m_iBlockLength, m_iHopLength, m_fSampleRate);

        CHECK(m_pCBlockAudio->getNumBlocks() == m_iAudioLength / m_iHopLength + 1);

        CBlockAudioIf::destroy(m_pCBlockAudio);

        m_iBlockLength = 1024;
        m_iHopLength = 512;
        m_fSampleRate = 40000;
        m_iAudioLength = 40000;

        CBlockAudioIf::create(m_pCBlockAudio, m_pfAudio, m_iAudioLength, m_iBlockLength, m_iHopLength, m_fSampleRate);

        CHECK(79 == m_pCBlockAudio->getNumBlocks());
    }

    SECTION("Content")
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
            CHECK(fTestTimeStamp == Approx(fTargetTimeStamp).margin(1e-6F).epsilon(1e-6F));
            fTargetTimeStamp += m_iHopLength / m_fSampleRate;
            CHECK_FALSE( m_pCBlockAudio->IsEndOfData());
        }

        for (auto i = 0; i < m_iHopLength + 1; i++)
            CHECK(m_pfBlock[i] == iBlockIdx* m_iHopLength + i);
    }


    SECTION("Api")
    {
        m_iBlockLength = 20;
        m_iHopLength = 10;
        m_fSampleRate = 1;
        m_iAudioLength = 101;

        CHECK_FALSE(Error_t::kNoError == CBlockAudioIf::create(m_pCBlockAudio, 0, m_iAudioLength, m_iBlockLength, m_iHopLength, m_fSampleRate));
        CHECK_FALSE(Error_t::kNoError == CBlockAudioIf::create(m_pCBlockAudio, m_pfAudio, 0, m_iBlockLength, m_iHopLength, m_fSampleRate));
        CHECK_FALSE(Error_t::kNoError == CBlockAudioIf::create(m_pCBlockAudio, m_pfAudio, m_iAudioLength, 0, m_iHopLength, m_fSampleRate));
        CHECK_FALSE(Error_t::kNoError == CBlockAudioIf::create(m_pCBlockAudio, m_pfAudio, m_iAudioLength, m_iBlockLength, 0, m_fSampleRate));
        CHECK_FALSE(Error_t::kNoError == CBlockAudioIf::create(m_pCBlockAudio, m_pfAudio, m_iAudioLength, m_iBlockLength, m_iHopLength, 0));
        CHECK_FALSE(Error_t::kNoError == CBlockAudioIf::create(m_pCBlockAudio, m_pfAudio, -1, m_iBlockLength, m_iHopLength, m_fSampleRate));
        CHECK_FALSE(Error_t::kNoError == CBlockAudioIf::create(m_pCBlockAudio, m_pfAudio, m_iAudioLength, -1, m_iHopLength, m_fSampleRate));
        CHECK_FALSE(Error_t::kNoError == CBlockAudioIf::create(m_pCBlockAudio, m_pfAudio, m_iAudioLength, m_iBlockLength, -1, m_fSampleRate));
        CHECK_FALSE(Error_t::kNoError == CBlockAudioIf::create(m_pCBlockAudio, m_pfAudio, m_iAudioLength, m_iBlockLength, m_iHopLength, -1));
        CHECK_FALSE(Error_t::kNoError == CBlockAudioIf::create(m_pCBlockAudio, m_pfAudio, m_iAudioLength, m_iBlockLength, m_iBlockLength << 1, m_fSampleRate));
    }


    CBlockAudioIf::destroy(m_pCBlockAudio);

    delete[] m_pfBlock;
    delete[] m_pfAudio;

}

TEST_CASE("ToolsCcf", "[ToolsCcf]")
{

    CCcf* m_pCCcf = 0;
    float* m_pfInput = 0,
        * m_pfOut = 0;
    int m_iNumValues = 1024;

    m_pCCcf = new CCcf();
    m_pfInput = new float[m_iNumValues];
    m_pfOut = new float[2 * m_iNumValues];

    CVectorFloat::setZero(m_pfInput, m_iNumValues);
    CVectorFloat::setZero(m_pfOut, 2 * m_iNumValues);

    SECTION("Api")
    {
        // not initialized
        CHECK(Error_t::kFunctionInvalidArgsError ==m_pCCcf->init(-1));
        CHECK(Error_t::kFunctionInvalidArgsError ==m_pCCcf->init(0));
        CHECK(Error_t::kFunctionIllegalCallError ==m_pCCcf->compCcf(m_pfInput, m_pfInput));
        CHECK(-1 == m_pCCcf->getCcfLength());
        CHECK(Error_t::kFunctionIllegalCallError ==m_pCCcf->getCcf(m_pfOut));
        CHECK(-1 == m_pCCcf->getCcfMax());
        CHECK(-1 == m_pCCcf->getCcfMaxIdx());

        // initialized
        CHECK(Error_t::kNoError == m_pCCcf->init(m_iNumValues));
        CHECK(2 * m_iNumValues - 1 == m_pCCcf->getCcfLength());
        CHECK(Error_t::kFunctionIllegalCallError ==m_pCCcf->getCcf(m_pfOut));
        CHECK(-1 == m_pCCcf->getCcfMax());
        CHECK(-1 == m_pCCcf->getCcfMaxIdx());

        CHECK(Error_t::kFunctionInvalidArgsError ==m_pCCcf->compCcf(0, m_pfInput));
        CHECK(Error_t::kFunctionInvalidArgsError ==m_pCCcf->compCcf(m_pfInput, 0));
        CHECK(Error_t::kNoError == m_pCCcf->compCcf(m_pfInput, m_pfInput));

        CHECK(Error_t::kNoError == m_pCCcf->reset());
    }

    SECTION("Acf")
    {
        // zero
        CHECK(Error_t::kNoError == m_pCCcf->init(6));
        CHECK(Error_t::kNoError == m_pCCcf->compCcf(m_pfInput, m_pfInput));
        CHECK(11 == m_pCCcf->getCcfLength());
        CHECK(Error_t::kNoError == m_pCCcf->getCcf(m_pfOut));
        CHECK(0 == CVectorFloat::getSum(m_pfOut, 6));
        CHECK(0 == m_pCCcf->getCcfMax());
        CHECK(0 == m_pCCcf->getCcfMaxIdx());

        // dc input
        CVectorFloat::setValue(m_pfInput, 1.F, 16);
        CHECK(Error_t::kNoError == m_pCCcf->init(16));
        CHECK(Error_t::kNoError == m_pCCcf->compCcf(m_pfInput, m_pfInput, false));
        CHECK(16 == m_pCCcf->getCcfLength(true));
        CHECK(Error_t::kNoError == m_pCCcf->getCcf(m_pfOut, true));
        CHECK(16.F == Approx(m_pCCcf->getCcfMax(true)).margin(1e-6F).epsilon(1e-6F));
        CHECK(16.F == Approx(m_pCCcf->getCcfMax(false)).margin(1e-6F).epsilon(1e-6F));
        CHECK(0 == m_pCCcf->getCcfMaxIdx(true));
        CHECK(16 - 1 == Approx(m_pCCcf->getCcfMaxIdx(false)).margin(1e-6F).epsilon(1e-6F));

        // normalized
        CHECK(Error_t::kNoError == m_pCCcf->compCcf(m_pfInput, m_pfInput, true));
        CHECK(16 == m_pCCcf->getCcfLength(true));
        CHECK(Error_t::kNoError == m_pCCcf->getCcf(m_pfOut, true));
        CHECK(1.F == Approx(m_pCCcf->getCcfMax(true)).margin(1e-6F).epsilon(1e-6F));
        CHECK(0 == m_pCCcf->getCcfMaxIdx(true));

        // sine wave
        CSynthesis::genSine(m_pfInput, 4, 512, m_iNumValues);
        CHECK(Error_t::kNoError == m_pCCcf->init(m_iNumValues));
        CHECK(Error_t::kNoError == m_pCCcf->compCcf(m_pfInput, m_pfInput, false));
        CHECK(m_iNumValues == m_pCCcf->getCcfLength(true));
        CHECK(Error_t::kNoError == m_pCCcf->getCcf(m_pfOut, true));
        CHECK(m_iNumValues / 2 == Approx(m_pCCcf->getCcfMax()).margin(1e-6F).epsilon(1e-6F));
        CHECK(m_iNumValues - 1 ==  m_pCCcf->getCcfMaxIdx());

        // normalized
        CHECK(Error_t::kNoError == m_pCCcf->compCcf(m_pfInput, m_pfInput, true));
        CHECK(Error_t::kNoError == m_pCCcf->getCcf(m_pfOut, true));
        CHECK(1.F == Approx(m_pCCcf->getCcfMax()).margin(1e-6F).epsilon(1e-6F));

        // local maximum
        CHECK((1024 - 128) / 1024.F == Approx(m_pfOut[128]).margin(1e-6F).epsilon(1e-6F));
        CHECK(m_pfOut[128] > m_pfOut[127]);
        CHECK(m_pfOut[128] > m_pfOut[129]);
    }
    
    SECTION("Ccf")
    {
        int iBlockLength = 8;

        // sine wave w impulse
        CSynthesis::genSine(m_pfInput, 1, 1.F*iBlockLength, iBlockLength);
        m_pfInput[iBlockLength] = 1;
        CHECK(Error_t::kNoError == m_pCCcf->init(iBlockLength));
        CHECK(Error_t::kNoError == m_pCCcf->compCcf(m_pfInput, &m_pfInput[iBlockLength], false));
        CHECK(2 * iBlockLength - 1 == m_pCCcf->getCcfLength(false));
        CHECK(Error_t::kNoError == m_pCCcf->getCcf(m_pfOut, false));
        for (auto i = 0; i < iBlockLength; i++)
            CHECK(m_pfInput[i] == Approx(m_pfOut[iBlockLength - 1+i]).margin(1e-3F).epsilon(1e-3F));

        // impulse w sine wave 
        CSynthesis::genSine(m_pfInput, 1, 1.F * iBlockLength, iBlockLength);
        m_pfInput[iBlockLength] = 1;
        CHECK(Error_t::kNoError == m_pCCcf->init(iBlockLength));
        CHECK(Error_t::kNoError == m_pCCcf->compCcf(&m_pfInput[iBlockLength], m_pfInput, false));
        CHECK(2 * iBlockLength - 1 == m_pCCcf->getCcfLength(false));
        CHECK(Error_t::kNoError == m_pCCcf->getCcf(m_pfOut, false));

        for (int eta = 0, i = iBlockLength - 1; eta < iBlockLength; eta++, i--)
            CHECK(m_pfOut[eta] == Approx(m_pfInput[i]).margin(1e-6F).epsilon(1e-6F));
    }

    delete m_pCCcf;

    delete[] m_pfInput;
    delete[] m_pfOut;
}

TEST_CASE("ToolsConversion", "[ToolsConversion]")
{
    float* m_pfMel = 0,
        * m_pfFreq = 0,
        * m_pfOut = 0;
    int m_iNumValues = 1024;

    m_pfMel = new float[m_iNumValues];
    m_pfFreq = new float[m_iNumValues];
    m_pfOut = new float[m_iNumValues];

    for (auto m = 0; m < m_iNumValues; m++)
        m_pfMel[m] = static_cast<float>(m);

    SECTION("Freq2Mel2Freq")
    {
        // Mel (Fant)
        CHECK(1000.F == Approx(CConversion::convertFreq2Mel(1000.F, CConversion::kFant)).margin(1e-6F).epsilon(1e-6F));
        CHECK(1000.F == Approx(CConversion::convertMel2Freq(1000.F)).margin(1e-6F).epsilon(1e-6F));

        CConversion::convertMel2Freq(m_pfFreq, m_pfMel, m_iNumValues, CConversion::kFant);
        CConversion::convertFreq2Mel(m_pfOut, m_pfFreq, m_iNumValues, CConversion::kFant);

        for (auto i = 0; i < m_iNumValues; i++)
            CHECK(m_pfMel[i] == Approx(m_pfOut[i]).margin(1e-3F).epsilon(1e-3F));

        // Mel (Shaughnessy)
        CHECK(1000.F == Approx(CConversion::convertFreq2Mel(1000.F, CConversion::kShaughnessy)).margin(1e-1F).epsilon(1e-1F));
        CHECK(1000.F == Approx(CConversion::convertMel2Freq(1000.F, CConversion::kShaughnessy)).margin(1e-1F).epsilon(1e-1F));

        CConversion::convertMel2Freq(m_pfFreq, m_pfMel, m_iNumValues, CConversion::kShaughnessy);
        CConversion::convertFreq2Mel(m_pfOut, m_pfFreq, m_iNumValues, CConversion::kShaughnessy);

        for (auto i = 0; i < m_iNumValues; i++)
            CHECK(m_pfMel[i] == Approx(m_pfOut[i]).margin(1e-3F).epsilon(1e-3F));

        // Mel (Umesh)
        CHECK(CConversion::convertFreq2Mel(1000.F, CConversion::kUmesh) - 1000.F <= 25.F);
        CHECK(1000.F - CConversion::convertMel2Freq(1000.F, CConversion::kUmesh) <= 25.F);

        CConversion::convertMel2Freq(m_pfFreq, m_pfMel, m_iNumValues, CConversion::kUmesh);
        CConversion::convertFreq2Mel(m_pfOut, m_pfFreq, m_iNumValues, CConversion::kUmesh);

        for (auto i = 0; i < m_iNumValues; i++)
            CHECK(m_pfMel[i] == Approx(m_pfOut[i]).margin(1e-3F).epsilon(1e-3F));
    }

    SECTION("Freq2Bark2Freq")
    {
        for (auto m = 1; m < m_iNumValues; m++)
            m_pfMel[m] = m * 20.F /m_iNumValues;
        m_pfMel[0] = m_pfMel[1];

        // Bark (Schroeder)
        CHECK(8.51137148802024F == Approx(CConversion::convertFreq2Bark(1000.F, CConversion::kSchroeder)).margin(1e-6F).epsilon(1e-6F));
        CHECK(1000.F == Approx(CConversion::convertBark2Freq(8.51137148802024F, CConversion::kSchroeder)).margin(1e-6F).epsilon(1e-6F));

        CConversion::convertBark2Freq(m_pfFreq, m_pfMel, m_iNumValues, CConversion::kSchroeder);
        CConversion::convertFreq2Bark(m_pfOut, m_pfFreq, m_iNumValues, CConversion::kSchroeder);

        for (auto i = 0; i < m_iNumValues; i++)
            CHECK(m_pfMel[i] == Approx(m_pfOut[i]).margin(1e-3F).epsilon(1e-3F));

        // Bark (kTerhardt)
        CHECK(8.55856474695068F == Approx(CConversion::convertFreq2Bark(1000.F, CConversion::kTerhardt)).margin(1e-1F).epsilon(1e-1F));
        CHECK(1000.F == Approx(CConversion::convertBark2Freq(8.55856474695068F, CConversion::kTerhardt)).margin(1e-1F).epsilon(1e-1F));

        CConversion::convertBark2Freq(m_pfFreq, m_pfMel, 650, CConversion::kTerhardt);
        CConversion::convertFreq2Bark(m_pfOut, m_pfFreq, 650, CConversion::kTerhardt);

        for (auto i = 0; i < 650; i++)
            CHECK(m_pfMel[i] == Approx(m_pfOut[i]).margin(1e-3F).epsilon(1e-3F));

        // Bark (kTraunmuller)
        CHECK(8.52743243243243F == Approx(CConversion::convertFreq2Bark(1000.F, CConversion::kTraunmuller)).margin(1e-1F).epsilon(1e-1F));
        CHECK(1000.F == Approx( CConversion::convertBark2Freq(8.52743243243243F, CConversion::kTraunmuller)).margin(1e-1F).epsilon(1e-1F));

        CConversion::convertBark2Freq(m_pfFreq, m_pfMel, m_iNumValues, CConversion::kTraunmuller);
        CConversion::convertFreq2Bark(m_pfOut, m_pfFreq, m_iNumValues, CConversion::kTraunmuller);

        for (auto i = 0; i < m_iNumValues; i++)
            CHECK(m_pfMel[i] == Approx(m_pfOut[i]).margin(1e-3F).epsilon(1e-3F));

        // Bark (Zwicker)
        CHECK(8.91224620539368F == Approx(CConversion::convertFreq2Bark(1000.F, CConversion::kZwicker)).margin(1e-1F).epsilon(1e-1F));
    }

    SECTION("Freq2Midi2Freq")
    {
        CHECK(69.F == Approx(CConversion::convertFreq2Midi(440.F)).margin(1e-6F).epsilon(1e-6F));
        CHECK(57.F == Approx(CConversion::convertFreq2Midi(440.F, 880.F)).margin(1e-6F).epsilon(1e-6F));
        CHECK(81.F == Approx(CConversion::convertFreq2Midi(440.F, 220.F)).margin(1e-6F).epsilon(1e-6F));
        CHECK(70.F == Approx(CConversion::convertFreq2Midi(440.F * 1.0594630943593F)).margin(1e-6F).epsilon(1e-6F));

        CHECK(440.F == Approx(CConversion::convertMidi2Freq(69.F)).margin(1e-6F).epsilon(1e-6F));
        CHECK(440.F == Approx(CConversion::convertMidi2Freq(57.F, 880.F)).margin(1e-6F).epsilon(1e-6F));
        CHECK(440.F == Approx(CConversion::convertMidi2Freq(81.F, 220.F)).margin(1e-6F).epsilon(1e-6F));
        CHECK(440.F == Approx(CConversion::convertMidi2Freq(70.F) / 1.0594630943593F).margin(1e-6F).epsilon(1e-6F));

        CConversion::convertMidi2Freq(m_pfFreq, m_pfMel, 128);
        CConversion::convertFreq2Midi(m_pfOut, m_pfFreq, 128);

        for (auto i = 0; i < 128; i++)
            CHECK(m_pfMel[i] == Approx(m_pfOut[i]).margin(1e-3F).epsilon(1e-3F));
    }

    SECTION("Freq2Bin2Freq")
    {
        float fSampleRate = 48000.f;
        int iFftLength = 16;

        CHECK(0.F == Approx(CConversion::convertFreq2Bin(0.F, iFftLength, fSampleRate)).margin(1e-6F).epsilon(1e-6F));
        CHECK(iFftLength / 2.F == Approx(CConversion::convertFreq2Bin(fSampleRate / 2, iFftLength, fSampleRate)).margin(1e-6F).epsilon(1e-6F));
        CHECK(1.F == Approx(CConversion::convertFreq2Bin(fSampleRate / iFftLength, iFftLength, fSampleRate)).margin(1e-6F).epsilon(1e-6F));

        CHECK(0.F == Approx(CConversion::convertBin2Freq(0.F, iFftLength, fSampleRate)).margin(1e-6F).epsilon(1e-6F));
        CHECK(fSampleRate / 2 == Approx(CConversion::convertBin2Freq(iFftLength / 2.F, iFftLength, fSampleRate)).margin(1e-6F).epsilon(1e-6F));
        CHECK(fSampleRate / iFftLength == Approx(CConversion::convertBin2Freq(1.F, iFftLength, fSampleRate)).margin(1e-6F).epsilon(1e-6F));

        CConversion::convertBin2Freq(m_pfFreq, m_pfMel, iFftLength, iFftLength, fSampleRate);
        CConversion::convertFreq2Bin(m_pfOut, m_pfFreq, iFftLength, iFftLength, fSampleRate);

        for (auto i = 0; i < iFftLength; i++)
            CHECK(m_pfMel[i] == Approx(m_pfOut[i]).margin(1e-3F).epsilon(1e-3F));
    }

    delete[] m_pfMel;
    delete[] m_pfFreq;
    delete[] m_pfOut;
}

TEST_CASE("ToolsMovingAverage", "[ToolsMovingAverage]")
{
    CMovingAverage* m_pCLowPass = 0;
    float* m_pfInput = 0,
        * m_pfOut = 0;
    int m_iNumValues = 1024;

    CMovingAverage::create(m_pCLowPass);
    m_pfInput = new float[m_iNumValues];
    m_pfOut = new float[2 * m_iNumValues];

    CVectorFloat::setZero(m_pfInput, m_iNumValues);
    CVectorFloat::setZero(m_pfOut, m_iNumValues);

    SECTION("Api")
    {
        CHECK(Error_t::kFunctionInvalidArgsError ==m_pCLowPass->setFilterParam(0));
        CHECK(Error_t::kFunctionInvalidArgsError ==m_pCLowPass->setFilterParam(-5));

        CHECK(Error_t::kNoError == m_pCLowPass->reset());

        CHECK(Error_t::kNoError == m_pCLowPass->setFilterParam(5));
        CHECK(5.F == Approx(m_pCLowPass->getFilterParam()).margin(1e-6F).epsilon(1e-6F));
        CHECK(Error_t::kNoError == m_pCLowPass->setFilterParam(1000));
        CHECK(1000.F == Approx(m_pCLowPass->getFilterParam()).margin(1e-6F).epsilon(1e-6F));
        CHECK(Error_t::kNoError == m_pCLowPass->setFilterParam(1));
        CHECK(1.F == Approx(m_pCLowPass->getFilterParam()).margin(1e-6F).epsilon(1e-6F));

        CHECK(Error_t::kFunctionInvalidArgsError ==m_pCLowPass->process(0, m_pfInput, m_iNumValues));
        CHECK(Error_t::kFunctionInvalidArgsError ==m_pCLowPass->process(m_pfOut, 0, m_iNumValues));
        CHECK(Error_t::kFunctionInvalidArgsError ==m_pCLowPass->process(m_pfOut, m_pfInput, 0));
        CHECK(Error_t::kNoError == m_pCLowPass->process(m_pfOut, m_pfInput, m_iNumValues));
    }

    SECTION("Process")
    {
        // zeros
        for (auto i = 0; i < m_iNumValues; i++)
        {
            CHECK(Error_t::kNoError == m_pCLowPass->process(&m_pfOut[i], &m_pfInput[i], 1));
            CHECK(0.F == Approx(m_pfOut[i]).margin(1e-6F).epsilon(1e-6F));
        }

        // ones
        CVectorFloat::setValue(m_pfInput, 1.F, m_iNumValues);
        m_pCLowPass->reset();
        m_pCLowPass->setFilterParam(1);
        CHECK(Error_t::kNoError == m_pCLowPass->process(m_pfOut, m_pfInput, m_iNumValues));
        for (auto i = 0; i < m_iNumValues; i++)
            CHECK(m_pfInput[i] == Approx(m_pfOut[i]).margin(1e-6F).epsilon(1e-6F));

        for (auto c = 0; c < 10; c++)
        {
            int iLength = c * 10 + 2;
            m_pCLowPass->reset();
            m_pCLowPass->setFilterParam(iLength);
            for (auto i = 0; i < iLength; i++)
            {
                CHECK(Error_t::kNoError == m_pCLowPass->process(&m_pfOut[i], &m_pfInput[i], 1));
                CHECK((i + 1.F) / iLength == Approx(m_pfOut[i]).margin(1e-6F).epsilon(1e-6F));
            }
        }
    }

    SECTION("Filtfilt")
    {
        int iSignalLength = 16;
        int iFilterLength = 5;
        int iMaxIdx = 6;
        m_pCLowPass->setFilterParam(iFilterLength);

        m_pfInput[iMaxIdx] = 1;

        m_pCLowPass->filtfilt(m_pfOut, m_pfInput, iSignalLength);
        
        CHECK(.2F == Approx(CVectorFloat::getMax(m_pfOut, iSignalLength)).margin(1e-6F).epsilon(1e-6F));
        CHECK(.2F == Approx(m_pfOut[iMaxIdx]).margin(1e-6F).epsilon(1e-6F));
        CHECK(.0F == Approx(CVectorFloat::getMin(m_pfOut, iSignalLength)).margin(1e-6F).epsilon(1e-6F));

        for (auto i = iMaxIdx-iFilterLength+1; i <= iMaxIdx; i++)
            CHECK(m_pfOut[i - 1] < m_pfOut[i]);
        for (auto i = iMaxIdx; i < iMaxIdx + iFilterLength-1; i++)
            CHECK(m_pfOut[i + 1] < m_pfOut[i]);

        iFilterLength = 4;
        m_pCLowPass->setFilterParam(iFilterLength);

        m_pCLowPass->filtfilt(m_pfOut, m_pfInput, iSignalLength);

        CHECK(.25F == Approx(CVectorFloat::getMax(m_pfOut, iSignalLength)).margin(1e-6F).epsilon(1e-6F));
        CHECK(.25F == Approx(m_pfOut[iMaxIdx]).margin(1e-6F).epsilon(1e-6F));
        CHECK(.0F == Approx(CVectorFloat::getMin(m_pfOut, iSignalLength)).margin(1e-6F).epsilon(1e-6F));

        for (auto i = iMaxIdx - iFilterLength + 1; i <= iMaxIdx; i++)
            CHECK(m_pfOut[i - 1] < m_pfOut[i]);
        for (auto i = iMaxIdx; i < iMaxIdx + iFilterLength - 1; i++)
            CHECK(m_pfOut[i + 1] < m_pfOut[i]);


        //DC input
        iFilterLength = 3;
        m_pCLowPass->setFilterParam(iFilterLength);
        CVectorFloat::setValue(m_pfInput, 1.F, iSignalLength);
        m_pCLowPass->filtfilt(m_pfOut, m_pfInput, iSignalLength);
        CHECK(1.F == Approx(CVectorFloat::getMax(m_pfOut, iSignalLength)).margin(1e-6F).epsilon(1e-6F));
        CHECK(1.F == Approx(CVectorFloat::getMean(&m_pfOut[iFilterLength - 1], iSignalLength - 2 * (iFilterLength - 1))).margin(1e-6F).epsilon(1e-6F));
    }

    CMovingAverage::destroy(m_pCLowPass);

    delete[] m_pfInput;
    delete[] m_pfOut;
}

TEST_CASE("ToolsSinglePole", "[ToolsSinglePole]")
{

    CSinglePoleLp* m_pCLowPass = 0;
    float* m_pfInput = 0,
        * m_pfOut = 0;
    int m_iNumValues = 1024;

    CSinglePoleLp::create(m_pCLowPass);
    m_pfInput = new float[m_iNumValues];
    m_pfOut = new float[2 * m_iNumValues];

    CVectorFloat::setZero(m_pfInput, m_iNumValues);
    CVectorFloat::setZero(m_pfOut, m_iNumValues);

    SECTION("Api")
    {
        CHECK(Error_t::kFunctionInvalidArgsError ==m_pCLowPass->setFilterParam(-1.F));
        CHECK(Error_t::kFunctionInvalidArgsError ==m_pCLowPass->setFilterParam(1.F));
        CHECK(Error_t::kFunctionInvalidArgsError ==m_pCLowPass->setFilterParam(1.1F));

        CHECK(Error_t::kNoError == m_pCLowPass->reset());

        CHECK(Error_t::kNoError == m_pCLowPass->setFilterParam(.01F));
        CHECK(.01F == Approx(m_pCLowPass->getFilterParam()).margin(1e-6F).epsilon(1e-6F));
        CHECK(Error_t::kNoError == m_pCLowPass->setFilterParam(.5F));
        CHECK(.5F == Approx(m_pCLowPass->getFilterParam()).margin(1e-6F).epsilon(1e-6F));
        CHECK(Error_t::kNoError == m_pCLowPass->setFilterParam(.99F));
        CHECK(.99F == Approx(m_pCLowPass->getFilterParam()).margin(1e-6F).epsilon(1e-6F));

        CHECK(Error_t::kFunctionInvalidArgsError ==m_pCLowPass->process(0, m_pfInput, m_iNumValues));
        CHECK(Error_t::kFunctionInvalidArgsError ==m_pCLowPass->process(m_pfOut, 0, m_iNumValues));
        CHECK(Error_t::kFunctionInvalidArgsError ==m_pCLowPass->process(m_pfOut, m_pfInput, 0));
        CHECK(Error_t::kNoError == m_pCLowPass->process(m_pfOut, m_pfInput, m_iNumValues));
    }

    SECTION("Process")
    {
        // zeros
        for (auto i = 0; i < m_iNumValues; i++)
        {
            CHECK(Error_t::kNoError == m_pCLowPass->process(&m_pfOut[i], &m_pfInput[i], 1));
            CHECK(0.F == Approx(m_pfOut[i]).margin(1e-6F).epsilon(1e-6F));
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
                CHECK(Error_t::kNoError == m_pCLowPass->process(&m_pfOut[i], &m_pfInput[i], 1));
                CHECK(1.F - std::pow(m_pCLowPass->getFilterParam(), i + 1) == Approx(m_pfOut[i]).margin(1e-6F).epsilon(1e-6F));
            }
        }
        CHECK(0 < CSinglePoleLp::calcFilterParam(0.1F, 48000));
        CHECK(0.F == Approx(CSinglePoleLp::calcFilterParam(0, 48000)).margin(1e-3F).epsilon(1e-3F));
        CHECK(1.F == Approx(0 < CSinglePoleLp::calcFilterParam(1000000, 48000)).margin(1e-6F).epsilon(1e-6F));

    }
    CSinglePoleLp::destroy(m_pCLowPass);

    delete[] m_pfInput;
    delete[] m_pfOut;

}

#endif //WITH_TESTS
