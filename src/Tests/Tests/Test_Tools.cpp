#include "AcaAll.h"

#ifdef WITH_TESTS

#include "Synthesis.h"
#include "Vector.h"
#include "Matrix.h"

#include "catch.hpp"


TEST_CASE("ToolsBlockAudio", "[ToolsBlockAudio]")
{
    CBlockAudioIf* pCBlockAudio = 0;

    float* pfAudio = 0;
    float* pfBlock = 0;

    float fSampleRate = 0;

    int iBlockLength = 0,
        iHopLength = 0,
        iAudioLength = 0,
        iBufferLength = 40000;

    pfAudio = new float[iBufferLength];
    pfBlock = new float[1024];
    for (auto i = 0; i < iBufferLength; i++)
        pfAudio[i] = static_cast<float>(i);

    SECTION("Dimensions")
    {
        iBlockLength = 20;
        iHopLength = 10;
        fSampleRate = 1;
        iAudioLength = 101;

        CBlockAudioIf::create(pCBlockAudio, pfAudio, iAudioLength, iBlockLength, iHopLength, fSampleRate);

        CHECK(pCBlockAudio->getNumBlocks() == iAudioLength / iHopLength + 1);

        CBlockAudioIf::destroy(pCBlockAudio);

        iBlockLength = 1024;
        iHopLength = 512;
        fSampleRate = 40000;
        iAudioLength = 40000;

        CBlockAudioIf::create(pCBlockAudio, pfAudio, iAudioLength, iBlockLength, iHopLength, fSampleRate);

        CHECK(79 == pCBlockAudio->getNumBlocks());
    }

    SECTION("Content")
    {
        iBlockLength = 20;
        iHopLength = 10;
        fSampleRate = 1;
        iAudioLength = 101;

        CBlockAudioIf::create(pCBlockAudio, pfAudio, iAudioLength, iBlockLength, iHopLength, fSampleRate);

        // check block 9
        float fTestTimeStamp = 0,
            fTargetTimeStamp = iBlockLength / fSampleRate * .5F;
        int iBlockIdx = 9;
        for (auto n = 0; n <= iBlockIdx; n++)
        {
            pCBlockAudio->getNextBlock(pfBlock, &fTestTimeStamp);
            CHECK(fTestTimeStamp == Approx(fTargetTimeStamp).margin(1e-6F).epsilon(1e-6F));
            fTargetTimeStamp += iHopLength / fSampleRate;
            CHECK_FALSE( pCBlockAudio->IsEndOfData());
        }

        for (auto i = 0; i < iHopLength + 1; i++)
            CHECK(pfBlock[i] == iBlockIdx* iHopLength + i);
    }


    SECTION("Api")
    {
        iBlockLength = 20;
        iHopLength = 10;
        fSampleRate = 1;
        iAudioLength = 101;

        CHECK_FALSE(Error_t::kNoError == CBlockAudioIf::create(pCBlockAudio, 0, iAudioLength, iBlockLength, iHopLength, fSampleRate));
        CHECK_FALSE(Error_t::kNoError == CBlockAudioIf::create(pCBlockAudio, pfAudio, 0, iBlockLength, iHopLength, fSampleRate));
        CHECK_FALSE(Error_t::kNoError == CBlockAudioIf::create(pCBlockAudio, pfAudio, iAudioLength, 0, iHopLength, fSampleRate));
        CHECK_FALSE(Error_t::kNoError == CBlockAudioIf::create(pCBlockAudio, pfAudio, iAudioLength, iBlockLength, 0, fSampleRate));
        CHECK_FALSE(Error_t::kNoError == CBlockAudioIf::create(pCBlockAudio, pfAudio, iAudioLength, iBlockLength, iHopLength, 0));
        CHECK_FALSE(Error_t::kNoError == CBlockAudioIf::create(pCBlockAudio, pfAudio, -1, iBlockLength, iHopLength, fSampleRate));
        CHECK_FALSE(Error_t::kNoError == CBlockAudioIf::create(pCBlockAudio, pfAudio, iAudioLength, -1, iHopLength, fSampleRate));
        CHECK_FALSE(Error_t::kNoError == CBlockAudioIf::create(pCBlockAudio, pfAudio, iAudioLength, iBlockLength, -1, fSampleRate));
        CHECK_FALSE(Error_t::kNoError == CBlockAudioIf::create(pCBlockAudio, pfAudio, iAudioLength, iBlockLength, iHopLength, -1));
        CHECK_FALSE(Error_t::kNoError == CBlockAudioIf::create(pCBlockAudio, pfAudio, iAudioLength, iBlockLength, iBlockLength << 1, fSampleRate));
    }


    CBlockAudioIf::destroy(pCBlockAudio);

    delete[] pfBlock;
    delete[] pfAudio;

}

TEST_CASE("ToolsCcf", "[ToolsCcf]")
{

    CCcf* pCCcf = 0;
    float* pfInput = 0,
        * pfOut = 0;
    int iNumValues = 1024;

    pCCcf = new CCcf();
    pfInput = new float[iNumValues];
    pfOut = new float[2 * iNumValues];

    CVector::setZero(pfInput, iNumValues);
    CVector::setZero(pfOut, 2 * static_cast<long long>(iNumValues));

    SECTION("Api")
    {
        // not initialized
        CHECK(Error_t::kFunctionInvalidArgsError ==pCCcf->init(-1));
        CHECK(Error_t::kFunctionInvalidArgsError ==pCCcf->init(0));
        CHECK(Error_t::kFunctionIllegalCallError ==pCCcf->compCcf(pfInput, pfInput));
        CHECK(-1 == pCCcf->getCcfLength());
        CHECK(Error_t::kFunctionIllegalCallError ==pCCcf->getCcf(pfOut));
        CHECK(-1 == pCCcf->getCcfMax());
        CHECK(-1 == pCCcf->getCcfMaxIdx());

        // initialized
        CHECK(Error_t::kNoError == pCCcf->init(iNumValues));
        CHECK(2 * iNumValues - 1 == pCCcf->getCcfLength());
        CHECK(Error_t::kFunctionIllegalCallError ==pCCcf->getCcf(pfOut));
        CHECK(-1 == pCCcf->getCcfMax());
        CHECK(-1 == pCCcf->getCcfMaxIdx());

        CHECK(Error_t::kFunctionInvalidArgsError ==pCCcf->compCcf(0, pfInput));
        CHECK(Error_t::kFunctionInvalidArgsError ==pCCcf->compCcf(pfInput, 0));
        CHECK(Error_t::kNoError == pCCcf->compCcf(pfInput, pfInput));

        CHECK(Error_t::kNoError == pCCcf->reset());
    }

    SECTION("Acf")
    {
        // zero
        CHECK(Error_t::kNoError == pCCcf->init(6));
        CHECK(Error_t::kNoError == pCCcf->compCcf(pfInput, pfInput));
        CHECK(11 == pCCcf->getCcfLength());
        CHECK(Error_t::kNoError == pCCcf->getCcf(pfOut));
        CHECK(0 == CVector::getSum(pfOut, 6));
        CHECK(0 == pCCcf->getCcfMax());
        CHECK(0 == pCCcf->getCcfMaxIdx());

        // dc input
        CVector::setValue(pfInput, 1.F, 16);
        CHECK(Error_t::kNoError == pCCcf->init(16));
        CHECK(Error_t::kNoError == pCCcf->compCcf(pfInput, pfInput, false));
        CHECK(16 == pCCcf->getCcfLength(true));
        CHECK(Error_t::kNoError == pCCcf->getCcf(pfOut, true));
        CHECK(16.F == Approx(pCCcf->getCcfMax(true)).margin(1e-6F).epsilon(1e-6F));
        CHECK(16.F == Approx(pCCcf->getCcfMax(false)).margin(1e-6F).epsilon(1e-6F));
        CHECK(0 == pCCcf->getCcfMaxIdx(true));
        CHECK(16 - 1 == Approx(pCCcf->getCcfMaxIdx(false)).margin(1e-6F).epsilon(1e-6F));

        // normalized
        CHECK(Error_t::kNoError == pCCcf->compCcf(pfInput, pfInput, true));
        CHECK(16 == pCCcf->getCcfLength(true));
        CHECK(Error_t::kNoError == pCCcf->getCcf(pfOut, true));
        CHECK(1.F == Approx(pCCcf->getCcfMax(true)).margin(1e-6F).epsilon(1e-6F));
        CHECK(0 == pCCcf->getCcfMaxIdx(true));

        // sine wave
        CSynthesis::genSine<float>(pfInput, 4, 512, iNumValues);
        CHECK(Error_t::kNoError == pCCcf->init(iNumValues));
        CHECK(Error_t::kNoError == pCCcf->compCcf(pfInput, pfInput, false));
        CHECK(iNumValues == pCCcf->getCcfLength(true));
        CHECK(Error_t::kNoError == pCCcf->getCcf(pfOut, true));
        CHECK(iNumValues / 2 == Approx(pCCcf->getCcfMax()).margin(1e-6F).epsilon(1e-6F));
        CHECK(iNumValues - 1 ==  pCCcf->getCcfMaxIdx());

        // normalized
        CHECK(Error_t::kNoError == pCCcf->compCcf(pfInput, pfInput, true));
        CHECK(Error_t::kNoError == pCCcf->getCcf(pfOut, true));
        CHECK(1.F == Approx(pCCcf->getCcfMax()).margin(1e-6F).epsilon(1e-6F));

        // local maximum
        CHECK((1024 - 128) / 1024.F == Approx(pfOut[128]).margin(1e-6F).epsilon(1e-6F));
        CHECK(pfOut[128] > pfOut[127]);
        CHECK(pfOut[128] > pfOut[129]);
    }
    
    SECTION("Ccf")
    {
        int iBlockLength = 8;

        // sine wave w impulse
        CSynthesis::genSine<float>(pfInput, 1, 1.F*iBlockLength, iBlockLength);
        pfInput[iBlockLength] = 1;
        CHECK(Error_t::kNoError == pCCcf->init(iBlockLength));
        CHECK(Error_t::kNoError == pCCcf->compCcf(pfInput, &pfInput[iBlockLength], false));
        CHECK(2 * iBlockLength - 1 == pCCcf->getCcfLength(false));
        CHECK(Error_t::kNoError == pCCcf->getCcf(pfOut, false));
        for (auto i = 0; i < iBlockLength; i++)
            CHECK(pfInput[i] == Approx(pfOut[iBlockLength - 1+i]).margin(1e-3F).epsilon(1e-3F));

        // impulse w sine wave 
        CSynthesis::genSine<float>(pfInput, 1, 1.F * iBlockLength, iBlockLength);
        pfInput[iBlockLength] = 1;
        CHECK(Error_t::kNoError == pCCcf->init(iBlockLength));
        CHECK(Error_t::kNoError == pCCcf->compCcf(&pfInput[iBlockLength], pfInput, false));
        CHECK(2 * iBlockLength - 1 == pCCcf->getCcfLength(false));
        CHECK(Error_t::kNoError == pCCcf->getCcf(pfOut, false));

        for (int eta = 0, i = iBlockLength - 1; eta < iBlockLength; eta++, i--)
            CHECK(pfOut[eta] == Approx(pfInput[i]).margin(1e-6F).epsilon(1e-6F));
    }

    delete pCCcf;

    delete[] pfInput;
    delete[] pfOut;
}

TEST_CASE("ToolsConversion", "[ToolsConversion]")
{
    float* pfMel = 0,
        * pfFreq = 0,
        * pfOut = 0;
    int iNumValues = 1024;

    pfMel = new float[iNumValues];
    pfFreq = new float[iNumValues];
    pfOut = new float[iNumValues];

    for (auto m = 0; m < iNumValues; m++)
        pfMel[m] = static_cast<float>(m);

    SECTION("Freq2Mel2Freq")
    {
        // Mel (Fant)
        CHECK(1000.F == Approx(CConversion::convertFreq2Mel(1000.F, CConversion::kFant)).margin(1e-6F).epsilon(1e-6F));
        CHECK(1000.F == Approx(CConversion::convertMel2Freq(1000.F)).margin(1e-6F).epsilon(1e-6F));

        CConversion::convertMel2Freq(pfFreq, pfMel, iNumValues, CConversion::kFant);
        CConversion::convertFreq2Mel(pfOut, pfFreq, iNumValues, CConversion::kFant);

        for (auto i = 0; i < iNumValues; i++)
            CHECK(pfMel[i] == Approx(pfOut[i]).margin(1e-3F).epsilon(1e-3F));

        // Mel (Shaughnessy)
        CHECK(1000.F == Approx(CConversion::convertFreq2Mel(1000.F, CConversion::kShaughnessy)).margin(1e-1F).epsilon(1e-1F));
        CHECK(1000.F == Approx(CConversion::convertMel2Freq(1000.F, CConversion::kShaughnessy)).margin(1e-1F).epsilon(1e-1F));

        CConversion::convertMel2Freq(pfFreq, pfMel, iNumValues, CConversion::kShaughnessy);
        CConversion::convertFreq2Mel(pfOut, pfFreq, iNumValues, CConversion::kShaughnessy);

        for (auto i = 0; i < iNumValues; i++)
            CHECK(pfMel[i] == Approx(pfOut[i]).margin(1e-3F).epsilon(1e-3F));

        // Mel (Umesh)
        CHECK(CConversion::convertFreq2Mel(1000.F, CConversion::kUmesh) - 1000.F <= 25.F);
        CHECK(1000.F - CConversion::convertMel2Freq(1000.F, CConversion::kUmesh) <= 25.F);

        CConversion::convertMel2Freq(pfFreq, pfMel, iNumValues, CConversion::kUmesh);
        CConversion::convertFreq2Mel(pfOut, pfFreq, iNumValues, CConversion::kUmesh);

        for (auto i = 0; i < iNumValues; i++)
            CHECK(pfMel[i] == Approx(pfOut[i]).margin(1e-3F).epsilon(1e-3F));
    }

    SECTION("Freq2Bark2Freq")
    {
        for (auto m = 1; m < iNumValues; m++)
            pfMel[m] = m * 20.F /iNumValues;
        pfMel[0] = pfMel[1];

        // Bark (Schroeder)
        CHECK(8.51137148802024F == Approx(CConversion::convertFreq2Bark(1000.F, CConversion::kSchroeder)).margin(1e-6F).epsilon(1e-6F));
        CHECK(1000.F == Approx(CConversion::convertBark2Freq(8.51137148802024F, CConversion::kSchroeder)).margin(1e-6F).epsilon(1e-6F));

        CConversion::convertBark2Freq(pfFreq, pfMel, iNumValues, CConversion::kSchroeder);
        CConversion::convertFreq2Bark(pfOut, pfFreq, iNumValues, CConversion::kSchroeder);

        for (auto i = 0; i < iNumValues; i++)
            CHECK(pfMel[i] == Approx(pfOut[i]).margin(1e-3F).epsilon(1e-3F));

        // Bark (kTerhardt)
        CHECK(8.55856474695068F == Approx(CConversion::convertFreq2Bark(1000.F, CConversion::kTerhardt)).margin(1e-1F).epsilon(1e-1F));
        CHECK(1000.F == Approx(CConversion::convertBark2Freq(8.55856474695068F, CConversion::kTerhardt)).margin(1e-1F).epsilon(1e-1F));

        CConversion::convertBark2Freq(pfFreq, pfMel, 650, CConversion::kTerhardt);
        CConversion::convertFreq2Bark(pfOut, pfFreq, 650, CConversion::kTerhardt);

        for (auto i = 0; i < 650; i++)
            CHECK(pfMel[i] == Approx(pfOut[i]).margin(1e-3F).epsilon(1e-3F));

        // Bark (kTraunmuller)
        CHECK(8.52743243243243F == Approx(CConversion::convertFreq2Bark(1000.F, CConversion::kTraunmuller)).margin(1e-1F).epsilon(1e-1F));
        CHECK(1000.F == Approx( CConversion::convertBark2Freq(8.52743243243243F, CConversion::kTraunmuller)).margin(1e-1F).epsilon(1e-1F));

        CConversion::convertBark2Freq(pfFreq, pfMel, iNumValues, CConversion::kTraunmuller);
        CConversion::convertFreq2Bark(pfOut, pfFreq, iNumValues, CConversion::kTraunmuller);

        for (auto i = 0; i < iNumValues; i++)
            CHECK(pfMel[i] == Approx(pfOut[i]).margin(1e-3F).epsilon(1e-3F));

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

        CConversion::convertMidi2Freq(pfFreq, pfMel, 128);
        CConversion::convertFreq2Midi(pfOut, pfFreq, 128);

        for (auto i = 0; i < 128; i++)
            CHECK(pfMel[i] == Approx(pfOut[i]).margin(1e-3F).epsilon(1e-3F));
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

        CConversion::convertBin2Freq(pfFreq, pfMel, iFftLength, iFftLength, fSampleRate);
        CConversion::convertFreq2Bin(pfOut, pfFreq, iFftLength, iFftLength, fSampleRate);

        for (auto i = 0; i < iFftLength; i++)
            CHECK(pfMel[i] == Approx(pfOut[i]).margin(1e-3F).epsilon(1e-3F));
    }

    delete[] pfMel;
    delete[] pfFreq;
    delete[] pfOut;
}

TEST_CASE("ToolsGammatone", "[ToolsGammatone]")
{
    CGammaToneFbIf* pCGammatone = 0;

    float* pfIn = 0;
    float** ppfOut = 0;

    float fSampleRate = 32000,
        fStartFreq = 100;

    int/* iBlockLength = 0,
        iAudioLength = 0,*/
        iBufferLength = 40000,
        iNumBands = 20;
    long long aiDims[2] = { 0 };

    CVector::alloc(pfIn, iBufferLength);
    CMatrix::alloc(ppfOut, iNumBands, iBufferLength);

    SECTION("Api")
    {
        iBufferLength = 1;
        CHECK(Error_t::kFunctionInvalidArgsError == CGammaToneFbIf::create(pCGammatone, 0, iBufferLength, fSampleRate, iNumBands, fStartFreq));
        CHECK(Error_t::kFunctionInvalidArgsError == CGammaToneFbIf::create(pCGammatone, pfIn, 0, fSampleRate, iNumBands, fStartFreq));
        CHECK(Error_t::kFunctionInvalidArgsError == CGammaToneFbIf::create(pCGammatone, pfIn, iBufferLength, 0, iNumBands, fStartFreq));
        CHECK(Error_t::kFunctionInvalidArgsError == CGammaToneFbIf::create(pCGammatone, pfIn, iBufferLength, fSampleRate, 0, fStartFreq));
        CHECK(Error_t::kFunctionInvalidArgsError == CGammaToneFbIf::create(pCGammatone, pfIn, iBufferLength, fSampleRate, iNumBands, 0));
        CHECK(Error_t::kNoError == CGammaToneFbIf::create(pCGammatone, pfIn, iBufferLength, fSampleRate, iNumBands, fStartFreq));

        CHECK(Error_t::kNoError == pCGammatone->getOutputDimensions(aiDims[0], aiDims[1]));

        CHECK(Error_t::kFunctionInvalidArgsError == pCGammatone->process(0));
        CHECK(Error_t::kNoError == pCGammatone->process(ppfOut));

        CHECK(Error_t::kNoError == CGammaToneFbIf::destroy(pCGammatone));
    }

    SECTION("ZeroInput")
    {
        CHECK(Error_t::kNoError == CGammaToneFbIf::create(pCGammatone, pfIn, iBufferLength, fSampleRate, iNumBands, fStartFreq));

        CHECK(Error_t::kNoError == pCGammatone->getOutputDimensions(aiDims[0], aiDims[1]));
        CHECK(iNumBands == aiDims[0]);
        CHECK(iBufferLength == aiDims[1]);

        CHECK(Error_t::kNoError == pCGammatone->process(ppfOut));

        for (auto c = 0; c < iNumBands; c++)
            CHECK(0.F == CVector::getSum(ppfOut[c], iBufferLength, true));

        CHECK(fStartFreq == pCGammatone->getCenterFreq(0));

        CHECK(Error_t::kNoError == CGammaToneFbIf::destroy(pCGammatone));
    }

    SECTION("SineInput")
    {
        CSynthesis::genSine<float>(pfIn, 100, 32000, iBufferLength);

        CHECK(Error_t::kNoError == CGammaToneFbIf::create(pCGammatone, pfIn, iBufferLength, fSampleRate, iNumBands, fStartFreq));

        CHECK(Error_t::kNoError == pCGammatone->getOutputDimensions(aiDims[0], aiDims[1]));
        CHECK(iNumBands == aiDims[0]);
        CHECK(iBufferLength == aiDims[1]);

        CHECK(Error_t::kNoError == pCGammatone->process(ppfOut));

        CHECK(1.F == Approx(CVector::getMax(ppfOut[0], iBufferLength, false)).margin(1e-4F).epsilon(1e-4F));
        CHECK(-1.F == Approx(CVector::getMin(ppfOut[0], iBufferLength, false)).margin(1e-4F).epsilon(1e-4F));

        CHECK(.01F > CVector::getMax(ppfOut[10], iBufferLength, false));

        CHECK(Error_t::kNoError == CGammaToneFbIf::destroy(pCGammatone));
    }

    SECTION("RealTime")
    {
        CSynthesis::genSine<float>(pfIn, 100, 32000, iBufferLength);

        CHECK(Error_t::kNoError == CGammaToneFbIf::create(pCGammatone, pfIn, iBufferLength, fSampleRate, iNumBands, fStartFreq));

        CHECK(Error_t::kNoError == pCGammatone->getOutputDimensions(aiDims[0], aiDims[1]));
        CHECK(iNumBands == aiDims[0]);
        CHECK(iBufferLength == aiDims[1]);

        CHECK(Error_t::kNoError == pCGammatone->process(ppfOut));

        CHECK(Error_t::kNoError == CGammaToneFbIf::destroy(pCGammatone));
        CHECK(Error_t::kNoError == CGammaToneFbIf::create(pCGammatone, fSampleRate, iNumBands, fStartFreq));

        float** ppfRtOut = 0;
        CMatrix::alloc(ppfRtOut, iNumBands, iBufferLength);
        int iBlockLength = 4096;
        for (auto n = 0; n < iBufferLength / iBlockLength; n++)
        {
            pCGammatone->process(ppfRtOut, &pfIn[n * iBlockLength], iBlockLength);
            for (auto c = 0; c < iNumBands; c++)
            {
                CVector::sub_I(ppfRtOut[c], &ppfOut[c][n * iBlockLength], iBlockLength);
                CHECK(0.F == CVector::getSum(ppfRtOut[c], iBlockLength));
            }
        }
        CMatrix::free(ppfRtOut, iNumBands);


        CHECK(Error_t::kNoError == CGammaToneFbIf::destroy(pCGammatone));
    }


    CGammaToneFbIf::destroy(pCGammatone);

    CVector::free(pfIn);
    CMatrix::free(ppfOut, iNumBands);

}

TEST_CASE("ToolsKnn", "[ToolsKnn]")
{
    CKnn* pCInstance = new CKnn();

    float** ppfTrainFeatures = 0;
    int* piClass = 0;
    float* pfQuery = 0;
    int* piQueryGT = 0;

    int aiDim[2] = { 3,6 };

    CMatrix::alloc(ppfTrainFeatures, aiDim[0], aiDim[1]);
    CVector::alloc(piClass, aiDim[1]);
    CVector::alloc(pfQuery, aiDim[0]);
    CVector::alloc(piQueryGT, 5);

    SECTION("Api")
    {

        CHECK(Error_t::kFunctionInvalidArgsError == pCInstance->init(0, piClass, aiDim[0], aiDim[1]));
        CHECK(Error_t::kFunctionInvalidArgsError == pCInstance->init(ppfTrainFeatures, 0, aiDim[0], aiDim[1]));
        CHECK(Error_t::kFunctionInvalidArgsError == pCInstance->init(ppfTrainFeatures, piClass, 0, aiDim[1]));
        CHECK(Error_t::kFunctionInvalidArgsError == pCInstance->init(ppfTrainFeatures, piClass, aiDim[0], 0));
        CHECK(CClassifierBase::kIllegalClassLabel == pCInstance->classify(ppfTrainFeatures[0]));

        CHECK(Error_t::kNoError == pCInstance->init(ppfTrainFeatures, piClass, aiDim[0], aiDim[1]));
        CHECK(CClassifierBase::kIllegalClassLabel == pCInstance->classify(0));

        CHECK(Error_t::kFunctionInvalidArgsError == pCInstance->setParam(0));
        CHECK(Error_t::kNoError == pCInstance->setParam(17));

        // zero test
        CHECK(0 == pCInstance->classify(ppfTrainFeatures[0]));

        CHECK(Error_t::kNoError == pCInstance->reset());
    }

    SECTION("test1")
    {
        ppfTrainFeatures[0][0] = 1.F; ppfTrainFeatures[0][1] = 2.F; ppfTrainFeatures[0][2] = 3.F; ppfTrainFeatures[0][3] = .8F; ppfTrainFeatures[0][4] = 1.8F; ppfTrainFeatures[0][5] = 3.F;
        ppfTrainFeatures[1][0] = 1.F; ppfTrainFeatures[1][1] = 2.F; ppfTrainFeatures[1][2] = 3.F; ppfTrainFeatures[1][3] = .9F; ppfTrainFeatures[1][4] = 2.F; ppfTrainFeatures[1][5] = 3.F;
        ppfTrainFeatures[2][0] = 1.F; ppfTrainFeatures[2][1] = 2.F; ppfTrainFeatures[2][2] = 3.F; ppfTrainFeatures[2][3] = .8F; ppfTrainFeatures[2][4] = 1.9F; ppfTrainFeatures[2][5] = 3.F;

        piClass[0] = 0; piClass[1] = 1; piClass[2] = 2; piClass[3] = 0; piClass[4] = 1; piClass[5] = 2;
        piQueryGT[0] = 2; piQueryGT[1] = 1; piQueryGT[2] = 0; piQueryGT[3] = 0; piQueryGT[4] = 1;

        CHECK(Error_t::kNoError == pCInstance->init(ppfTrainFeatures, piClass, aiDim[0], aiDim[1]));

        //////////////////////////////////////////////////////////////
        CHECK(Error_t::kNoError == pCInstance->setParam(1));

        pfQuery[0] = 10.F; pfQuery[1] = 10.F; pfQuery[2] = 10.F;
        CHECK(piQueryGT[0] == pCInstance->classify(pfQuery));

        pfQuery[0] = 2.F; pfQuery[1] = 2.F; pfQuery[2] = 2.F;
        CHECK(piQueryGT[1] == pCInstance->classify(pfQuery));

        pfQuery[0] = 1.1F; pfQuery[1] = 0.95F; pfQuery[2] = 1.3F;
        CHECK(piQueryGT[2] == pCInstance->classify(pfQuery));

        pfQuery[0] = 0.F; pfQuery[1] = 0.F; pfQuery[2] = 0.F;
        CHECK(piQueryGT[3] == pCInstance->classify(pfQuery));

        pfQuery[0] = 1.5F; pfQuery[1] = 1.5F; pfQuery[2] = 1.5F;
        CHECK(piQueryGT[4] == pCInstance->classify(pfQuery));

        //////////////////////////////////////////////////////////////
        CHECK(Error_t::kNoError == pCInstance->setParam(2));

        pfQuery[0] = 10.F; pfQuery[1] = 10.F; pfQuery[2] = 10.F;
        CHECK(piQueryGT[0] == pCInstance->classify(pfQuery));

        pfQuery[0] = 2.F; pfQuery[1] = 2.F; pfQuery[2] = 2.F;
        CHECK(piQueryGT[1] == pCInstance->classify(pfQuery));

        pfQuery[0] = 1.1F; pfQuery[1] = 0.95F; pfQuery[2] = 1.3F;
        CHECK(piQueryGT[2] == pCInstance->classify(pfQuery));

        pfQuery[0] = 0.F; pfQuery[1] = 0.F; pfQuery[2] = 0.F;
        CHECK(piQueryGT[3] == pCInstance->classify(pfQuery));

        pfQuery[0] = 1.5F; pfQuery[1] = 1.5F; pfQuery[2] = 1.5F;
        CHECK(piQueryGT[4] == pCInstance->classify(pfQuery));

        //////////////////////////////////////////////////////////////
        CHECK(Error_t::kNoError == pCInstance->setParam(5));

        pfQuery[0] = 10.F; pfQuery[1] = 10.F; pfQuery[2] = 10.F;
        CHECK(piQueryGT[0] == pCInstance->classify(pfQuery));

        pfQuery[0] = 2.F; pfQuery[1] = 2.F; pfQuery[2] = 2.F;
        CHECK(piQueryGT[1] == pCInstance->classify(pfQuery));

        pfQuery[0] = 1.1F; pfQuery[1] = 0.95F; pfQuery[2] = 1.3F;
        CHECK(piQueryGT[2] == pCInstance->classify(pfQuery));

        pfQuery[0] = 0.F; pfQuery[1] = 0.F; pfQuery[2] = 0.F;
        CHECK(piQueryGT[3] == pCInstance->classify(pfQuery));

        pfQuery[0] = 1.5F; pfQuery[1] = 1.5F; pfQuery[2] = 1.5F;
        CHECK(piQueryGT[4] == pCInstance->classify(pfQuery));


        CHECK(Error_t::kNoError == pCInstance->reset());
    }

    SECTION("test2")
    {
        ppfTrainFeatures[0][0] = 1.F; ppfTrainFeatures[0][1] = 1.F; ppfTrainFeatures[0][2] = -1.F; ppfTrainFeatures[0][3] = 2.1F;
        ppfTrainFeatures[1][0] = 0.F; ppfTrainFeatures[1][1] = 2.F; ppfTrainFeatures[1][2] = 2.F; ppfTrainFeatures[1][3] = 1.F;

        piClass[0] = 0; piClass[1] = 1; piClass[2] = 1; piClass[3] = 0;
        piQueryGT[0] = 0; piQueryGT[1] = 1; piQueryGT[3] = 0;
        pfQuery[0] = 0.F; pfQuery[1] = 0.F;

        CHECK(Error_t::kNoError == pCInstance->init(ppfTrainFeatures, piClass, 2, 4));

        //////////////////////////////////////////////////////////////
        CHECK(Error_t::kNoError == pCInstance->setParam(1));
        CHECK(piQueryGT[0] == pCInstance->classify(pfQuery));

        //////////////////////////////////////////////////////////////
        CHECK(Error_t::kNoError == pCInstance->setParam(3));
        CHECK(piQueryGT[1] == pCInstance->classify(pfQuery));

        //////////////////////////////////////////////////////////////
        CHECK(Error_t::kNoError == pCInstance->setParam(4));
        CHECK(piQueryGT[2] == pCInstance->classify(pfQuery));

        CHECK(Error_t::kNoError == pCInstance->reset());
    }


    CMatrix::free(ppfTrainFeatures, aiDim[0]);
    CVector::free(piClass);
    CVector::free(pfQuery);
    CVector::free(piQueryGT);

    delete pCInstance;
}

TEST_CASE("ToolsInstFreq", "[ToolsInstFreq]")
{
    int iBlockLength = 1024;
    int iHopLength = 128;
    int iFreqLength = iBlockLength / 2 + 1;
    float fSampleRate = 48000;
    int iBuffLength = iBlockLength + 2 * iHopLength;

    float* pfIn = new float[iBuffLength];
    float* pfTmp = new float[iBuffLength];
    float* pfOut = new float[iFreqLength];
    CFft::complex_t* pfSpectrum = new float[iBuffLength];

    CFft* pCFft = new CFft();
    CInstFreq* pCInstFreq = new CInstFreq(iBlockLength, iHopLength, fSampleRate);

    pCFft->init(iBlockLength);
    CVector::setZero(pfIn, iBuffLength);
    CVector::setZero(pfOut, iFreqLength);
    CVector::setZero(pfSpectrum, iBlockLength);

    SECTION("Api")
    {
        CHECK(Error_t::kFunctionInvalidArgsError == pCInstFreq->process(0, pfIn));
        CHECK(Error_t::kFunctionInvalidArgsError == pCInstFreq->process(pfOut, 0));
    }

    SECTION("Sine")
    {
        int aiBins[3] = { 32, 8, 4 };
        float afBinOffset[3] = { .5F, .25F, 0.F };
        float afFreq[3] = { 0 };

        // generate test signal with three sines
        for (auto s = 0; s < 3; s++)
        {
            afFreq[s] = fSampleRate / iBlockLength * (aiBins[s] + afBinOffset[s]);
            CSynthesis::genSine(pfTmp, afFreq[s], fSampleRate, iBuffLength);

            CVector::add_I(pfIn, pfTmp, iBuffLength);
        }

        // compute FFTs
        for (auto n = 0; n < 3; n++)
        {
            pCFft->compFft(pfSpectrum, &pfIn[n * iHopLength]);
            pCInstFreq->process(pfOut, pfSpectrum);

            if (n == 2)
            {
                for (auto s = 0; s < 3; s++)
                    CHECK(afFreq[s] == Approx(pfOut[aiBins[s]]).margin(5e-1F).epsilon(5e-1F));
            }
        }
    }

    delete pCInstFreq;
    delete pCFft;

    delete[] pfSpectrum;
    delete[] pfIn;
    delete[] pfTmp;
    delete[] pfOut;

}

TEST_CASE("ToolsMovingAverage", "[ToolsMovingAverage]")
{
    CMovingAverage* pCLowPass = 0;
    float* pfInput = 0,
        * pfOut = 0;
    int iNumValues = 1024;

    CMovingAverage::create(pCLowPass);
    pfInput = new float[iNumValues];
    pfOut = new float[2 * iNumValues];

    CVector::setZero(pfInput, iNumValues);
    CVector::setZero(pfOut, iNumValues);

    SECTION("Api")
    {
        CHECK(Error_t::kFunctionInvalidArgsError ==pCLowPass->setFilterParam(0));
        CHECK(Error_t::kFunctionInvalidArgsError ==pCLowPass->setFilterParam(-5));

        CHECK(Error_t::kNoError == pCLowPass->reset());

        CHECK(Error_t::kNoError == pCLowPass->setFilterParam(5));
        CHECK(5.F == Approx(pCLowPass->getFilterParam()).margin(1e-6F).epsilon(1e-6F));
        CHECK(Error_t::kNoError == pCLowPass->setFilterParam(1000));
        CHECK(1000.F == Approx(pCLowPass->getFilterParam()).margin(1e-6F).epsilon(1e-6F));
        CHECK(Error_t::kNoError == pCLowPass->setFilterParam(1));
        CHECK(1.F == Approx(pCLowPass->getFilterParam()).margin(1e-6F).epsilon(1e-6F));

        CHECK(Error_t::kFunctionInvalidArgsError ==pCLowPass->process(0, pfInput, iNumValues));
        CHECK(Error_t::kFunctionInvalidArgsError ==pCLowPass->process(pfOut, 0, iNumValues));
        CHECK(Error_t::kFunctionInvalidArgsError ==pCLowPass->process(pfOut, pfInput, 0));
        CHECK(Error_t::kNoError == pCLowPass->process(pfOut, pfInput, iNumValues));
    }

    SECTION("Process")
    {
        // zeros
        for (auto i = 0; i < iNumValues; i++)
        {
            CHECK(Error_t::kNoError == pCLowPass->process(&pfOut[i], &pfInput[i], 1));
            CHECK(0.F == Approx(pfOut[i]).margin(1e-6F).epsilon(1e-6F));
        }

        // ones
        CVector::setValue(pfInput, 1.F, iNumValues);
        pCLowPass->reset();
        pCLowPass->setFilterParam(1);
        CHECK(Error_t::kNoError == pCLowPass->process(pfOut, pfInput, iNumValues));
        for (auto i = 0; i < iNumValues; i++)
            CHECK(pfInput[i] == Approx(pfOut[i]).margin(1e-6F).epsilon(1e-6F));

        for (auto c = 0; c < 10; c++)
        {
            int iLength = c * 10 + 2;
            pCLowPass->reset();
            pCLowPass->setFilterParam(iLength);
            for (auto i = 0; i < iLength; i++)
            {
                CHECK(Error_t::kNoError == pCLowPass->process(&pfOut[i], &pfInput[i], 1));
                CHECK((i + 1.F) / iLength == Approx(pfOut[i]).margin(1e-6F).epsilon(1e-6F));
            }
        }
    }

    SECTION("Filtfilt")
    {
        int iSignalLength = 16;
        int iFilterLength = 5;
        int iMaxIdx = 6;
        pCLowPass->setFilterParam(iFilterLength);

        pfInput[iMaxIdx] = 1;

        pCLowPass->filtfilt(pfOut, pfInput, iSignalLength);
        
        CHECK(.2F == Approx(CVector::getMax(pfOut, iSignalLength)).margin(1e-6F).epsilon(1e-6F));
        CHECK(.2F == Approx(pfOut[iMaxIdx]).margin(1e-6F).epsilon(1e-6F));
        CHECK(.0F == Approx(CVector::getMin(pfOut, iSignalLength)).margin(1e-6F).epsilon(1e-6F));

        for (auto i = iMaxIdx-iFilterLength+1; i <= iMaxIdx; i++)
            CHECK(pfOut[i - 1] < pfOut[i]);
        for (auto i = iMaxIdx; i < iMaxIdx + iFilterLength-1; i++)
            CHECK(pfOut[i + 1] < pfOut[i]);

        iFilterLength = 4;
        pCLowPass->setFilterParam(iFilterLength);

        pCLowPass->filtfilt(pfOut, pfInput, iSignalLength);

        CHECK(.25F == Approx(CVector::getMax(pfOut, iSignalLength)).margin(1e-6F).epsilon(1e-6F));
        CHECK(.25F == Approx(pfOut[iMaxIdx]).margin(1e-6F).epsilon(1e-6F));
        CHECK(.0F == Approx(CVector::getMin(pfOut, iSignalLength)).margin(1e-6F).epsilon(1e-6F));

        for (auto i = iMaxIdx - iFilterLength + 1; i <= iMaxIdx; i++)
            CHECK(pfOut[i - 1] < pfOut[i]);
        for (auto i = iMaxIdx; i < iMaxIdx + iFilterLength - 1; i++)
            CHECK(pfOut[i + 1] < pfOut[i]);


        //DC input
        iFilterLength = 3;
        pCLowPass->setFilterParam(iFilterLength);
        CVector::setValue(pfInput, 1.F, iSignalLength);
        pCLowPass->filtfilt(pfOut, pfInput, iSignalLength);
        CHECK(1.F == Approx(CVector::getMax(pfOut, iSignalLength)).margin(1e-6F).epsilon(1e-6F));
        CHECK(1.F == Approx(CVector::getMean(&pfOut[iFilterLength - 1], iSignalLength - 2 * (iFilterLength - 1))).margin(1e-6F).epsilon(1e-6F));
    }

    CMovingAverage::destroy(pCLowPass);

    delete[] pfInput;
    delete[] pfOut;
}

TEST_CASE("ToolsInterp", "[ToolsInterp]")
{
    int iInputLength = 10;
    int iOutputLength = 32;
    float* pfIn = new float[iInputLength];
    float* pfOut = new float[iOutputLength];
    float* pfOutIdx = new float[iOutputLength];

    for (auto i = 0; i < iInputLength; i++)
        pfIn[i] = i + 1.F;

    SECTION("Api")
    {
        CHECK(Error_t::kFunctionInvalidArgsError == CResample::interp1d(0, pfOutIdx, pfIn, iOutputLength, iInputLength));
        CHECK(Error_t::kFunctionInvalidArgsError == CResample::interp1d(pfOut, 0, pfIn, iOutputLength, iInputLength));
        CHECK(Error_t::kFunctionInvalidArgsError == CResample::interp1d(pfOut, pfOutIdx, 0, iOutputLength, iInputLength));
        CHECK(Error_t::kFunctionInvalidArgsError == CResample::interp1d(pfOut, pfOutIdx, pfIn, 0, iInputLength));
        CHECK(Error_t::kFunctionInvalidArgsError == CResample::interp1d(pfOut, pfOutIdx, pfIn, iOutputLength, 0));
    }

    SECTION("Zeros")
    {
        for (auto i = 0; i < iOutputLength; i++)
            pfOutIdx[i] = i - 500.F;
        CHECK(Error_t::kNoError == CResample::interp1d( pfOut, pfOutIdx,pfIn, iOutputLength, iInputLength));
        CHECK(0.F == CVector::getSum(pfOut, iOutputLength));

        for (auto i = 0; i < iOutputLength; i++)
            pfOutIdx[i] = i + 500.F;
        CHECK(Error_t::kNoError == CResample::interp1d(pfOut, pfOutIdx, pfIn, iOutputLength, iInputLength));
        CHECK(0.F == CVector::getSum(pfOut, iOutputLength));

        for (auto i = 0; i < iOutputLength; i++)
            pfOutIdx[i] = i - 3.4F;
        CHECK(Error_t::kNoError == CResample::interp1d(pfOut, pfOutIdx, pfIn, iOutputLength, iInputLength));
        CHECK(0.F == CVector::getSum(pfOut, 3));
        CHECK(0.F == CVector::getSum(&pfOut[iInputLength + 4], iOutputLength - iInputLength - 4));

        for (auto i = 3; i < iInputLength + 3; i++)
            CHECK((i - 3) + .6F == Approx(pfOut[i]).margin(1e-6F).epsilon(1e-6F));
        CHECK(pfIn[iInputLength - 1] * (1 - .6F) == Approx(pfOut[iInputLength + 3]).margin(1e-6F).epsilon(1e-6F));
    }

    SECTION("Sine")
    {
        iOutputLength = 30;
        float* pfSine = 0;
        CVector::alloc(pfSine, iOutputLength);
        CSynthesis::genSine(pfSine, 1.F, 1.F*iOutputLength, iOutputLength);

        for (auto i = 0; i < iInputLength; i++)
            pfIn[i] = pfSine[3*i];

        for (auto i = 0; i < iOutputLength; i++)
            pfOutIdx[i] = i / 3.F;

        CHECK(Error_t::kNoError == CResample::interp1d(pfOut, pfOutIdx, pfIn, iOutputLength, iInputLength));

        for (auto i = 0; i < iOutputLength; i++)
            CHECK(pfSine[i] == Approx(pfOut[i]).margin(1e-1F).epsilon(1e-1F));
        CVector::free(pfSine);
    }

    delete[] pfIn;
    delete[] pfOut;
    delete[] pfOutIdx;
}

TEST_CASE("ToolsNmf", "[ToolsNmf]")
{
    CNmf* pCInstance = 0;
    float** ppfInput = 0;
    float** ppfOut = 0;
    float** ppfW = 0;
    float** ppfH = 0;
    int aiDim[2] = { 128, 13 };
    int iRank = 2;
    int iMaxIter = 300;
    CNmfResult* pCResult = new CNmfResult();

    pCInstance = new CNmf();
    CMatrix::alloc(ppfInput, aiDim[0], aiDim[1]);
    CMatrix::alloc(ppfOut, aiDim[0], aiDim[1]);
    CMatrix::alloc(ppfW, aiDim[0], iRank + 1);
    CMatrix::alloc(ppfH, iRank + 1, aiDim[1]);

    SECTION("Api")
    {
        CHECK(Error_t::kFunctionIllegalCallError == pCResult->getMat(ppfW, CNmfResult::kW));

        CHECK(Error_t::kFunctionInvalidArgsError == pCInstance->init(0, iRank, aiDim[0], aiDim[1], iMaxIter));
        CHECK(Error_t::kFunctionInvalidArgsError == pCInstance->init(pCResult, 0, aiDim[0], aiDim[1], iMaxIter));
        CHECK(Error_t::kFunctionInvalidArgsError == pCInstance->init(pCResult, iRank, 0, aiDim[1], iMaxIter));
        CHECK(Error_t::kFunctionInvalidArgsError == pCInstance->init(pCResult, iRank, aiDim[0], 0, iMaxIter));
        CHECK(Error_t::kFunctionInvalidArgsError == pCInstance->init(pCResult, iRank, aiDim[0], aiDim[1], 0));
        CHECK(Error_t::kFunctionIllegalCallError == pCInstance->compNmf(pCResult, ppfInput));

        CHECK(Error_t::kNoError == pCInstance->init(pCResult, iRank, aiDim[0], aiDim[1], iMaxIter));

        CHECK(Error_t::kFunctionInvalidArgsError == pCResult->getMat(0, CNmfResult::kW));
        CHECK(Error_t::kNoError == pCResult->getMat(ppfW, CNmfResult::kW));

        CHECK(Error_t::kFunctionInvalidArgsError == pCInstance->compNmf(0, ppfInput));
        CHECK(Error_t::kFunctionInvalidArgsError == pCInstance->compNmf(pCResult, 0));

        CHECK(Error_t::kNoError == pCInstance->compNmf(pCResult, ppfInput));
        CHECK(Error_t::kNoError == pCInstance->reset());
    }

    SECTION("Trivial")
    {
        srand(42);
        aiDim[0] = 8;
        aiDim[1] = 4;
        for (auto k = 0; k < 4; k++)
        {
            ppfInput[k][0] = 1.F;
            ppfInput[k + 4][1] = 1.F;
        }

        for (auto k = 0; k < aiDim[0]; k++)
        {
            ppfInput[k][2] = .7F * ppfInput[k][0] + .3F * ppfInput[k][1];
            ppfInput[k][3] = .5F * ppfInput[k][0] + .5F * ppfInput[k][1];
        }

        CHECK(Error_t::kNoError == pCInstance->init(pCResult, iRank, aiDim[0], aiDim[1], iMaxIter));

        CHECK(Error_t::kNoError == pCInstance->compNmf(pCResult, ppfInput));
        CHECK(Error_t::kNoError == pCResult->getMat(ppfW, CNmfResult::kW));
        CHECK(Error_t::kNoError == pCResult->getMat(ppfH, CNmfResult::kH));
        CHECK(Error_t::kNoError == pCResult->getMat(ppfOut, CNmfResult::kXHat));

        // check XHat
        CMatrix::sub_I(ppfInput, ppfOut, aiDim[0], aiDim[1]);
        CHECK(0 == Approx(CMatrix::getMax(ppfInput, aiDim[0], aiDim[1])).margin(1e-2F).epsilon(1e-2F));

        // check H
        CHECK(ppfH[0][0] == Approx(ppfH[1][1]).margin(1e-4F).epsilon(1e-3F));
        CHECK(ppfH[0][1] == Approx(ppfH[1][0]).margin(1e-4F).epsilon(1e-3F));
        CHECK(ppfH[0][3] == Approx(ppfH[1][3]).margin(1e-4F).epsilon(1e-3F));
        CHECK(((.7F / .3F == Approx(ppfH[0][2] / ppfH[1][2]).margin(1e-2F).epsilon(1e-2F)) || (.3F / .7F == Approx(ppfH[0][2] / ppfH[1][2]).margin(1e-2F).epsilon(1e-2F))));

        // check W
        float afRes[4] = { 0 };
        for (auto k = 0; k < 4; k++)
        {
            afRes[0] += ppfW[k][0];
            afRes[1] += ppfW[k][1];
            afRes[2] += ppfW[k + 4][0];
            afRes[3] += ppfW[k + 4][1];
        }
        CHECK(afRes[0] == Approx(afRes[3]).margin(1e-3F).epsilon(1e-3F));
        CHECK(afRes[1] == Approx(afRes[2]).margin(1e-3F).epsilon(1e-3F));
        CHECK(afRes[2] + afRes[3] == Approx(afRes[0] + afRes[1]).margin(1e-3F).epsilon(1e-3F));

        aiDim[0] = 128;
        aiDim[1] = 13;
    }

    SECTION("2Rank")
    {
        aiDim[1] = 8;
        srand(42);
        CMatrix::setRand(ppfInput, aiDim[0], aiDim[1]);
        CMatrix::mulC_I(ppfInput, 1.F / 20.F, aiDim[0], aiDim[1]);

        for (auto k = 1; k < aiDim[0]; k++)
        {
            if (k % 4 == 0)
            {
                ppfInput[k][0] = 1;
                ppfInput[k][1] = 1;
            }
        }

        for (auto k = 1; k < aiDim[0]; k++)
        {
            if (k % 7 == 0)
            {
                ppfInput[k][2] = 1;
                ppfInput[k][3] = 1;
            }
        }

        for (auto k = 0; k < aiDim[0]; k++)
        {
            ppfInput[k][4] = .7F * ppfInput[k][0] + .3F * ppfInput[k][2];
            ppfInput[k][5] = ppfInput[k][4];
        }

        CHECK(Error_t::kNoError == pCInstance->init(pCResult, iRank, aiDim[0], aiDim[1], iMaxIter));

        CHECK(Error_t::kNoError == pCInstance->compNmf(pCResult, ppfInput));
        CHECK(Error_t::kNoError == pCResult->getMat(ppfW, CNmfResult::kW));
        CHECK(Error_t::kNoError == pCResult->getMat(ppfH, CNmfResult::kH));
        CHECK(Error_t::kNoError == pCResult->getMat(ppfOut, CNmfResult::kXHat));

        CHECK(ppfW[4][1] == Approx(ppfW[8][1]).margin(1e-2F).epsilon(1e-2F));
        CHECK(ppfW[4][1] == Approx(ppfW[12][1]).margin(1e-2F).epsilon(1e-2F));
        CHECK(ppfW[4][1] == Approx(ppfW[16][1]).margin(1e-2F).epsilon(1e-2F));
        CHECK(ppfW[7][0] == Approx(ppfW[14][0]).margin(1e-2F).epsilon(1e-2F));
        CHECK(ppfW[7][0] == Approx(ppfW[21][0]).margin(1e-2F).epsilon(1e-2F));
        CHECK(ppfW[7][0] == Approx(ppfW[28][0]).margin(1e-2F).epsilon(1e-2F));

        aiDim[0] = 128;
        aiDim[1] = 13;
    }

    SECTION("3Rank")
    {
        aiDim[0] = 8;
        aiDim[1] = 13;
        iRank = 3;

        for (auto j = 0; j < 4; j++)
            ppfInput[2][j] = 1.F;
        for (auto j = 4; j < 9; j++)
            ppfInput[5][j] = 1.F;
        for (auto j = 9; j < 13; j++)
            ppfInput[7][j] = 1.F;

        CHECK(Error_t::kNoError == pCInstance->init(pCResult, iRank, aiDim[0], aiDim[1], iMaxIter));

        CHECK(Error_t::kNoError == pCInstance->compNmf(pCResult, ppfInput));

        CHECK(Error_t::kNoError == pCResult->getMat(ppfW, CNmfResult::kW));
        CHECK(Error_t::kNoError == pCResult->getMat(ppfH, CNmfResult::kH));
        CHECK(Error_t::kNoError == pCResult->getMat(ppfOut, CNmfResult::kXHat));

        CHECK(3.F == Approx(CMatrix::getSum(ppfW, aiDim[0], iRank)).margin(1e-3F).epsilon(1e-3F));
        CHECK(13.F == Approx(CMatrix::getSum(ppfH, iRank, aiDim[1])).margin(1e-3F).epsilon(1e-3F));

        aiDim[0] = 128;
        aiDim[1] = 13;
        iRank = 2;
    }

    delete pCResult;
    delete pCInstance;

    CMatrix::free(ppfInput, aiDim[0]);
    CMatrix::free(ppfOut, aiDim[0]);
    CMatrix::free(ppfW, aiDim[0]);
    CMatrix::free(ppfH, iRank + 1);
}

TEST_CASE("ToolsPcaCov", "[ToolsPca]")
{
    float** ppfInput = 0;
    float** ppfCov = 0;
    int aiDim[2] = { 8, 10 };
    CMatrix::alloc(ppfInput, aiDim[0], aiDim[1]);
    CMatrix::alloc(ppfCov, aiDim[0], aiDim[0]);

    SECTION("Api")
    {
        CHECK(Error_t::kFunctionInvalidArgsError == CPca::compCov(0, ppfInput, aiDim[0], aiDim[1]));
        CHECK(Error_t::kFunctionInvalidArgsError == CPca::compCov(ppfCov, 0, aiDim[0], aiDim[1]));
        CHECK(Error_t::kFunctionInvalidArgsError == CPca::compCov(ppfCov, ppfInput, 0, aiDim[1]));
        CHECK(Error_t::kFunctionInvalidArgsError == CPca::compCov(ppfCov, ppfInput, aiDim[0], 0));

        CHECK(Error_t::kNoError == CPca::compCov(ppfCov, ppfInput, aiDim[0], aiDim[1]));

    }

    SECTION("Trivial")
    {
        int iNumRows = 2;
        int iNumCols = 3;

        for (auto j = 0; j < iNumCols; j++)
        {
            ppfInput[0][j] = 1.F * j;
            ppfInput[1][iNumCols - 1 - j] = ppfInput[0][j];
        }

        CHECK(Error_t::kNoError == CPca::compCov(ppfCov, ppfInput, iNumRows, iNumCols));

        CHECK(1.F == Approx(ppfCov[0][0]).margin(1e-6F).epsilon(1e-6F));
        CHECK(1.F == Approx(ppfCov[1][1]).margin(1e-6F).epsilon(1e-6F));
        CHECK(-1.F == Approx(ppfCov[0][1]).margin(1e-6F).epsilon(1e-6F));
        CHECK(-1.F == Approx(ppfCov[1][0]).margin(1e-6F).epsilon(1e-6F));
    }

    SECTION("3Var")
    {
        int iNumRows = 3;
        int iNumCols = 3;

        ppfInput[0][0] = 1.F;  ppfInput[0][1] = 3.F;  ppfInput[0][2] = -5.F;
        ppfInput[1][0] = 3.F;  ppfInput[1][1] = 9.F;  ppfInput[1][2] = 4.F;
        ppfInput[2][0] = -7.F;  ppfInput[2][1] = 2.F;  ppfInput[2][2] = 6.F;

        CHECK(Error_t::kNoError == CPca::compCov(ppfCov, ppfInput, iNumRows, iNumCols));

        CHECK(17.333333F == Approx(ppfCov[0][0]).margin(1e-6F).epsilon(1e-6F));
        CHECK(10.333333F == Approx(ppfCov[1][1]).margin(1e-6F).epsilon(1e-6F));
        CHECK(44.333333F == Approx(ppfCov[2][2]).margin(1e-6F).epsilon(1e-6F));

        CHECK(ppfCov[0][1] == Approx(ppfCov[1][0]).margin(1e-6F).epsilon(1e-6F));
        CHECK(ppfCov[0][2] == Approx(ppfCov[2][0]).margin(1e-6F).epsilon(1e-6F));
        CHECK(ppfCov[1][2] == Approx(ppfCov[2][1]).margin(1e-6F).epsilon(1e-6F));

        CHECK(7.6666666F == Approx(ppfCov[1][0]).margin(1e-6F).epsilon(1e-6F));
        CHECK(-15.3333333F == Approx(ppfCov[2][0]).margin(1e-6F).epsilon(1e-6F));
        CHECK(7.83333333F == Approx(ppfCov[2][1]).margin(1e-6F).epsilon(1e-6F));
    }


    CMatrix::free(ppfInput, aiDim[0]);
    CMatrix::free(ppfCov, aiDim[0]);
}

TEST_CASE("ToolsPca", "[ToolsPca]")
{
    float** ppfIn = 0;
    float** ppfOut = 0;
    float* pfEigenValues = 0;
    int aiDim[2] = { 2,8 };
    CMatrix::alloc(ppfIn, aiDim[0], aiDim[1]);
    CMatrix::alloc(ppfOut, aiDim[0], aiDim[1]);
    CVector::alloc(pfEigenValues, aiDim[0]);
    CPca *pCInstance = new CPca();

    SECTION("Api")
    {
        CHECK(Error_t::kFunctionInvalidArgsError == pCInstance->init(0, aiDim[1]));
        CHECK(Error_t::kFunctionInvalidArgsError == pCInstance->init(aiDim[0], 0));

        CHECK(Error_t::kNoError == pCInstance->init(aiDim[0], aiDim[1]));

        CHECK(Error_t::kFunctionInvalidArgsError == pCInstance->compPca(0, pfEigenValues, ppfIn));
        CHECK(Error_t::kFunctionInvalidArgsError == pCInstance->compPca(ppfOut, 0, ppfIn));
        CHECK(Error_t::kFunctionInvalidArgsError == pCInstance->compPca(ppfOut, pfEigenValues, 0));
        
        CHECK(Error_t::kNoError == pCInstance->compPca(ppfOut, pfEigenValues, ppfIn));
        CHECK(Error_t::kNoError == pCInstance->reset());
    }

    SECTION("OneComponent")
    {
        for (auto i = 0; i < 4; i++)
        {
            ppfIn[0][i] = i + 1.F;
            ppfIn[1][i] = .5F * ppfIn[0][i];
            ppfIn[0][i + 4] = -ppfIn[0][i];
            ppfIn[1][i + 4] = -ppfIn[1][i];
        }

        CHECK(Error_t::kNoError == pCInstance->init(aiDim[0], aiDim[1]));
        CHECK(Error_t::kNoError == pCInstance->compPca(ppfOut, pfEigenValues, ppfIn));

        CHECK(0.F == Approx(pfEigenValues[1]).margin(1e-6F).epsilon(1e-6F));

        CHECK(0.F == Approx(CVector::getSum(ppfOut[1], aiDim[1])).margin(1e-6F).epsilon(1e-6F));
    }

    SECTION("SimpleMatrix")
    {
        ppfIn[0][0] = -2.F; ppfIn[0][1] = -1.F; ppfIn[0][2] = 1.F; ppfIn[0][3] = 2.F; ppfIn[0][4] = -1.F; ppfIn[0][5] = -.5F; ppfIn[0][6] = .5F; ppfIn[0][7] = 1.F;
        CVector::copy(ppfIn[1], ppfIn[0], aiDim[1]);
        CVector::mulC_I(&ppfIn[1][4], -1.F, 4);


        CHECK(Error_t::kNoError == pCInstance->init(aiDim[0], aiDim[1]));
        CHECK(Error_t::kNoError == pCInstance->compPca(ppfOut, pfEigenValues, ppfIn));

        CHECK(2.85714285714286F == Approx(pfEigenValues[0]).margin(1e-6F).epsilon(1e-6F));
        CHECK(0.714285714285714F == Approx(pfEigenValues[1]).margin(1e-6F).epsilon(1e-6F));

        CHECK(0.F == Approx(CVector::getSum(ppfOut[1], 4)).margin(1e-6F).epsilon(1e-6F));
        CHECK(0.F == Approx(CVector::getSum(&ppfOut[0][4], 4)).margin(1e-6F).epsilon(1e-6F));
        CHECK(ppfOut[0][3] == Approx(-ppfOut[0][0]).margin(1e-6F).epsilon(1e-6F));
        CHECK(ppfOut[0][2] == Approx(-ppfOut[0][1]).margin(1e-6F).epsilon(1e-6F));
        CHECK(ppfOut[1][7] == Approx(-ppfOut[1][4]).margin(1e-6F).epsilon(1e-6F));
        CHECK(ppfOut[1][6] == Approx(-ppfOut[1][5]).margin(1e-6F).epsilon(1e-6F));
        CHECK(ppfOut[0][1] == Approx(.5F * ppfOut[0][0]).margin(1e-6F).epsilon(1e-6F));
        CHECK(ppfOut[0][2] == Approx(.5F * ppfOut[0][3]).margin(1e-6F).epsilon(1e-6F));
        CHECK(ppfOut[1][5] == Approx(.5F * ppfOut[1][4]).margin(1e-6F).epsilon(1e-6F));
        CHECK(ppfOut[1][6] == Approx(.5F * ppfOut[1][7]).margin(1e-6F).epsilon(1e-6F));
    }

    CMatrix::free(ppfIn, aiDim[0]);
    CMatrix::free(ppfOut, aiDim[0]);
    CVector::free(pfEigenValues);
    delete pCInstance;
}

TEST_CASE("ToolsPcaSvd", "[Svd]")
{
    float** ppfIn = 0;
    float** ppfU = 0;
    float** ppfW = 0;
    float** ppfV = 0;
    float** ppfRes = 0;
    float** ppfTmp = 0;

    int aiDim[2] = { 4,2 };

    CMatrix::alloc(ppfIn, aiDim[0], aiDim[1]);
    CMatrix::alloc(ppfU, aiDim[0], aiDim[1]);
    CMatrix::alloc(ppfW, aiDim[1], aiDim[1]);
    CMatrix::alloc(ppfV, aiDim[1], aiDim[1]);
    CMatrix::alloc(ppfRes, aiDim[0], aiDim[0]);
    CMatrix::alloc(ppfTmp, aiDim[0], aiDim[0]);

    SECTION("zero")
    {
        CPca::calcSVD(ppfU, ppfW, ppfV, ppfIn, aiDim[0], aiDim[1]);

        for (auto i = 0; i < aiDim[1]; i++)
            CHECK(1 == ppfU[i][i]);
        CHECK(aiDim[1] == CMatrix::getSum(ppfU, aiDim[1], aiDim[1]));

        CHECK(0 == CMatrix::getSum(ppfW, aiDim[1], aiDim[1]));

        for (auto i = 0; i < aiDim[1]; i++)
            CHECK(1 == ppfV[i][i]);
        CHECK(aiDim[1] == CMatrix::getSum(ppfV, aiDim[1], aiDim[1]));
    }

    SECTION("matlabexample")
    {
        ppfIn[0][0] = 1.F; ppfIn[0][1] = 2.F;
        ppfIn[1][0] = 3.F; ppfIn[1][1] = 4.F;
        ppfIn[2][0] = 5.F; ppfIn[2][1] = 6.F;
        ppfIn[3][0] = 7.F; ppfIn[3][1] = 8.F;

        CPca::calcSVD(ppfU, ppfW, ppfV, ppfIn, aiDim[0], aiDim[1]);

        CMatrix::transpose(ppfRes, ppfV, aiDim[1], aiDim[1]);
        CMatrix::mulMatMat(ppfTmp, ppfW, ppfRes, aiDim[1], aiDim[1], aiDim[1], aiDim[1]);
        CMatrix::mulMatMat(ppfRes, ppfU, ppfTmp, aiDim[0], aiDim[1], aiDim[1], aiDim[1]);
        CMatrix::sub_I(ppfRes, ppfIn, aiDim[0], aiDim[1]);

        CHECK(0 == Approx(CMatrix::getSum(ppfRes, aiDim[0], aiDim[1])).margin(1e-4F).epsilon(1e-4F));
    }

    CMatrix::free(ppfIn, aiDim[0]);
    CMatrix::free(ppfU, aiDim[0]);
    CMatrix::free(ppfW, aiDim[1]);
    CMatrix::free(ppfV, aiDim[1]);
    CMatrix::free(ppfRes, aiDim[0]);
    CMatrix::free(ppfTmp, aiDim[0]);
}

TEST_CASE("ToolsResample", "[ToolsResample]")
{
    int iInputLength = 1026;
    int iOutputLength = 3*iInputLength;
    float* pfIn = new float[iInputLength];
    float* pfOut = new float[iOutputLength];

    float fInSampleRate = 1;
    float fOutSampleRate = 1.5;

    CResample* pCResample = new CResample(fInSampleRate, fOutSampleRate);
    
    CVector::setZero(pfIn, iInputLength);
    CVector::setZero(pfOut, iOutputLength);

    SECTION("Api")
    {
        CHECK(0 == pCResample->getOutputLength(0));
        CHECK(Error_t::kFunctionInvalidArgsError == pCResample->process(0, pfIn, iInputLength));
        CHECK(Error_t::kFunctionInvalidArgsError == pCResample->process(pfOut, 0, iInputLength));
        CHECK(Error_t::kFunctionInvalidArgsError == pCResample->process(pfOut, pfIn, 0));
    }

    SECTION("Zeros")
    {
        CVector::setValue(pfOut, 1.F, iOutputLength);
        CHECK(fOutSampleRate/fInSampleRate*iInputLength == pCResample->getOutputLength(iInputLength));
        CHECK(Error_t::kNoError == pCResample->process(pfOut, pfIn, iInputLength));
        CHECK(0.F == CVector::getSum(pfOut, pCResample->getOutputLength(iInputLength)));
        CHECK(1.F == CVector::getMean(&pfOut[pCResample->getOutputLength(iInputLength)], iOutputLength - pCResample->getOutputLength(iInputLength)));
    }

    SECTION("Sine")
    {
        //iOutputLength = 30;
        float* pfSine = 0;
        float fSampleRateScale = 3.F;
 
        delete pCResample;
        pCResample = new CResample(1.F, fSampleRateScale);

        CVector::alloc(pfSine, iOutputLength);
        CSynthesis::genSine(pfSine, 1.F, 1.F * iOutputLength, iOutputLength);

        for (auto i = 0; i < iInputLength; i++)
            pfIn[i] = pfSine[static_cast<int>(fSampleRateScale * i)];

         CHECK(Error_t::kNoError == pCResample->process(pfOut, pfIn, iInputLength));

        for (auto i = 0; i < iOutputLength; i++)
            CHECK(pfSine[i] == Approx(pfOut[i]).margin(1e-4F).epsilon(1e-4F));

        CVector::free(pfSine);
    }

    delete pCResample;
    delete[] pfIn;
    delete[] pfOut;
}

TEST_CASE("ToolsSinglePole", "[ToolsSinglePole]")
{

    CSinglePoleLp* pCLowPass = 0;
    float* pfInput = 0,
        * pfOut = 0;
    int iNumValues = 1024;

    CSinglePoleLp::create(pCLowPass);
    pfInput = new float[iNumValues];
    pfOut = new float[2 * iNumValues];

    CVector::setZero(pfInput, iNumValues);
    CVector::setZero(pfOut, iNumValues);

    SECTION("Api")
    {
        CHECK(Error_t::kFunctionInvalidArgsError == pCLowPass->setFilterParam(-1.F));
        CHECK(Error_t::kFunctionInvalidArgsError == pCLowPass->setFilterParam(1.F));
        CHECK(Error_t::kFunctionInvalidArgsError == pCLowPass->setFilterParam(1.1F));

        CHECK(Error_t::kNoError == pCLowPass->reset());

        CHECK(Error_t::kNoError == pCLowPass->setFilterParam(.01F));
        CHECK(.01F == Approx(pCLowPass->getFilterParam()).margin(1e-6F).epsilon(1e-6F));
        CHECK(Error_t::kNoError == pCLowPass->setFilterParam(.5F));
        CHECK(.5F == Approx(pCLowPass->getFilterParam()).margin(1e-6F).epsilon(1e-6F));
        CHECK(Error_t::kNoError == pCLowPass->setFilterParam(.99F));
        CHECK(.99F == Approx(pCLowPass->getFilterParam()).margin(1e-6F).epsilon(1e-6F));

        CHECK(Error_t::kFunctionInvalidArgsError == pCLowPass->process(0, pfInput, iNumValues));
        CHECK(Error_t::kFunctionInvalidArgsError == pCLowPass->process(pfOut, 0, iNumValues));
        CHECK(Error_t::kFunctionInvalidArgsError == pCLowPass->process(pfOut, pfInput, 0));
        CHECK(Error_t::kNoError == pCLowPass->process(pfOut, pfInput, iNumValues));
    }

    SECTION("Process")
    {
        // zeros
        for (auto i = 0; i < iNumValues; i++)
        {
            CHECK(Error_t::kNoError == pCLowPass->process(&pfOut[i], &pfInput[i], 1));
            CHECK(0.F == Approx(pfOut[i]).margin(1e-6F).epsilon(1e-6F));
        }

        // ones
        CVector::setValue(pfInput, 1.F, iNumValues);
        for (auto c = 0; c < 10; c++)
        {
            float fAlpha = c / 10.1F;
            pCLowPass->reset();
            pCLowPass->setFilterParam(fAlpha);
            for (auto i = 0; i < iNumValues; i++)
            {
                CHECK(Error_t::kNoError == pCLowPass->process(&pfOut[i], &pfInput[i], 1));
                CHECK(1.F - std::pow(pCLowPass->getFilterParam(), i + 1) == Approx(pfOut[i]).margin(1e-6F).epsilon(1e-6F));
            }
        }
        CHECK(0 < CSinglePoleLp::calcFilterParam(0.1F, 48000));
        CHECK(0.F == Approx(CSinglePoleLp::calcFilterParam(0, 48000)).margin(1e-3F).epsilon(1e-3F));
        CHECK(1.F == Approx(0 < CSinglePoleLp::calcFilterParam(1000000, 48000)).margin(1e-6F).epsilon(1e-6F));

    }
    CSinglePoleLp::destroy(pCLowPass);

    delete[] pfInput;
    delete[] pfOut;

}

TEST_CASE("ToolsViterbi", "[ToolsViterbi]")
{

    CViterbi* pCInstance = 0;
    float* pfStartProb = 0; 
    float** ppfEmission = 0;
    float** ppfTransProb = 0; 
    int* piPath = 0;
    float fResProb = .01512F;
    int aiResPath[3] = { 0,0,1 };

    int iNumStates = 2; 
    int iNumObs = 3;  

    pCInstance = new CViterbi();
    CVector::alloc(pfStartProb, iNumStates);
    CMatrix::alloc(ppfEmission, iNumStates, iNumObs);
    CMatrix::alloc(ppfTransProb, iNumStates, iNumStates);
    CVector::alloc(piPath, iNumObs);

    // states: healthy: 0, fever: 1
    // 
    // start prob: healthy: 0.6, fever: 0.4
    pfStartProb[0] = .6F; pfStartProb[1] = .4F;

    // trans prob: healthy->healthy: 0.7, healthy->fever: 0.3, fever->healthy: 0.4, fever->fever: 0.6
    ppfTransProb[0][0] = .7F; ppfTransProb[0][1] = .3F;
    ppfTransProb[1][0] = .4F; ppfTransProb[1][1] = .6F;

    // obs: normal: 0, cold: 1, dizzy: 2
    // emission prob : normal | healthy : 0.5, cold | healthy : 0.4, dizzy | healthy : 0.1
    //                 normal | fever: 0.1, cold | fever : 0.3, dizzy | fever : 0.6
    ppfEmission[0][0] = .5F; ppfEmission[0][1] = .4F; ppfEmission[0][2] = .1F;
    ppfEmission[1][0] = .1F; ppfEmission[1][1] = .3F; ppfEmission[1][2] = .6F;

    SECTION("Api")
    {
        CHECK(Error_t::kFunctionInvalidArgsError == pCInstance->init(0, pfStartProb, iNumStates, iNumObs));
        CHECK(Error_t::kFunctionInvalidArgsError == pCInstance->init(ppfTransProb, 0, iNumStates, iNumObs));
        CHECK(Error_t::kFunctionInvalidArgsError == pCInstance->init(ppfTransProb, pfStartProb, 0, iNumObs));
        CHECK(Error_t::kFunctionInvalidArgsError == pCInstance->init(ppfTransProb, pfStartProb, iNumStates, 0));

        CHECK(Error_t::kNoError == pCInstance->init(ppfTransProb, pfStartProb, iNumStates, iNumObs));

        CHECK(Error_t::kFunctionInvalidArgsError == pCInstance->compViterbi(0));
        CHECK(Error_t::kFunctionInvalidArgsError == pCInstance->getStateSequence(0));

        CHECK(Error_t::kNoError == pCInstance->reset());
    }

    SECTION("ProcessProb")
    {
        CHECK(Error_t::kNoError == pCInstance->init(ppfTransProb, pfStartProb, iNumStates, iNumObs));
        CHECK(Error_t::kNoError == pCInstance->compViterbi(ppfEmission, false));
        CHECK(Error_t::kNoError == pCInstance->getStateSequence(piPath));

        for (auto n = 0; n < iNumObs; n++)
            CHECK(aiResPath[n] == piPath[n]);

        CHECK(fResProb == Approx(pCInstance->getOverallProbability()).margin(1e-6F).epsilon(1e-6F));
    }

    SECTION("ProcessLog")
    {
        CHECK(Error_t::kNoError == pCInstance->init(ppfTransProb, pfStartProb, iNumStates, iNumObs));
        CHECK(Error_t::kNoError == pCInstance->compViterbi(ppfEmission));
        CHECK(Error_t::kNoError == pCInstance->getStateSequence(piPath));

        for (auto n = 0; n < iNumObs; n++)
            CHECK(aiResPath[n] == piPath[n]);

        CHECK(std::log(fResProb) == Approx(pCInstance->getOverallProbability()).margin(1e-6F).epsilon(1e-6F));
    }

    delete pCInstance ;
    CVector::free(pfStartProb);
    CMatrix::free(ppfEmission, iNumStates);
    CMatrix::free(ppfTransProb, iNumStates);
    CVector::free(piPath);
}

#endif //WITH_TESTS
