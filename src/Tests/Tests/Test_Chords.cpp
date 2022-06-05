#include "ACAConfig.h"

#ifdef WITH_TESTS

#include "Vector.h"
#include "Synthesis.h"
#include "ToolConversion.h"

#include "Chord.h"
#include "ChordFromBlock.h"
#include "catch.hpp"



TEST_CASE("Chord (per block)", "[ChordsBlock]")
{
    CChordFromBlockIf* pCInstance = 0;
    float* pfResult = 0;
    float* pfInput = 0;
    float fSampleRate = 1;
    int iBufferLength = 2049;
    
    CVector::alloc(pfInput, iBufferLength);
    CVector::alloc(pfResult, CChordIf::kNumChords);

    SECTION("Api")
    {
        CHECK(Error_t::kFunctionInvalidArgsError == CChordFromBlockIf::create(pCInstance, 0, fSampleRate));
        CHECK(Error_t::kFunctionInvalidArgsError == CChordFromBlockIf::create(pCInstance, iBufferLength, 0));

        CHECK(Error_t::kNoError == CChordFromBlockIf::create(pCInstance, iBufferLength, fSampleRate));

        CHECK(Error_t::kNoError == CChordFromBlockIf::destroy(pCInstance));
    }

 
    SECTION("Zeros")
    {
        fSampleRate = 32000;

        CHECK(Error_t::kNoError == CChordFromBlockIf::create(pCInstance, iBufferLength, fSampleRate));
        CHECK(Error_t::kNoError == pCInstance->compChordProb(pfResult, pfInput));

        CHECK(1.F == Approx(pfResult[CChordIf::kNoChord]).margin(1e-6F).epsilon(1e-6F));

    }

    SECTION("ChordTemplates")
    {
        fSampleRate = 32000;

        float afChordPitches[2][3] = {  { 60, 64, 67 },
                                        { 60, 63, 67 } };

        CHECK(Error_t::kNoError == CChordFromBlockIf::create(pCInstance, iBufferLength, fSampleRate));

        for (auto i = 0; i < 2; i++)
        {
            for (auto c = 0; c < 12; c++)
            {
                float fMax = 0;
                long long iMax = 0;
                float afFreq[3] = { 0 };
                float afBin[3] = { 0 };
                CConversion::convertMidi2Freq(afFreq, afChordPitches[i], 3);
                CConversion::convertFreq2Bin(afBin, afFreq, 3, 2 * (iBufferLength - 1), fSampleRate);
                CVectorFloat::setZero(pfInput, iBufferLength);

                for (auto p = 0; p < 3; p++)
                    pfInput[CUtil::float2int<int>(afBin[p])] = 1.F;

                CHECK(Error_t::kNoError == pCInstance->compChordProb(pfResult, pfInput));

                CVectorFloat::findMax(pfResult, fMax, iMax, CChordIf::kNumChords);
                CHECK(iMax == i*12+c);

                CVectorFloat::addC_I(afChordPitches[i], 1.F, 3);
            }
        }

    }

    CChordFromBlockIf::destroy(pCInstance);

    CVector::free(pfInput);
    CVector::free(pfResult);
}


TEST_CASE("Chord (per file)", "[ChordsClass]")
{
    CChordIf* pCInstance = 0;
    CChordIf::Chords_t* peResult = 0;
    float* pfInput = 0;
    float fSampleRate = 44100;
    int iBufferLength = 176400;
    int iBlockLength = 4096;
    int iHopLength = 1024;

    CVector::alloc(pfInput, iBufferLength);
    CVector::alloc(peResult, 173);

    SECTION("Api")
    {
        CHECK(Error_t::kFunctionInvalidArgsError == CChordIf::create(pCInstance, 0, iBufferLength, fSampleRate, iBlockLength, iHopLength));
        CHECK(Error_t::kFunctionInvalidArgsError == CChordIf::create(pCInstance, pfInput, 0, fSampleRate, iBlockLength, iHopLength));
        CHECK(Error_t::kFunctionInvalidArgsError == CChordIf::create(pCInstance, pfInput, iBufferLength, 0, iBlockLength, iHopLength));
        CHECK(Error_t::kFunctionInvalidArgsError == CChordIf::create(pCInstance, pfInput, iBufferLength, fSampleRate, 0, iHopLength));

        CHECK(Error_t::kNoError == CChordIf::create(pCInstance, pfInput, iBufferLength, fSampleRate, iBlockLength, iHopLength));

        CHECK(pCInstance);
        CHECK(173 == pCInstance->getNumBlocks());
        CHECK(Error_t::kNoError == CChordIf::destroy(pCInstance));
    }

    SECTION("Zeros")
    {
        CHECK(Error_t::kNoError == CChordIf::create(pCInstance, pfInput, iBufferLength, fSampleRate, iBlockLength, iHopLength));
        CHECK(Error_t::kNoError == pCInstance->compChords(peResult));

        for (auto n = 0; n < 173; n++)
            CHECK(CChordIf::kNoChord == peResult[n]);
    }

    SECTION("SineChords")
    {
        float* pfTmp = 0;
        CVector::alloc(pfTmp, CUtil::float2int<int>(fSampleRate));
        float afChordPitches[4][3] = { { 60, 64, 67 },
                                        { 65, 69, 72 },
                                        { 67, 71, 74 },
                                        { 60, 63, 67 } };
        for (auto n = 0; n < 4; n++)
        {
            for (auto l = 0; l < 3; l++)
            {
                CSynthesis::genSine(pfTmp,  CConversion::convertMidi2Freq(12 + afChordPitches[n][l]), fSampleRate, CUtil::float2int<int>(fSampleRate));
                CVectorFloat::add_I(&pfInput[n * CUtil::float2int<int>(fSampleRate)], pfTmp, CUtil::float2int<int>(fSampleRate));
            }
        }

        CHECK(Error_t::kNoError == CChordIf::create(pCInstance, pfInput, iBufferLength, fSampleRate, iBlockLength, iHopLength));
        CHECK(Error_t::kNoError == pCInstance->compChords(peResult, false));

        CHECK(CChordIf::kCMajor == peResult[10]);
        CHECK(CChordIf::kFMajor == peResult[50]);
        CHECK(CChordIf::kGMajor == peResult[100]);
        CHECK(CChordIf::kCMinor == peResult[150]);

        CChordIf::destroy(pCInstance);
        CHECK(Error_t::kNoError == CChordIf::create(pCInstance, pfInput, iBufferLength, fSampleRate, iBlockLength, iHopLength));
        CHECK(Error_t::kNoError == pCInstance->compChords(peResult));

        CHECK(CChordIf::kCMajor == peResult[10]);
        CHECK(CChordIf::kFMajor == peResult[50]);
        CHECK(CChordIf::kGMajor == peResult[100]);
        CHECK(CChordIf::kCMinor == peResult[150]);

        CVector::free(pfTmp);
    }

    SECTION("Smoothing")
    {
        float* pfTmp = 0;
        CVector::alloc(pfTmp, iBufferLength);
        float afChordPitches[2][3] = { { 60, 64, 67 },
                                        { 66, 70, 73 }};
        int iDisruptionLength = 2048;

        iBufferLength = 60000;

        for (auto l = 0; l < 3; l++)
        {
            CSynthesis::genSine(pfTmp, CConversion::convertMidi2Freq(12 + afChordPitches[0][l]), fSampleRate, iBufferLength, .5F);
            CVectorFloat::add_I(pfInput, pfTmp, iBufferLength);
        }

        //CVectorFloat::setZero(&pfInput[30000], iDisruptionLength);
        for (auto l = 0; l < 3; l++)
        {
            CSynthesis::genSine(pfTmp, CConversion::convertMidi2Freq(12 + afChordPitches[1][l]), fSampleRate, iDisruptionLength);
            CVectorFloat::add_I(&pfInput[30000], pfTmp, iDisruptionLength);
        }


        CHECK(Error_t::kNoError == CChordIf::create(pCInstance, pfInput, iBufferLength, fSampleRate, iBlockLength, iHopLength));
        CHECK(Error_t::kNoError == pCInstance->compChords(peResult, false));


        for (auto n = 0; n < 55; n++)
        {
            if (n != 28 && n != 29)
                CHECK(CChordIf::kCMajor == peResult[n]);
            else
                CHECK(CChordIf::kFsMajor == peResult[n]);
        }
        
        CChordIf::destroy(pCInstance);
        CHECK(Error_t::kNoError == CChordIf::create(pCInstance, pfInput, iBufferLength, fSampleRate, iBlockLength, iHopLength));
        CHECK(Error_t::kNoError == pCInstance->compChords(peResult));

        for (auto n = 0; n < pCInstance->getNumBlocks(); n++)
            CHECK(CChordIf::kCMajor == peResult[n]);

        CVector::free(pfTmp);
    }


    CChordIf::destroy(pCInstance);

    CVector::free(pfInput);
    CVector::free(peResult);
}

#endif //WITH_TESTS
