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

#endif //WITH_TESTS
