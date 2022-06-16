#include "AcaAll.h"

#ifdef WITH_TESTS
#include "catch.hpp"

TEST_CASE("Pitch tracking (class interface per block)", "[PitchBlockClass]")
{
    CPitchFromBlockIf* pCInstance = 0;

    float* pfIn = 0;

    int iBlockLength = 4096;
    float fSampleRate = 4096;
    //float fResult = -1.F;

    CVector::alloc(pfIn, iBlockLength);

    SECTION("Api")
    {
        for (auto j = 0; j < CPitchIf::kNumPitchExtractors; j++)
        {
            CHECK(Error_t::kFunctionInvalidArgsError == CPitchFromBlockIf::create(pCInstance, static_cast<CPitchIf::PitchExtractors_t>(j), 0, fSampleRate));
            CHECK(Error_t::kFunctionInvalidArgsError == CPitchFromBlockIf::create(pCInstance, static_cast<CPitchIf::PitchExtractors_t>(j), -1, fSampleRate));
            CHECK(Error_t::kFunctionInvalidArgsError == CPitchFromBlockIf::create(pCInstance, static_cast<CPitchIf::PitchExtractors_t>(j), iBlockLength, 0));

            CHECK(Error_t::kNoError == CPitchFromBlockIf::create(pCInstance, static_cast<CPitchIf::PitchExtractors_t>(j), iBlockLength, fSampleRate));

            CHECK_FALSE(pCInstance == 0);

            CHECK(static_cast<CPitchIf::PitchExtractors_t>(j) == pCInstance->getPitchExtractorIdx());

            CHECK(Error_t::kNoError == CPitchFromBlockIf::destroy(pCInstance));
        }
    }

    SECTION("SpectralAcf")
    {
        fSampleRate = 48000;
        iBlockLength = 2049;
        CHECK(Error_t::kNoError == CPitchFromBlockIf::create(pCInstance, CPitchIf::kPitchSpectralAcf, iBlockLength, fSampleRate));

        // zero input
        CHECK(0.F == pCInstance->compF0(pfIn));

        // sine 
        int iIdx = 30;
        pfIn[iIdx] = 1.F;
        CHECK(iIdx * .5 * fSampleRate / (iBlockLength - 1) == pCInstance->compF0(pfIn));

        // harmonics
        pfIn[iIdx] = 1.F;
        pfIn[2 * iIdx] = .5F;
        pfIn[3 * iIdx] = .25F;
        pfIn[4 * iIdx] = .125F;
        CHECK(iIdx * .5 * fSampleRate / (iBlockLength - 1) == pCInstance->compF0(pfIn));

    }

    SECTION("SpectralHps")
    {
        fSampleRate = 48000;
        iBlockLength = 2049;
        CHECK(Error_t::kNoError == CPitchFromBlockIf::create(pCInstance, CPitchIf::kPitchSpectralHps, iBlockLength, fSampleRate));

        // zero input
        CHECK(0.F == pCInstance->compF0(pfIn));

        // sine 
        int iIdx = 30;
        pfIn[iIdx] = 1.F;
        CHECK(0.F == pCInstance->compF0(pfIn));

        // harmonics
        pfIn[iIdx] = 1.F;
        pfIn[2 * iIdx] = .5F;
        pfIn[3 * iIdx] = .25F;
        pfIn[4 * iIdx] = .125F;
        CHECK(iIdx * .5 * fSampleRate / (iBlockLength - 1) == pCInstance->compF0(pfIn));

    }

    SECTION("TimeAcf")
    {
        CHECK(Error_t::kNoError == CPitchFromBlockIf::create(pCInstance, CPitchIf::kPitchTimeAcf, iBlockLength, fSampleRate));

        // zero input
        CHECK(0.F == pCInstance->compF0(pfIn));
        
        // sine 
        float fFreq = 16.F;
        CSynthesis::genSine(pfIn, fFreq, fSampleRate, iBlockLength);

        CHECK(fFreq == pCInstance->compF0(pfIn));

        // T0 between bins (T0 = 511.5)
        fFreq = 8.0078201369F;
        CSynthesis::genSine(pfIn, fFreq, fSampleRate, iBlockLength);

        CHECK(((fSampleRate/511 == pCInstance->compF0(pfIn)) || (fSampleRate / 512 == pCInstance->compF0(pfIn))));
    }

    SECTION("TimeAmdf")
    {
        fSampleRate = 40960;
        CHECK(Error_t::kNoError == CPitchFromBlockIf::create(pCInstance, CPitchIf::kPitchTimeAmdf, iBlockLength, fSampleRate));

        // zero input
        CHECK(0.F == pCInstance->compF0(pfIn));

        // sine 
        float fFreq = 160.F;
        CSynthesis::genSine(pfIn, fFreq, fSampleRate, iBlockLength);

        CHECK(((fFreq == pCInstance->compF0(pfIn)) ||
            (fFreq / 2 == pCInstance->compF0(pfIn)) ||
            (fFreq / 3 == pCInstance->compF0(pfIn))));
    }

    SECTION("TimeAuditory")
    {
        CHECK(Error_t::kNoError == CPitchFromBlockIf::create(pCInstance, CPitchIf::kPitchTimeAuditory, iBlockLength, fSampleRate));

        // zero input
        CHECK(0.F == pCInstance->compF0(pfIn));

        // sine 
        float fFreq = 16.F;
        CSynthesis::genSine(pfIn, fFreq, fSampleRate, iBlockLength);

        CHECK(fFreq == pCInstance->compF0(pfIn));

        // T0 between bins (T0 = 511.5)
        fFreq = 8.0078201369F;
        CSynthesis::genSine(pfIn, fFreq, fSampleRate, iBlockLength);

        CHECK(((fSampleRate / 511 == pCInstance->compF0(pfIn)) || (fSampleRate / 512 == pCInstance->compF0(pfIn))));
    }

    SECTION("TimeZeroCrossings")
    {
        fSampleRate = 40960;
        CHECK(Error_t::kNoError == CPitchFromBlockIf::create(pCInstance, CPitchIf::kPitchTimeZeroCrossings, iBlockLength, fSampleRate));

        // zero input
        CHECK(0.F == pCInstance->compF0(pfIn));

        // sine 
        float fFreq = 160.F;
        CSynthesis::genSine(pfIn, fFreq, fSampleRate, iBlockLength, 1.F, 3.13F);

        CHECK(fFreq == pCInstance->compF0(pfIn));
    }

    CHECK(Error_t::kNoError == CPitchFromBlockIf::destroy(pCInstance));

    CVector::free(pfIn);
}


TEST_CASE("Pitch (per array)", "[PitchClass]")
{
    CPitchIf* pCInstance = 0;
    float* pfIn = 0;
    float* pfPitch = 0;
    float fSampleRate = 44100;
    int iBlockLength = 4096,
        iHopLength = 512,
        iLenBuff = 96000;
    int iDim = 0;

    CVector::alloc(pfPitch, 188);
    CVector::alloc(pfIn, iLenBuff);

    SECTION("Api")
    {
        for (auto f = 0; f < CPitchIf::kNumPitchExtractors; f++)
        {
            CHECK(Error_t::kFunctionInvalidArgsError == CPitchIf::create(pCInstance, static_cast<CPitchIf::PitchExtractors_t>(f), 0, iLenBuff, fSampleRate, iBlockLength, iHopLength));
            CHECK(Error_t::kFunctionInvalidArgsError == CPitchIf::create(pCInstance, static_cast<CPitchIf::PitchExtractors_t>(f), pfIn, 0, fSampleRate, iBlockLength, iHopLength));
            CHECK(Error_t::kFunctionInvalidArgsError == CPitchIf::create(pCInstance, static_cast<CPitchIf::PitchExtractors_t>(f), pfIn, -1, fSampleRate, iBlockLength, iHopLength));
            CHECK(Error_t::kFunctionInvalidArgsError == CPitchIf::create(pCInstance, static_cast<CPitchIf::PitchExtractors_t>(f), pfIn, iLenBuff, 0, iBlockLength, iHopLength));
            CHECK(Error_t::kFunctionInvalidArgsError == CPitchIf::create(pCInstance, static_cast<CPitchIf::PitchExtractors_t>(f), pfIn, iLenBuff, fSampleRate, 0, iHopLength));
            CHECK(Error_t::kFunctionInvalidArgsError == CPitchIf::create(pCInstance, static_cast<CPitchIf::PitchExtractors_t>(f), pfIn, iLenBuff, fSampleRate, iBlockLength, 0));

            CHECK(Error_t::kNoError == CPitchIf::create(pCInstance, static_cast<CPitchIf::PitchExtractors_t>(f), pfIn, iLenBuff, fSampleRate, iBlockLength, iHopLength));

            CHECK_FALSE(pCInstance == 0);

            CHECK(188 == pCInstance->getNumBlocks());
            CHECK(Error_t::kNoError == pCInstance->getNumBlocks(iDim));
            CHECK(188 == iDim);

            CHECK(Error_t::kFunctionInvalidArgsError == pCInstance->compF0(0));

            CHECK(Error_t::kNoError == CPitchIf::destroy(pCInstance));

        }
    }

    SECTION("TimeAcf")
    {
        float fFreq = 441.F;
        CSynthesis::genSine(pfIn, fFreq, fSampleRate, iLenBuff);

        CHECK(Error_t::kNoError == CPitchIf::create(pCInstance, CPitchIf::kPitchTimeAcf, pfIn, iLenBuff, fSampleRate, iBlockLength, iHopLength));
        CHECK_FALSE(pCInstance == 0);

        CHECK(Error_t::kNoError == pCInstance->getNumBlocks(iDim));

        CHECK(Error_t::kNoError == pCInstance->compF0(pfPitch));

        for (auto n = 0; n < iDim; n++)
            CHECK(fFreq == Approx(pfPitch[n]).margin(1e-6F).epsilon(1e-6F));
    }

    SECTION("TimeAuditory")
    {
        float fFreq = 441.F;
        CSynthesis::genSine(pfIn, fFreq, fSampleRate, iLenBuff);

        CHECK(Error_t::kNoError == CPitchIf::create(pCInstance, CPitchIf::kPitchTimeAuditory, pfIn, iLenBuff, fSampleRate, iBlockLength, iHopLength));
        CHECK_FALSE(pCInstance == 0);

        CHECK(Error_t::kNoError == pCInstance->getNumBlocks(iDim));

        CHECK(Error_t::kNoError == pCInstance->compF0(pfPitch));

        for (auto n = 0; n < iDim; n++)
            CHECK(fFreq == Approx(pfPitch[n]).margin(1e-6F).epsilon(1e-6F));
    }

    SECTION("SpectralAcf")
    {
        float fFreq = 441.43F;
        CSynthesis::genSine(pfIn, fFreq, fSampleRate, iLenBuff);

        CHECK(Error_t::kNoError == CPitchIf::create(pCInstance, CPitchIf::kPitchSpectralAcf, pfIn, iLenBuff, fSampleRate, iBlockLength, iHopLength));
        CHECK_FALSE(pCInstance == 0);

        CHECK(Error_t::kNoError == pCInstance->getNumBlocks(iDim));

        CHECK(Error_t::kNoError == pCInstance->compF0(pfPitch));

        for (auto n = 0; n < iDim-5; n++)
            CHECK(fFreq == Approx(pfPitch[n]).margin(1e-3F).epsilon(1e-3F));
    }

    SECTION("SpectralHps")
    {
        float fFreq = 441.43F;
        float* pfTmp = 0;
        CVector::alloc(pfTmp, iLenBuff);

        for (auto k = 1; k <= 4; k++)
        {
            CSynthesis::genSine(pfTmp, k*fFreq, fSampleRate, iLenBuff, 1.F/k);
            CVector::add_I(pfIn, pfTmp, iLenBuff);
        }

        CHECK(Error_t::kNoError == CPitchIf::create(pCInstance, CPitchIf::kPitchSpectralAcf, pfIn, iLenBuff, fSampleRate, iBlockLength, iHopLength));
        CHECK_FALSE(pCInstance == 0);

        CHECK(Error_t::kNoError == pCInstance->getNumBlocks(iDim));

        CHECK(Error_t::kNoError == pCInstance->compF0(pfPitch));

        for (auto n = 0; n < iDim-5; n++)
            CHECK(fFreq == Approx(pfPitch[n]).margin(1e-3F).epsilon(1e-3F));

        CVector::free(pfTmp);
    }

    CVector::free(pfIn);
    CVector::free(pfPitch);
    CHECK(Error_t::kNoError == CPitchIf::destroy(pCInstance));
}

#endif //WITH_TESTS
