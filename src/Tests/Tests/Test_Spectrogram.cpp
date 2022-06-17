#include "AcaAll.h"

#ifdef WITH_TESTS
#include "catch.hpp"

TEST_CASE("Spectrogram", "[Spectrogram]")
{

    CSpectrogramIf* pCSpecGram = 0;

    float** ppfSpecGram = 0;
    float* pfIn = 0;
    float* pfTimeStamps = 0;

    int  iBlockLength = 1024,
        iHopLength = 512;

    float fSampleRate = 40000,
        f0 = 400;

    int     aiSpecGramDimension[2] = { 0, 0 };

    pfIn = new float[static_cast<int>(fSampleRate)];
    pfTimeStamps = new float[static_cast<int>(fSampleRate)];

    SECTION("Api")
    {
        CHECK_FALSE(Error_t::kNoError == CSpectrogramIf::create(pCSpecGram, static_cast<float*>(0), 1, fSampleRate, iBlockLength, iHopLength));
        CHECK_FALSE(Error_t::kNoError == CSpectrogramIf::create(pCSpecGram, pfIn, 0, fSampleRate, iBlockLength, iHopLength));
        CHECK_FALSE(Error_t::kNoError == CSpectrogramIf::create(pCSpecGram, pfIn, 1, 0, iBlockLength, iHopLength));
        CHECK_FALSE(Error_t::kNoError == CSpectrogramIf::create(pCSpecGram, pfIn, 1, fSampleRate, 0, iHopLength));
        CHECK_FALSE(Error_t::kNoError == CSpectrogramIf::create(pCSpecGram, pfIn, 1, fSampleRate, iBlockLength, 0));
        CHECK_FALSE(Error_t::kNoError == CSpectrogramIf::create(pCSpecGram, pfIn, 1, -1, iBlockLength, iHopLength));
        CHECK_FALSE(Error_t::kNoError == CSpectrogramIf::create(pCSpecGram, pfIn, 1, fSampleRate, -1, iHopLength));
        CHECK_FALSE(Error_t::kNoError == CSpectrogramIf::create(pCSpecGram, pfIn, 1, fSampleRate, iBlockLength, -1));
        CHECK_FALSE(Error_t::kNoError == CSpectrogramIf::create(pCSpecGram, pfIn, 1, fSampleRate, 3, iHopLength));

        CHECK(Error_t::kNoError ==CSpectrogramIf::create(pCSpecGram, pfIn, 5, fSampleRate, iBlockLength, iHopLength));
        CHECK_FALSE(Error_t::kNoError == pCSpecGram->getSpectrogramAxisVectors(0, CSpectrogramIf::kFrequencyInHz));
        CHECK_FALSE(Error_t::kNoError == pCSpecGram->getMelSpectrogramAxisVectors(pfIn, CSpectrogramIf::kFrequencyInHz, 0));
        CHECK(Error_t::kNoError ==CSpectrogramIf::destroy(pCSpecGram));
    }


    SECTION("Dimensions")
    {
        float* pfVector = 0;

        f0 = 400;

        CSynthesis::genSine(pfIn, f0, fSampleRate, static_cast<int>(fSampleRate));

        CHECK(Error_t::kNoError == CSpectrogramIf::create(pCSpecGram, pfIn, static_cast<int>(fSampleRate), fSampleRate, iBlockLength, iHopLength));

        CHECK(Error_t::kNoError == pCSpecGram->getSpectrogramDimensions(aiSpecGramDimension[0], aiSpecGramDimension[1]));

        CHECK(iBlockLength / 2 + 1 == aiSpecGramDimension[0]);
        CHECK(static_cast<int>(fSampleRate) / iHopLength + 1 == aiSpecGramDimension[1]);

        pfVector = new float[std::max(aiSpecGramDimension[0], aiSpecGramDimension[1])];

        CHECK(Error_t::kNoError == pCSpecGram->getSpectrogramAxisVectors(pfVector, CSpectrogramIf::kFrequencyInHz));
        CHECK(pfVector[0] == 0);
        CHECK(pfVector[1] == Approx( fSampleRate / iBlockLength).margin(1e-6F).epsilon(1e-6F));
        CHECK(pfVector[iBlockLength / 2] == Approx(fSampleRate / 2).margin(1e-6F).epsilon(1e-6F));

        CHECK(Error_t::kNoError == pCSpecGram->getSpectrogramAxisVectors(pfVector, CSpectrogramIf::kTimeInS));
        CHECK(pfVector[0] == Approx( iBlockLength / (2 * fSampleRate)).margin(1e-6F).epsilon(1e-6F));
        CHECK(pfVector[1] == Approx( pfVector[0] + iHopLength / fSampleRate).margin(1e-6F).epsilon(1e-6F));

        CHECK(Error_t::kNoError == CSpectrogramIf::destroy(pCSpecGram));

        delete[] pfVector;
    }

    SECTION("Values")
    {
        f0 = 80;
        fSampleRate = 8192;
        iBlockLength = 1024;
        iHopLength = 512;

        CSynthesis::genSine(pfIn, f0, fSampleRate, static_cast<int>(fSampleRate));

        CHECK(Error_t::kNoError == CSpectrogramIf::create(pCSpecGram, pfIn, 40000, fSampleRate, iBlockLength, iHopLength));

        CHECK(Error_t::kNoError == pCSpecGram->getSpectrogramDimensions(aiSpecGramDimension[0], aiSpecGramDimension[1]));

        ppfSpecGram = new float* [aiSpecGramDimension[0]];
        for (auto k = 0; k < aiSpecGramDimension[0]; k++)
            ppfSpecGram[k] = new float[aiSpecGramDimension[1]];

        pCSpecGram->compSpectrogram(ppfSpecGram);

        REQUIRE(aiSpecGramDimension[0] >= 117);
        REQUIRE(aiSpecGramDimension[1] >= 11);
        CHECK(ppfSpecGram[10][10] - ppfSpecGram[20][10] == Approx(ppfSpecGram[10][10]).margin(1e-4F).epsilon(1e-4F));
        CHECK(ppfSpecGram[115][10] == Approx(0).margin(1e-6F).epsilon(1e-6F));
        CHECK(ppfSpecGram[116][10] == Approx(0).margin(1e-6F).epsilon(1e-6F));

        CHECK(Error_t::kNoError == CSpectrogramIf::destroy(pCSpecGram));

        for (auto k = 0; k < aiSpecGramDimension[0]; k++)
            delete[] ppfSpecGram[k];
        delete[] ppfSpecGram;

        f0 = 84;
        fSampleRate = 8192;
        iBlockLength = 1024;
        iHopLength = 512;

        CSynthesis::genSine(pfIn, f0, fSampleRate, static_cast<int>(fSampleRate));

        CHECK(Error_t::kNoError == CSpectrogramIf::create(pCSpecGram, pfIn, static_cast<int>(fSampleRate), fSampleRate, iBlockLength, iHopLength));

        CHECK(Error_t::kNoError == pCSpecGram->getSpectrogramDimensions(aiSpecGramDimension[0], aiSpecGramDimension[1]));

        ppfSpecGram = new float* [aiSpecGramDimension[0]];
        for (auto k = 0; k < aiSpecGramDimension[0]; k++)
            ppfSpecGram[k] = new float[aiSpecGramDimension[1]];

        pCSpecGram->compSpectrogram(ppfSpecGram);

        CHECK(ppfSpecGram[10][10] - ppfSpecGram[11][10] == Approx(0).margin(1e-4F).epsilon(1e-4F));

        CHECK(Error_t::kNoError == CSpectrogramIf::destroy(pCSpecGram));

        for (auto k = 0; k < aiSpecGramDimension[0]; k++)
            delete[] ppfSpecGram[k];
        delete[] ppfSpecGram;

        f0 = 4;
        fSampleRate = 16;
        float fAmp = .5;
        iBlockLength = 16;
        iHopLength = 16;

        //reuse buffer for window setting
        CVector::setValue(pfTimeStamps, 1.F, iBlockLength);

        CSynthesis::genSine(pfIn, f0, fSampleRate, static_cast<int>(fSampleRate), fAmp);

        CHECK(Error_t::kNoError == CSpectrogramIf::create(pCSpecGram, pfIn, static_cast<int>(fSampleRate), fSampleRate, iBlockLength, iHopLength, false, pfTimeStamps));

        CHECK(Error_t::kNoError == pCSpecGram->getSpectrogramDimensions(aiSpecGramDimension[0], aiSpecGramDimension[1]));

        ppfSpecGram = new float* [aiSpecGramDimension[0]];
        for (auto k = 0; k < aiSpecGramDimension[0]; k++)
            ppfSpecGram[k] = new float[aiSpecGramDimension[1]];

        pCSpecGram->compSpectrogram(ppfSpecGram);

        CHECK(ppfSpecGram[4][0] == Approx(fAmp).margin(1e-6F).epsilon(1e-6F));
        CHECK(ppfSpecGram[0][0] == Approx(0).margin(1e-4F).epsilon(1e-4F));

        CHECK(Error_t::kNoError == CSpectrogramIf::destroy(pCSpecGram));
        CHECK(Error_t::kNoError == CSpectrogramIf::create(pCSpecGram, pfIn, static_cast<int>(fSampleRate), fSampleRate, iBlockLength, iHopLength, true, pfTimeStamps));

        pCSpecGram->compSpectrogram(ppfSpecGram);

        CHECK(ppfSpecGram[4][0] == Approx(1).margin(1e-6F).epsilon(1e-6F));
        CHECK(ppfSpecGram[0][0] == Approx(0).margin(1e-6F).epsilon(1e-6F));

        CHECK(Error_t::kNoError == CSpectrogramIf::destroy(pCSpecGram));

        for (auto k = 0; k < aiSpecGramDimension[0]; k++)
            delete[] ppfSpecGram[k];
        delete[] ppfSpecGram;
        ppfSpecGram = 0;
    }

    SECTION("MelSpectrogram")
    {
        CSpectrogramIf::MelSpectrogramConfig_t stMelSpecConfig;
        float* pfVector = 0;

        f0 = 400;

        stMelSpecConfig.bIsLogarithmic = false;
        stMelSpecConfig.fMaxFreqInHz = 20000;
        stMelSpecConfig.fMinFreqInHz = 0;
        stMelSpecConfig.iNumMelBins = 128;

        CSynthesis::genSine(pfIn, f0, fSampleRate, static_cast<int>(fSampleRate));

        CHECK(Error_t::kNoError == CSpectrogramIf::create(pCSpecGram, pfIn, static_cast<long long>(fSampleRate), fSampleRate, iBlockLength, iHopLength));

        CHECK(Error_t::kNoError == pCSpecGram->getMelSpectrogramDimensions(aiSpecGramDimension[0], aiSpecGramDimension[1], &stMelSpecConfig));

        CHECK(stMelSpecConfig.iNumMelBins == aiSpecGramDimension[0]);
        CHECK(static_cast<int>(fSampleRate) / iHopLength + 1 == aiSpecGramDimension[1]);

        pfVector = new float[std::max(aiSpecGramDimension[0], aiSpecGramDimension[1])];

        CHECK(Error_t::kNoError == pCSpecGram->getMelSpectrogramAxisVectors(pfVector, CSpectrogramIf::kFrequencyInHz, &stMelSpecConfig));

        CHECK(Error_t::kNoError == pCSpecGram->getMelSpectrogramAxisVectors(pfVector, CSpectrogramIf::kTimeInS, &stMelSpecConfig));
        CHECK(pfVector[0] == Approx( iBlockLength / (2 * fSampleRate)).margin(1e-6F).epsilon(1e-6F));
        CHECK(pfVector[1] == Approx( pfVector[0] + iHopLength / fSampleRate).margin(1e-6F).epsilon(1e-6F));

        ppfSpecGram = new float* [aiSpecGramDimension[0]];
        for (auto k = 0; k < aiSpecGramDimension[0]; k++)
            ppfSpecGram[k] = new float[aiSpecGramDimension[1]];

        stMelSpecConfig.bIsLogarithmic = true;
        pCSpecGram->compMelSpectrogram(ppfSpecGram, &stMelSpecConfig);

        CHECK(-36.9F == Approx(ppfSpecGram[13][1]).margin(1e-1F).epsilon(1e-1F));

        delete[] pfVector;
    }
    if (ppfSpecGram)
    {
        for (int i = 0; i < aiSpecGramDimension[0]; i++)
        {
            delete[] ppfSpecGram[i];
        }
        delete[] ppfSpecGram;
    }

    delete[] pfIn;
    pfIn = 0;

    CSpectrogramIf::destroy(pCSpecGram);

    delete[] pfTimeStamps;

}

#endif //WITH_TESTS
