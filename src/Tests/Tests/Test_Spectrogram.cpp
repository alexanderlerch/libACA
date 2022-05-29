#include "ACAConfig.h"

#ifdef WITH_TESTS

#include "Synthesis.h"
#include "Vector.h"
#include "Spectrogram.h"

#include "catch.hpp"

TEST_CASE("Spectrogram", "[Spectrogram]")
{

    CSpectrogramIf* pCSpecGram = 0;

    float** ppfSpecGram = 0;
    float* pfInput = 0;
    float* pfTimeStamps = 0;

    int  iBlockLength = 1024,
        iHopLength = 512;

    float fs = 40000,
        f0 = 400;

    int     aiSpecGramDimension[2] = { 0, 0 };

    pfInput = new float[static_cast<int>(fs)];
    pfTimeStamps = new float[static_cast<int>(fs)];

    SECTION("Api")
    {
        CHECK_FALSE(Error_t::kNoError == CSpectrogramIf::create(pCSpecGram, static_cast<float*>(0), 1, fs, iBlockLength, iHopLength));
        CHECK_FALSE(Error_t::kNoError == CSpectrogramIf::create(pCSpecGram, pfInput, 0, fs, iBlockLength, iHopLength));
        CHECK_FALSE(Error_t::kNoError == CSpectrogramIf::create(pCSpecGram, pfInput, 1, 0, iBlockLength, iHopLength));
        CHECK_FALSE(Error_t::kNoError == CSpectrogramIf::create(pCSpecGram, pfInput, 1, fs, 0, iHopLength));
        CHECK_FALSE(Error_t::kNoError == CSpectrogramIf::create(pCSpecGram, pfInput, 1, fs, iBlockLength, 0));
        CHECK_FALSE(Error_t::kNoError == CSpectrogramIf::create(pCSpecGram, pfInput, 1, -1, iBlockLength, iHopLength));
        CHECK_FALSE(Error_t::kNoError == CSpectrogramIf::create(pCSpecGram, pfInput, 1, fs, -1, iHopLength));
        CHECK_FALSE(Error_t::kNoError == CSpectrogramIf::create(pCSpecGram, pfInput, 1, fs, iBlockLength, -1));
        CHECK_FALSE(Error_t::kNoError == CSpectrogramIf::create(pCSpecGram, pfInput, 1, fs, 3, iHopLength));

        CHECK(Error_t::kNoError ==CSpectrogramIf::create(pCSpecGram, pfInput, 5, fs, iBlockLength, iHopLength));
        CHECK_FALSE(Error_t::kNoError == pCSpecGram->getSpectrogramAxisVectors(0, CSpectrogramIf::kFrequencyInHz));
        CHECK_FALSE(Error_t::kNoError == pCSpecGram->getMelSpectrogramAxisVectors(pfInput, CSpectrogramIf::kFrequencyInHz, 0));
        CHECK(Error_t::kNoError ==CSpectrogramIf::destroy(pCSpecGram));
    }


    SECTION("Dimensions")
    {
        float* pfVector = 0;

        f0 = 400;

        CSynthesis::genSine(pfInput, f0, fs, static_cast<int>(fs));

        CHECK(Error_t::kNoError == CSpectrogramIf::create(pCSpecGram, pfInput, static_cast<int>(fs), fs, iBlockLength, iHopLength));

        CHECK(Error_t::kNoError == pCSpecGram->getSpectrogramDimensions(aiSpecGramDimension[0], aiSpecGramDimension[1]));

        CHECK(iBlockLength / 2 + 1 == aiSpecGramDimension[0]);
        CHECK(static_cast<int>(fs) / iHopLength + 1 == aiSpecGramDimension[1]);

        pfVector = new float[std::max(aiSpecGramDimension[0], aiSpecGramDimension[1])];

        CHECK(Error_t::kNoError == pCSpecGram->getSpectrogramAxisVectors(pfVector, CSpectrogramIf::kFrequencyInHz));
        CHECK(pfVector[0] == 0);
        CHECK(pfVector[1] == Approx( fs / iBlockLength).margin(1e-6F).epsilon(1e-6F));
        CHECK(pfVector[iBlockLength / 2] == Approx(fs / 2).margin(1e-6F).epsilon(1e-6F));

        CHECK(Error_t::kNoError == pCSpecGram->getSpectrogramAxisVectors(pfVector, CSpectrogramIf::kTimeInS));
        CHECK(pfVector[0] == Approx( iBlockLength / (2 * fs)).margin(1e-6F).epsilon(1e-6F));
        CHECK(pfVector[1] == Approx( pfVector[0] + iHopLength / fs).margin(1e-6F).epsilon(1e-6F));

        CHECK(Error_t::kNoError == CSpectrogramIf::destroy(pCSpecGram));

        delete[] pfVector;
    }

    SECTION("Values")
    {
        f0 = 80;
        fs = 8192;
        iBlockLength = 1024;
        iHopLength = 512;

        CSynthesis::genSine(pfInput, f0, fs, static_cast<int>(fs));

        CHECK(Error_t::kNoError == CSpectrogramIf::create(pCSpecGram, pfInput, static_cast<int>(fs), fs, iBlockLength, iHopLength));

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
        fs = 8192;
        iBlockLength = 1024;
        iHopLength = 512;

        CSynthesis::genSine(pfInput, f0, fs, static_cast<int>(fs));

        CHECK(Error_t::kNoError == CSpectrogramIf::create(pCSpecGram, pfInput, static_cast<int>(fs), fs, iBlockLength, iHopLength));

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
        fs = 16;
        float fAmp = .5;
        iBlockLength = 16;
        iHopLength = 16;

        //reuse buffer for window setting
        CVectorFloat::setValue(pfTimeStamps, 1.F, iBlockLength);

        CSynthesis::genSine(pfInput, f0, fs, static_cast<int>(fs), fAmp);

        CHECK(Error_t::kNoError == CSpectrogramIf::create(pCSpecGram, pfInput, static_cast<int>(fs), fs, iBlockLength, iHopLength, false, pfTimeStamps));

        CHECK(Error_t::kNoError == pCSpecGram->getSpectrogramDimensions(aiSpecGramDimension[0], aiSpecGramDimension[1]));

        ppfSpecGram = new float* [aiSpecGramDimension[0]];
        for (auto k = 0; k < aiSpecGramDimension[0]; k++)
            ppfSpecGram[k] = new float[aiSpecGramDimension[1]];

        pCSpecGram->compSpectrogram(ppfSpecGram);

        CHECK(ppfSpecGram[4][0] == Approx(fAmp).margin(1e-6F).epsilon(1e-6F));
        CHECK(ppfSpecGram[0][0] == Approx(0).margin(1e-4F).epsilon(1e-4F));

        CHECK(Error_t::kNoError == CSpectrogramIf::destroy(pCSpecGram));
        CHECK(Error_t::kNoError == CSpectrogramIf::create(pCSpecGram, pfInput, static_cast<int>(fs), fs, iBlockLength, iHopLength, true, pfTimeStamps));

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

        CSynthesis::genSine(pfInput, f0, fs, static_cast<int>(fs));

        CHECK(Error_t::kNoError == CSpectrogramIf::create(pCSpecGram, pfInput, static_cast<long long>(fs), fs, iBlockLength, iHopLength));

        CHECK(Error_t::kNoError == pCSpecGram->getMelSpectrogramDimensions(aiSpecGramDimension[0], aiSpecGramDimension[1], &stMelSpecConfig));

        CHECK(stMelSpecConfig.iNumMelBins == aiSpecGramDimension[0]);
        CHECK(static_cast<int>(fs) / iHopLength + 1 == aiSpecGramDimension[1]);

        pfVector = new float[std::max(aiSpecGramDimension[0], aiSpecGramDimension[1])];

        CHECK(Error_t::kNoError == pCSpecGram->getMelSpectrogramAxisVectors(pfVector, CSpectrogramIf::kFrequencyInHz, &stMelSpecConfig));

        CHECK(Error_t::kNoError == pCSpecGram->getMelSpectrogramAxisVectors(pfVector, CSpectrogramIf::kTimeInS, &stMelSpecConfig));
        CHECK(pfVector[0] == Approx( iBlockLength / (2 * fs)).margin(1e-6F).epsilon(1e-6F));
        CHECK(pfVector[1] == Approx( pfVector[0] + iHopLength / fs).margin(1e-6F).epsilon(1e-6F));

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

    delete[] pfInput;
    pfInput = 0;

    CSpectrogramIf::destroy(pCSpecGram);

    delete[] pfTimeStamps;

}

#endif //WITH_TESTS
