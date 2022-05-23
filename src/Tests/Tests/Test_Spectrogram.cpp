#include "ACAConfig.h"

#ifdef WITH_TESTS

#include "Synthesis.h"
#include "Vector.h"
#include "Spectrogram.h"

#include "catch.hpp"

TEST_CASE("Spectrogram", "[Spectrogram]")
{

    CSpectrogramIf* m_pCSpecGram = 0;

    float** m_ppfSpecGram = 0;
    float* m_pfInput = 0;
    float* m_pfTimeStamps = 0;

    int  m_iBlockLength = 1024,
        m_iHopLength = 512;

    float m_fs = 40000,
        m_f0 = 400;

    int     m_aiSpecGramDimension[2] = { 0, 0 };

    m_pfInput = new float[static_cast<int>(m_fs)];
    m_pfTimeStamps = new float[static_cast<int>(m_fs)];

    SECTION("Api")
    {
        CHECK_FALSE(Error_t::kNoError == CSpectrogramIf::create(m_pCSpecGram, static_cast<float*>(0), 1, m_fs, m_iBlockLength, m_iHopLength));
        CHECK_FALSE(Error_t::kNoError == CSpectrogramIf::create(m_pCSpecGram, m_pfInput, 0, m_fs, m_iBlockLength, m_iHopLength));
        CHECK_FALSE(Error_t::kNoError == CSpectrogramIf::create(m_pCSpecGram, m_pfInput, 1, 0, m_iBlockLength, m_iHopLength));
        CHECK_FALSE(Error_t::kNoError == CSpectrogramIf::create(m_pCSpecGram, m_pfInput, 1, m_fs, 0, m_iHopLength));
        CHECK_FALSE(Error_t::kNoError == CSpectrogramIf::create(m_pCSpecGram, m_pfInput, 1, m_fs, m_iBlockLength, 0));
        CHECK_FALSE(Error_t::kNoError == CSpectrogramIf::create(m_pCSpecGram, m_pfInput, 1, -1, m_iBlockLength, m_iHopLength));
        CHECK_FALSE(Error_t::kNoError == CSpectrogramIf::create(m_pCSpecGram, m_pfInput, 1, m_fs, -1, m_iHopLength));
        CHECK_FALSE(Error_t::kNoError == CSpectrogramIf::create(m_pCSpecGram, m_pfInput, 1, m_fs, m_iBlockLength, -1));
        CHECK_FALSE(Error_t::kNoError == CSpectrogramIf::create(m_pCSpecGram, m_pfInput, 1, m_fs, 3, m_iHopLength));

        CHECK(Error_t::kNoError ==CSpectrogramIf::create(m_pCSpecGram, m_pfInput, 5, m_fs, m_iBlockLength, m_iHopLength));
        CHECK_FALSE(Error_t::kNoError == m_pCSpecGram->getSpectrogramAxisVectors(0, CSpectrogramIf::kFrequencyInHz));
        CHECK_FALSE(Error_t::kNoError == m_pCSpecGram->getMelSpectrogramAxisVectors(m_pfInput, CSpectrogramIf::kFrequencyInHz, 0));
        CHECK(Error_t::kNoError ==CSpectrogramIf::destroy(m_pCSpecGram));
    }


    SECTION("Dimensions")
    {
        float* pfVector = 0;

        m_f0 = 400;

        CSynthesis::generateSine(m_pfInput, m_f0, m_fs, static_cast<int>(m_fs));

        CHECK(Error_t::kNoError == CSpectrogramIf::create(m_pCSpecGram, m_pfInput, static_cast<int>(m_fs), m_fs, m_iBlockLength, m_iHopLength));

        CHECK(Error_t::kNoError == m_pCSpecGram->getSpectrogramDimensions(m_aiSpecGramDimension[0], m_aiSpecGramDimension[1]));

        CHECK(m_iBlockLength / 2 + 1 == m_aiSpecGramDimension[0]);
        CHECK(static_cast<int>(m_fs) / m_iHopLength + 1 == m_aiSpecGramDimension[1]);

        pfVector = new float[std::max(m_aiSpecGramDimension[0], m_aiSpecGramDimension[1])];

        CHECK(Error_t::kNoError == m_pCSpecGram->getSpectrogramAxisVectors(pfVector, CSpectrogramIf::kFrequencyInHz));
        CHECK(pfVector[0] == 0);
        CHECK(pfVector[1] == Approx( m_fs / m_iBlockLength).margin(1e-6F).epsilon(1e-6F));
        CHECK(pfVector[m_iBlockLength / 2] == Approx(m_fs / 2).margin(1e-6F).epsilon(1e-6F));

        CHECK(Error_t::kNoError == m_pCSpecGram->getSpectrogramAxisVectors(pfVector, CSpectrogramIf::kTimeInS));
        CHECK(pfVector[0] == Approx( m_iBlockLength / (2 * m_fs)).margin(1e-6F).epsilon(1e-6F));
        CHECK(pfVector[1] == Approx( pfVector[0] + m_iHopLength / m_fs).margin(1e-6F).epsilon(1e-6F));

        CHECK(Error_t::kNoError == CSpectrogramIf::destroy(m_pCSpecGram));

        delete[] pfVector;
    }

    SECTION("Values")
    {
        m_f0 = 80;
        m_fs = 8192;
        m_iBlockLength = 1024;
        m_iHopLength = 512;

        CSynthesis::generateSine(m_pfInput, m_f0, m_fs, static_cast<int>(m_fs));

        CHECK(Error_t::kNoError == CSpectrogramIf::create(m_pCSpecGram, m_pfInput, static_cast<int>(m_fs), m_fs, m_iBlockLength, m_iHopLength));

        CHECK(Error_t::kNoError == m_pCSpecGram->getSpectrogramDimensions(m_aiSpecGramDimension[0], m_aiSpecGramDimension[1]));

        m_ppfSpecGram = new float* [m_aiSpecGramDimension[0]];
        for (auto k = 0; k < m_aiSpecGramDimension[0]; k++)
            m_ppfSpecGram[k] = new float[m_aiSpecGramDimension[1]];

        m_pCSpecGram->getSpectrogram(m_ppfSpecGram);

        CHECK(m_ppfSpecGram[10][10] - m_ppfSpecGram[20][10] == Approx(m_ppfSpecGram[10][10]).margin(1e-4F).epsilon(1e-4F));
        CHECK(m_ppfSpecGram[115][10] == Approx(0).margin(1e-6F).epsilon(1e-6F));
        CHECK(m_ppfSpecGram[116][10] == Approx(0).margin(1e-6F).epsilon(1e-6F));

        CHECK(Error_t::kNoError == CSpectrogramIf::destroy(m_pCSpecGram));

        for (auto k = 0; k < m_aiSpecGramDimension[0]; k++)
            delete[] m_ppfSpecGram[k];
        delete[] m_ppfSpecGram;

        m_f0 = 84;
        m_fs = 8192;
        m_iBlockLength = 1024;
        m_iHopLength = 512;

        CSynthesis::generateSine(m_pfInput, m_f0, m_fs, static_cast<int>(m_fs));

        CHECK(Error_t::kNoError == CSpectrogramIf::create(m_pCSpecGram, m_pfInput, static_cast<int>(m_fs), m_fs, m_iBlockLength, m_iHopLength));

        CHECK(Error_t::kNoError == m_pCSpecGram->getSpectrogramDimensions(m_aiSpecGramDimension[0], m_aiSpecGramDimension[1]));

        m_ppfSpecGram = new float* [m_aiSpecGramDimension[0]];
        for (auto k = 0; k < m_aiSpecGramDimension[0]; k++)
            m_ppfSpecGram[k] = new float[m_aiSpecGramDimension[1]];

        m_pCSpecGram->getSpectrogram(m_ppfSpecGram);

        CHECK(m_ppfSpecGram[10][10] - m_ppfSpecGram[11][10] == Approx(0).margin(1e-4F).epsilon(1e-4F));

        CHECK(Error_t::kNoError == CSpectrogramIf::destroy(m_pCSpecGram));

        for (auto k = 0; k < m_aiSpecGramDimension[0]; k++)
            delete[] m_ppfSpecGram[k];
        delete[] m_ppfSpecGram;

        m_f0 = 4;
        m_fs = 16;
        float fAmp = .5;
        m_iBlockLength = 16;
        m_iHopLength = 16;

        //reuse buffer for window setting
        CVectorFloat::setValue(m_pfTimeStamps, 1.F, m_iBlockLength);

        CSynthesis::generateSine(m_pfInput, m_f0, m_fs, static_cast<int>(m_fs), fAmp);

        CHECK(Error_t::kNoError == CSpectrogramIf::create(m_pCSpecGram, m_pfInput, static_cast<int>(m_fs), m_fs, m_iBlockLength, m_iHopLength, false, m_pfTimeStamps));

        CHECK(Error_t::kNoError == m_pCSpecGram->getSpectrogramDimensions(m_aiSpecGramDimension[0], m_aiSpecGramDimension[1]));

        m_ppfSpecGram = new float* [m_aiSpecGramDimension[0]];
        for (auto k = 0; k < m_aiSpecGramDimension[0]; k++)
            m_ppfSpecGram[k] = new float[m_aiSpecGramDimension[1]];

        m_pCSpecGram->getSpectrogram(m_ppfSpecGram);

        CHECK(m_ppfSpecGram[4][0] == Approx(fAmp).margin(1e-6F).epsilon(1e-6F));
        CHECK(m_ppfSpecGram[0][0] == Approx(0).margin(1e-4F).epsilon(1e-4F));

        CHECK(Error_t::kNoError == CSpectrogramIf::destroy(m_pCSpecGram));
        CHECK(Error_t::kNoError == CSpectrogramIf::create(m_pCSpecGram, m_pfInput, static_cast<int>(m_fs), m_fs, m_iBlockLength, m_iHopLength, true, m_pfTimeStamps));

        m_pCSpecGram->getSpectrogram(m_ppfSpecGram);

        CHECK(m_ppfSpecGram[4][0] == Approx(1).margin(1e-6F).epsilon(1e-6F));
        CHECK(m_ppfSpecGram[0][0] == Approx(0).margin(1e-6F).epsilon(1e-6F));

        CHECK(Error_t::kNoError == CSpectrogramIf::destroy(m_pCSpecGram));

        for (auto k = 0; k < m_aiSpecGramDimension[0]; k++)
            delete[] m_ppfSpecGram[k];
        delete[] m_ppfSpecGram;
        m_ppfSpecGram = 0;
    }

    SECTION("MelSpectrogram")
    {
        CSpectrogramIf::MelSpectrogramConfig_t stMelSpecConfig;
        float* pfVector = 0;

        m_f0 = 400;

        stMelSpecConfig.bIsLogarithmic = false;
        stMelSpecConfig.fMaxFreqInHz = 20000;
        stMelSpecConfig.fMinFreqInHz = 0;
        stMelSpecConfig.iNumMelBins = 128;

        CSynthesis::generateSine(m_pfInput, m_f0, m_fs, static_cast<int>(m_fs));

        CHECK(Error_t::kNoError == CSpectrogramIf::create(m_pCSpecGram, m_pfInput, static_cast<long long>(m_fs), m_fs, m_iBlockLength, m_iHopLength));

        CHECK(Error_t::kNoError == m_pCSpecGram->getMelSpectrogramDimensions(m_aiSpecGramDimension[0], m_aiSpecGramDimension[1], &stMelSpecConfig));

        CHECK(stMelSpecConfig.iNumMelBins == m_aiSpecGramDimension[0]);
        CHECK(static_cast<int>(m_fs) / m_iHopLength + 1 == m_aiSpecGramDimension[1]);

        pfVector = new float[std::max(m_aiSpecGramDimension[0], m_aiSpecGramDimension[1])];

        CHECK(Error_t::kNoError == m_pCSpecGram->getMelSpectrogramAxisVectors(pfVector, CSpectrogramIf::kFrequencyInHz, &stMelSpecConfig));

        CHECK(Error_t::kNoError == m_pCSpecGram->getMelSpectrogramAxisVectors(pfVector, CSpectrogramIf::kTimeInS, &stMelSpecConfig));
        CHECK(pfVector[0] == Approx( m_iBlockLength / (2 * m_fs)).margin(1e-6F).epsilon(1e-6F));
        CHECK(pfVector[1] == Approx( pfVector[0] + m_iHopLength / m_fs).margin(1e-6F).epsilon(1e-6F));

        m_ppfSpecGram = new float* [m_aiSpecGramDimension[0]];
        for (auto k = 0; k < m_aiSpecGramDimension[0]; k++)
            m_ppfSpecGram[k] = new float[m_aiSpecGramDimension[1]];

        stMelSpecConfig.bIsLogarithmic = true;
        m_pCSpecGram->getMelSpectrogram(m_ppfSpecGram, &stMelSpecConfig);

        CHECK(-36.9F == Approx(m_ppfSpecGram[13][1]).margin(1e-1F).epsilon(1e-1F));

        delete[] pfVector;
    }
    if (m_ppfSpecGram)
    {
        for (int i = 0; i < m_aiSpecGramDimension[0]; i++)
        {
            delete[] m_ppfSpecGram[i];
        }
        delete[] m_ppfSpecGram;
    }

    delete[] m_pfInput;
    m_pfInput = 0;

    CSpectrogramIf::destroy(m_pCSpecGram);

    delete[] m_pfTimeStamps;

}

#endif //WITH_TESTS
