#include "ACAConfig.h"

#ifdef WITH_TESTS

#include "Vector.h"

#include "BeatHisto.h"

#include "catch.hpp"

TEST_CASE("BeatHisto", "[BeatHistoClass]")
{

    CBeatHistoIf* m_pCInstance = 0;
    float* m_pfInput = 0;
    float* m_pfBeatHisto = 0;
    float* m_pfBeatTicks = 0;
    float m_fSampleRate = 44100;
    int m_iBlockLength = 1023,
        m_iHopLength = 512,
        m_iBufferLength = static_cast<int>(m_fSampleRate * 10);
    int iDim = 0;

    m_pfBeatHisto = new float[65536];
    m_pfBeatTicks = new float[65536];
    m_pfInput = new float[m_iBufferLength];
    CVectorFloat::setZero(m_pfInput, m_iBufferLength);


    SECTION("Api")
    {
        m_iBufferLength = 5000;
        CHECK(Error_t::kFunctionInvalidArgsError == CBeatHistoIf::create(m_pCInstance, 0, m_iBufferLength, m_fSampleRate, m_iBlockLength, m_iHopLength));
        CHECK(Error_t::kFunctionInvalidArgsError == CBeatHistoIf::create(m_pCInstance, m_pfInput, 0, m_fSampleRate, m_iBlockLength, m_iHopLength));
        CHECK(Error_t::kFunctionInvalidArgsError == CBeatHistoIf::create(m_pCInstance, m_pfInput, m_iBufferLength, 0, m_iBlockLength, m_iHopLength));
        CHECK(Error_t::kFunctionInvalidArgsError == CBeatHistoIf::create(m_pCInstance, m_pfInput, m_iBufferLength, m_fSampleRate, 0, m_iHopLength));
        CHECK(Error_t::kFunctionInvalidArgsError == CBeatHistoIf::create(m_pCInstance, m_pfInput, m_iBufferLength, m_fSampleRate, m_iBlockLength, 0));

        CHECK(Error_t::kNoError == CBeatHistoIf::create(m_pCInstance, m_pfInput, m_iBufferLength, m_fSampleRate, m_iBlockLength, m_iHopLength));

        CHECK_FALSE(m_pCInstance == 0);

        CHECK(Error_t::kNoError == m_pCInstance->getNumBins(iDim, CBeatHistoIf::kBeatHistoFft));
        CHECK(iDim == 4314);

        CHECK(Error_t::kFunctionInvalidArgsError == m_pCInstance->getBeatHistoAxisTicks(0, CBeatHistoIf::kBeatHistoFft));
        CHECK(Error_t::kNoError == m_pCInstance->getBeatHistoAxisTicks(m_pfBeatTicks, CBeatHistoIf::kBeatHistoFft));

        CHECK(Error_t::kFunctionInvalidArgsError == m_pCInstance->getBeatHisto(0, CBeatHistoIf::kBeatHistoFft));
        CHECK(Error_t::kNoError == m_pCInstance->getBeatHisto(m_pfBeatHisto, CBeatHistoIf::kBeatHistoFft));

        CHECK(Error_t::kNoError == CBeatHistoIf::destroy(m_pCInstance));
    }

    SECTION("Fft")
    {
        // bursts of 512 at 120BPM
        for (auto s = 0; s < 20; s++)
        {
            int iStart = static_cast<int>(s * m_fSampleRate / 2);
            CVectorFloat::setValue(&m_pfInput[iStart], 1.F, 512);
        }
        CHECK(Error_t::kNoError == CBeatHistoIf::create(m_pCInstance, m_pfInput, m_iBufferLength, m_fSampleRate));
        CHECK_FALSE(m_pCInstance == 0);

        CHECK(Error_t::kNoError == m_pCInstance->getBeatHisto(m_pfBeatHisto));
        CHECK(Error_t::kNoError == m_pCInstance->getBeatHistoAxisTicks(m_pfBeatTicks, CBeatHistoIf::kBeatHistoFft));

        float fMax = 0;
        long long iMax = -1;
        CVectorFloat::findMax(m_pfBeatHisto, fMax, iMax, m_pCInstance->getNumBins(CBeatHistoIf::kBeatHistoFft));
        CHECK(1.F == Approx(fMax).margin(1e-6F).epsilon(1e-6F));
        CHECK(37 == iMax);
        CHECK(std::abs(m_pfBeatTicks[iMax] - 120.F) <= 1.5F);


        CHECK(Error_t::kNoError == CBeatHistoIf::destroy(m_pCInstance));
    }

    SECTION("Corr")
    {
        // bursts of 512 at 120BPM
        for (auto s = 0; s < 20; s++)
        {
            int iStart = static_cast<int>(s * m_fSampleRate / 2);
            CVectorFloat::setValue(&m_pfInput[iStart], 1.F, 512);
        }
        CHECK(Error_t::kNoError == CBeatHistoIf::create(m_pCInstance, m_pfInput, m_iBufferLength, m_fSampleRate));
        CHECK_FALSE(m_pCInstance == 0);

        CHECK(Error_t::kNoError == m_pCInstance->getBeatHisto(m_pfBeatHisto, CBeatHistoIf::kBeatHistoCorr));
        CHECK(Error_t::kNoError == m_pCInstance->getBeatHistoAxisTicks(m_pfBeatTicks, CBeatHistoIf::kBeatHistoCorr));

        float fMax = 0;
        long long iMax = -1;
        CVectorFloat::findMax(m_pfBeatHisto, fMax, iMax, m_pCInstance->getNumBins(CBeatHistoIf::kBeatHistoCorr));
        CHECK(1.F == Approx(fMax).margin(1e-6F).epsilon(1e-6F));
        CHECK(8271 == iMax);
        CHECK(std::abs(m_pfBeatTicks[iMax] - 120.F) <= 1.F);


        CHECK(Error_t::kNoError == CBeatHistoIf::destroy(m_pCInstance));
    }

    CHECK(Error_t::kNoError == CBeatHistoIf::destroy(m_pCInstance));

    delete[] m_pfInput;
    delete[] m_pfBeatHisto;
    delete[] m_pfBeatTicks;
}

#endif //WITH_TESTS
