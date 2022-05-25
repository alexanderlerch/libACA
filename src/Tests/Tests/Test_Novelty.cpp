#include "ACAConfig.h"

#ifdef WITH_TESTS

#include "Vector.h"
#include "Synthesis.h"
#include "ToolConversion.h"
#include "ToolLowPass.h"

#include "NoveltyFromBlock.h"
#include "catch.hpp"

TEST_CASE("Novelty (static functions)", "[NoveltyStatic]")
{

    float* m_pfInput = 0;
    float m_fSampleRate = 1;
    int m_iBufferLength = 1025;

    m_pfInput = new float[m_iBufferLength];
    CVectorFloat::setZero(m_pfInput, m_iBufferLength);

    SECTION("NoveltyFlux")
    {
        int iIdx = m_iBufferLength / 2;

        // zero test
        CHECK(0.F == CNoveltyFromBlockIf::compNoveltyFlux(m_pfInput, &m_pfInput[iIdx], m_iBufferLength / 2));

        // 'sine' test
        m_pfInput[10] = 1;
        m_pfInput[iIdx + 10] = 1;
        m_fSampleRate = 2048;
        CHECK(0.F == Approx(CNoveltyFromBlockIf::compNoveltyFlux(m_pfInput, &m_pfInput[iIdx], m_iBufferLength / 2, m_fSampleRate)).margin(1e-6F).epsilon(1e-6F));
        m_fSampleRate = 32;
        CHECK(0.F == Approx(CNoveltyFromBlockIf::compNoveltyFlux(m_pfInput, &m_pfInput[iIdx], m_iBufferLength / 2, m_fSampleRate)).margin(1e-6F).epsilon(1e-6F));

        // 'noise' test
        CVectorFloat::setValue(m_pfInput, 1, m_iBufferLength);
        CHECK(0.F == Approx(CNoveltyFromBlockIf::compNoveltyFlux(m_pfInput, &m_pfInput[iIdx], m_iBufferLength / 2)).margin(1e-6F).epsilon(1e-6F));

        // one spectrum zero, the other one
        CVectorFloat::setZero(m_pfInput, m_iBufferLength / 2);
        CHECK(0.F == Approx(CNoveltyFromBlockIf::compNoveltyFlux(m_pfInput, &m_pfInput[iIdx], m_iBufferLength / 2)).margin(1e-4F).epsilon(1e-4F));
        CHECK(1.F / std::sqrt(m_iBufferLength / 2.F) == Approx(CNoveltyFromBlockIf::compNoveltyFlux(&m_pfInput[iIdx], m_pfInput, m_iBufferLength / 2)).margin(1e-4F).epsilon(1e-4F));

        // one spectrum zero, the other two
        CVectorFloat::mulC_I(m_pfInput, 2.F, m_iBufferLength);
        CHECK(0.F == Approx(CNoveltyFromBlockIf::compNoveltyFlux(m_pfInput, &m_pfInput[iIdx], m_iBufferLength / 2)).margin(1e-4F).epsilon(1e-4F));
        CHECK(std::sqrt(4.F * (m_iBufferLength / 2)) / (m_iBufferLength / 2) == Approx(CNoveltyFromBlockIf::compNoveltyFlux(&m_pfInput[iIdx], m_pfInput, m_iBufferLength / 2)).margin(1e-4F).epsilon(1e-4F));

        //decreasing spectrum
        CVectorFloat::setZero(m_pfInput, m_iBufferLength / 2);
        CVectorFloat::setValue(&m_pfInput[m_iBufferLength / 2], 1.F, m_iBufferLength / 2);
        for (auto n = 0; n < 5; n++)
        {
            CHECK(0.F == Approx(CNoveltyFromBlockIf::compNoveltyFlux(m_pfInput, &m_pfInput[m_iBufferLength / 2], m_iBufferLength / 2)).margin(1e-4F).epsilon(1e-4F));
            CVectorFloat::mulC_I(&m_pfInput[m_iBufferLength / 2], .75F, m_iBufferLength / 2);
        }

        // alternating spectral bins
        CVectorFloat::setZero(m_pfInput, m_iBufferLength);
        iIdx = 4;
        m_iBufferLength = 4;
        m_pfInput[0] = 1;
        m_pfInput[2] = 1;
        m_pfInput[iIdx + 1] = 1;
        m_pfInput[iIdx + 3] = 1;
        CHECK(std::sqrt(2.F) / 4.F == Approx(CNoveltyFromBlockIf::compNoveltyFlux(m_pfInput, &m_pfInput[iIdx], m_iBufferLength)).margin(1e-4F).epsilon(1e-4F));
    }

    SECTION("NoveltyHainsworth")
    {
        int iIdx = m_iBufferLength / 2;

        float fEpsilon = 1e-5F;

        // zero test
        CHECK(0.F == CNoveltyFromBlockIf::compNoveltyHainsworth(m_pfInput, &m_pfInput[iIdx], m_iBufferLength / 2));

        // 'sine' test
        m_pfInput[10] = 1;
        m_pfInput[iIdx + 10] = 1;
        m_fSampleRate = 2048;
        CHECK(0.F == Approx(CNoveltyFromBlockIf::compNoveltyHainsworth(m_pfInput, &m_pfInput[iIdx], m_iBufferLength / 2, m_fSampleRate)).margin(1e-6F).epsilon(1e-6F));
        m_fSampleRate = 32;
        CHECK(0.F == Approx(CNoveltyFromBlockIf::compNoveltyHainsworth(m_pfInput, &m_pfInput[iIdx], m_iBufferLength / 2, m_fSampleRate)).margin(1e-6F).epsilon(1e-6F));

        // 'noise' test
        CVectorFloat::setValue(m_pfInput, 1, m_iBufferLength);
        CHECK(0.F == Approx(CNoveltyFromBlockIf::compNoveltyHainsworth(m_pfInput, &m_pfInput[iIdx], m_iBufferLength / 2)).margin(1e-6F).epsilon(1e-6F));

        // one spectrum zero, the other one
        CVectorFloat::setZero(m_pfInput, m_iBufferLength / 2);
        CHECK(std::log2(fEpsilon) == Approx(CNoveltyFromBlockIf::compNoveltyHainsworth(m_pfInput, &m_pfInput[iIdx], m_iBufferLength / 2)).margin(1e-4F).epsilon(1e-4F));
        CHECK(std::log2(1.F/ fEpsilon) == Approx(CNoveltyFromBlockIf::compNoveltyHainsworth(&m_pfInput[iIdx], m_pfInput, m_iBufferLength / 2)).margin(1e-4F).epsilon(1e-4F));

        // one spectrum zero, the other two
        CVectorFloat::mulC_I(m_pfInput, 2.F, m_iBufferLength);
        CHECK(std::log2(.5* fEpsilon) == Approx(CNoveltyFromBlockIf::compNoveltyHainsworth(m_pfInput, &m_pfInput[iIdx], m_iBufferLength / 2)).margin(1e-4F).epsilon(1e-4F));
        CHECK(std::log2(2.F/ fEpsilon) == Approx(CNoveltyFromBlockIf::compNoveltyHainsworth(&m_pfInput[iIdx], m_pfInput, m_iBufferLength / 2)).margin(1e-4F).epsilon(1e-4F));

        //decreasing spectrum
        CVectorFloat::setZero(m_pfInput, m_iBufferLength / 2);
        CVectorFloat::setValue(&m_pfInput[m_iBufferLength / 2], 1.F, m_iBufferLength / 2);
        for (auto n = 0; n < 5; n++)
        {
            CHECK(std::log2(fEpsilon) <= Approx(CNoveltyFromBlockIf::compNoveltyHainsworth(m_pfInput, &m_pfInput[m_iBufferLength / 2], m_iBufferLength / 2)).margin(1e-4F).epsilon(1e-4F));
            CVectorFloat::mulC_I(&m_pfInput[m_iBufferLength / 2], .75F, m_iBufferLength / 2);
        }

        // alternating spectral bins
        CVectorFloat::setZero(m_pfInput, m_iBufferLength);
        iIdx = 4;
        m_iBufferLength = 4;
        m_pfInput[0] = 1;
        m_pfInput[2] = 1;
        m_pfInput[iIdx + 1] = 1;
        m_pfInput[iIdx + 3] = 1;
        CHECK(0.F == Approx(CNoveltyFromBlockIf::compNoveltyHainsworth(m_pfInput, &m_pfInput[iIdx], m_iBufferLength)).margin(1e-4F).epsilon(1e-4F));
    }

    SECTION("NoveltyLaroche")
    {
        int iIdx = m_iBufferLength / 2;

        // zero test
        CHECK(0.F == CNoveltyFromBlockIf::compNoveltyLaroche(m_pfInput, &m_pfInput[iIdx], m_iBufferLength / 2));

        // 'sine' test
        m_pfInput[10] = 1;
        m_pfInput[iIdx + 10] = 1;
        m_fSampleRate = 2048;
        CHECK(0.F == Approx(CNoveltyFromBlockIf::compNoveltyLaroche(m_pfInput, &m_pfInput[iIdx], m_iBufferLength / 2, m_fSampleRate)).margin(1e-6F).epsilon(1e-6F));
        m_fSampleRate = 32;
        CHECK(0.F == Approx(CNoveltyFromBlockIf::compNoveltyLaroche(m_pfInput, &m_pfInput[iIdx], m_iBufferLength / 2, m_fSampleRate)).margin(1e-6F).epsilon(1e-6F));

        // 'noise' test
        CVectorFloat::setValue(m_pfInput, 1, m_iBufferLength);
        CHECK(0.F == Approx(CNoveltyFromBlockIf::compNoveltyLaroche(m_pfInput, &m_pfInput[iIdx], m_iBufferLength / 2)).margin(1e-6F).epsilon(1e-6F));

        // one spectrum zero, the other one
        CVectorFloat::setZero(m_pfInput, m_iBufferLength / 2);
        CHECK(0.F == Approx(CNoveltyFromBlockIf::compNoveltyLaroche(m_pfInput, &m_pfInput[iIdx], m_iBufferLength / 2)).margin(1e-4F).epsilon(1e-4F));
        CHECK(1.F == Approx(CNoveltyFromBlockIf::compNoveltyLaroche(&m_pfInput[iIdx], m_pfInput, m_iBufferLength / 2)).margin(1e-4F).epsilon(1e-4F));

        // one spectrum zero, the other two
        CVectorFloat::mulC_I(m_pfInput, 2.F, m_iBufferLength);
        CHECK(0.F == Approx(CNoveltyFromBlockIf::compNoveltyLaroche(m_pfInput, &m_pfInput[iIdx], m_iBufferLength / 2)).margin(1e-4F).epsilon(1e-4F));
        CHECK(std::sqrt(2.F) == Approx(CNoveltyFromBlockIf::compNoveltyLaroche(&m_pfInput[iIdx], m_pfInput, m_iBufferLength / 2)).margin(1e-4F).epsilon(1e-4F));

        //decreasing spectrum
        CVectorFloat::setZero(m_pfInput, m_iBufferLength / 2);
        CVectorFloat::setValue(&m_pfInput[m_iBufferLength / 2], 1.F, m_iBufferLength / 2);
        for (auto n = 0; n < 5; n++)
        {
            CHECK(0.F == Approx(CNoveltyFromBlockIf::compNoveltyLaroche(m_pfInput, &m_pfInput[m_iBufferLength / 2], m_iBufferLength / 2)).margin(1e-4F).epsilon(1e-4F));
            CVectorFloat::mulC_I(&m_pfInput[m_iBufferLength / 2], .75F, m_iBufferLength / 2);
        }

        // alternating spectral bins
        CVectorFloat::setZero(m_pfInput, m_iBufferLength);
        iIdx = 4;
        m_iBufferLength = 4;
        m_pfInput[0] = 1;
        m_pfInput[2] = 1;
        m_pfInput[iIdx + 1] = 1;
        m_pfInput[iIdx + 3] = 1;
        CHECK(2.F / 4.F == Approx(CNoveltyFromBlockIf::compNoveltyLaroche(m_pfInput, &m_pfInput[iIdx], m_iBufferLength)).margin(1e-4F).epsilon(1e-4F));
    }



    delete[] m_pfInput;
}

TEST_CASE("Novelty (class interface per block)", "[NoveltyBlockClass]")
{
    CNoveltyFromBlockIf* m_pCInstance = 0;
    float* m_pfInput = 0;
    float m_fSampleRate = 1;
    int m_iBufferLength = 1025;
    
    m_pfInput = new float[m_iBufferLength];
    CVectorFloat::setValue(m_pfInput, 1.F, m_iBufferLength);

    SECTION("Api")
    {
        CHECK(Error_t::kFunctionInvalidArgsError == CNoveltyFromBlockIf::create(m_pCInstance, CNoveltyIf::kNoveltyFlux, 0, m_fSampleRate));
        CHECK(Error_t::kFunctionInvalidArgsError == CNoveltyFromBlockIf::create(m_pCInstance, CNoveltyIf::kNoveltyFlux, -1, m_fSampleRate));
        CHECK(Error_t::kFunctionInvalidArgsError == CNoveltyFromBlockIf::create(m_pCInstance, CNoveltyIf::kNoveltyFlux, m_iBufferLength, 0));
        CHECK(Error_t::kFunctionInvalidArgsError == CNoveltyFromBlockIf::create(m_pCInstance, CNoveltyIf::kNoveltyFlux, m_iBufferLength, -1));

        CHECK(Error_t::kNoError == CNoveltyFromBlockIf::create(m_pCInstance, CNoveltyIf::kNoveltyFlux, m_iBufferLength, m_fSampleRate));
        CHECK(1 == m_pCInstance->getNoveltyDimension());
        CHECK(Error_t::kNoError == CNoveltyFromBlockIf::destroy(m_pCInstance));
    }

    SECTION("NoveltyCalc")
    {
        m_fSampleRate = 16000.F;
        for (auto k = 0; k < CNoveltyIf::kNumNoveltyFunctions; k++)
        {
            float fResult = 0;
            CHECK(Error_t::kNoError == CNoveltyFromBlockIf::create(m_pCInstance, static_cast<CNoveltyIf::Novelty_t>(k), m_iBufferLength, m_fSampleRate));
            CHECK(Error_t::kNoError == m_pCInstance->calcNoveltyFromBlock(&fResult, m_pfInput));
            CHECK(Error_t::kNoError == CNoveltyFromBlockIf::destroy(m_pCInstance));
        }
    }

    CNoveltyFromBlockIf::destroy(m_pCInstance);

    delete[] m_pfInput;
}

TEST_CASE("Novelty (per array)", "[NoveltyClass]")
{

    CNoveltyIf* m_pCInstance = 0;
    float* m_pfInput = 0;
    float* m_pfNovelty = 0;
    bool* m_pbOnset = 0;
    float m_fSampleRate = 44100;
    int m_iBlockLength = 1023,
        m_iHopLength = 512,
        m_iBufferLength = 96000;
    int iDim = 0;

    m_pbOnset = new bool[188];
    m_pfNovelty = new float[188];
    m_pfInput = new float[m_iBufferLength];
    CVectorFloat::setZero(m_pfInput, m_iBufferLength);


    SECTION("Api")
    {
        for (auto f = 0; f < CNoveltyIf::kNumNoveltyFunctions; f++)
        {
            CHECK(Error_t::kFunctionInvalidArgsError == CNoveltyIf::create(m_pCInstance, static_cast<CNoveltyIf::Novelty_t>(f), 0, m_iBufferLength, m_fSampleRate, m_iBlockLength, m_iHopLength));
            CHECK(Error_t::kFunctionInvalidArgsError == CNoveltyIf::create(m_pCInstance, static_cast<CNoveltyIf::Novelty_t>(f), m_pfInput, 0, m_fSampleRate, m_iBlockLength, m_iHopLength));
            CHECK(Error_t::kFunctionInvalidArgsError == CNoveltyIf::create(m_pCInstance, static_cast<CNoveltyIf::Novelty_t>(f), m_pfInput, -1, m_fSampleRate, m_iBlockLength, m_iHopLength));
            CHECK(Error_t::kFunctionInvalidArgsError == CNoveltyIf::create(m_pCInstance, static_cast<CNoveltyIf::Novelty_t>(f), m_pfInput, m_iBufferLength, 0, m_iBlockLength, m_iHopLength));
            CHECK(Error_t::kFunctionInvalidArgsError == CNoveltyIf::create(m_pCInstance, static_cast<CNoveltyIf::Novelty_t>(f), m_pfInput, m_iBufferLength, m_fSampleRate, 0, m_iHopLength));
            CHECK(Error_t::kFunctionInvalidArgsError == CNoveltyIf::create(m_pCInstance, static_cast<CNoveltyIf::Novelty_t>(f), m_pfInput, m_iBufferLength, m_fSampleRate, m_iBlockLength, 0));

            CHECK(Error_t::kNoError == CNoveltyIf::create(m_pCInstance, static_cast<CNoveltyIf::Novelty_t>(f), m_pfInput, m_iBufferLength, m_fSampleRate, m_iBlockLength, m_iHopLength));

            CHECK_FALSE(m_pCInstance == 0);

            CHECK(Error_t::kNoError == m_pCInstance->getNumBlocks(iDim));
            CHECK(iDim == 188);

            CHECK(Error_t::kFunctionInvalidArgsError == m_pCInstance->getNovelty(0));

            CHECK(Error_t::kNoError == CNoveltyIf::destroy(m_pCInstance));

        }
    }

    SECTION("Output")
    {
        CVectorFloat::setValue(&m_pfInput[10 * 512], 1.F, 512);
        CHECK(Error_t::kNoError == CNoveltyIf::create(m_pCInstance, CNoveltyIf::kNoveltyFlux, m_pfInput, m_iBufferLength, m_fSampleRate));
        CHECK_FALSE(m_pCInstance == 0);

        CHECK(Error_t::kNoError == m_pCInstance->getNumBlocks(iDim));

        CHECK(Error_t::kNoError == m_pCInstance->getNovelty(m_pfNovelty, m_pbOnset));

        for (auto n = 0; n < iDim; n++)
        {
            if (n != 5)
                CHECK_FALSE(m_pbOnset[n]);
            else
                CHECK(m_pbOnset[n]);

        }
        CHECK(Error_t::kNoError == CNoveltyIf::destroy(m_pCInstance));
    }

    CNoveltyIf::destroy(m_pCInstance);

    delete[] m_pfInput;
    delete[] m_pfNovelty;
    delete[] m_pbOnset;

}

#endif //WITH_TESTS
