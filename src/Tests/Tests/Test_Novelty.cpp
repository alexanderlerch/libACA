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

//TEST_CASE("Noveltys (class interface per block)", "[NoveltysBlockClass]")
//{
//    CNoveltyFromBlockIf* m_pCInstance = 0;
//    float* m_pfInput = 0;
//    float m_fSampleRate = 1;
//    int m_iBufferLength = 1025;
//    
//    m_pfInput = new float[m_iBufferLength];
//    CVectorFloat::setValue(m_pfInput, 1.F, m_iBufferLength);
//
//    SECTION("Api")
//    {
//        CHECK(Error_t::kFunctionInvalidArgsError == CNoveltyFromBlockIf::create(m_pCInstance, CNoveltyIf::kNoveltyCentroid, 0, m_fSampleRate));
//        CHECK(Error_t::kFunctionInvalidArgsError == CNoveltyFromBlockIf::create(m_pCInstance, CNoveltyIf::kNoveltyCentroid, -1, m_fSampleRate));
//        CHECK(Error_t::kFunctionInvalidArgsError == CNoveltyFromBlockIf::create(m_pCInstance, CNoveltyIf::kNoveltyCentroid, m_iBufferLength, 0));
//        CHECK(Error_t::kFunctionInvalidArgsError == CNoveltyFromBlockIf::create(m_pCInstance, CNoveltyIf::kNoveltyCentroid, m_iBufferLength, -1));
//
//        CHECK(Error_t::kNoError == CNoveltyFromBlockIf::create(m_pCInstance, CNoveltyIf::kNoveltyCentroid, m_iBufferLength, m_fSampleRate));
//        CHECK(1 == m_pCInstance->getNoveltyDimensions());
//        CHECK(Error_t::kNoError == CNoveltyFromBlockIf::destroy(m_pCInstance));
//    }
//
//    SECTION("NoveltyCalc")
//    {
//        m_fSampleRate = 16000.F;
//        for (auto k = 0; k < CNoveltyIf::kNumNoveltys; k++)
//        {
//            float fResult = 0;
//            CHECK(Error_t::kNoError == CNoveltyFromBlockIf::create(m_pCInstance, static_cast<CNoveltyIf::Novelty_t>(k), m_iBufferLength, m_fSampleRate));
//            CHECK(m_pCInstance->getNoveltyDimensions() >= 1);
//            if (m_pCInstance->getNoveltyDimensions() == 1)
//                CHECK(Error_t::kNoError == m_pCInstance->calcNoveltyFromBlock(&fResult, m_pfInput));
//            CHECK(Error_t::kNoError == CNoveltyFromBlockIf::destroy(m_pCInstance));
//        }
//    }
//
//    SECTION("TimeMaxAcf")
//    {
//        float fResult = 0;
//        CHECK(Error_t::kNoError == CNoveltyFromBlockIf::create(m_pCInstance, CNoveltyIf::kNoveltyTimeMaxAcf, m_iBufferLength, m_fSampleRate));
//
//        // ones
//        CHECK(Error_t::kNoError == m_pCInstance->calcNoveltyFromBlock(&fResult, m_pfInput));
//        CHECK(fResult == Approx(1.F).margin(1e-6F).epsilon(1e-6F));
//
//        // zeros
//        CVectorFloat::setZero(m_pfInput, m_iBufferLength);
//        CHECK(Error_t::kNoError == m_pCInstance->calcNoveltyFromBlock(&fResult, m_pfInput));
//        CHECK(fResult == Approx(0.F).margin(1e-6F).epsilon(1e-6F));
//
//        // sine wave
//        int eta = 500;
//        m_fSampleRate = 1000;
//        CSynthesis::generateSine(m_pfInput, 2, m_fSampleRate, m_iBufferLength, 1.F);
//        CHECK(Error_t::kNoError == m_pCInstance->calcNoveltyFromBlock(&fResult, m_pfInput));
//        CHECK((1.F - eta / 1000.F) == Approx(fResult).margin(1e-3F).epsilon(1e-3F));
//    }
//
//    SECTION("SpectralPitchChroma")
//    {
//        float afResult[12] = { 0,0,0,0,0,0,0,0,0,0,0,0 };
//        m_fSampleRate = 32000;
//        CHECK(Error_t::kNoError == CNoveltyFromBlockIf::create(m_pCInstance, CNoveltyIf::kNoveltyPitchChroma, m_iBufferLength, m_fSampleRate));
//        CHECK(12 == m_pCInstance->getNoveltyDimensions());
//
//        // ones
//        CHECK(Error_t::kNoError == m_pCInstance->calcNoveltyFromBlock(afResult, m_pfInput));
//        CHECK(1.F == Approx(CVectorFloat::getSum(afResult, 12)).margin(1e-6F).epsilon(1e-6F));
//        CHECK(1.F / 12.F == Approx(CVectorFloat::getMax(afResult, 12)).margin(1e-6F).epsilon(1e-6F));
//        CHECK(1.F / 12.F == Approx(CVectorFloat::getMin(afResult, 12)).margin(1e-6F).epsilon(1e-6F));
//
//        // zeros
//        CVectorFloat::setZero(m_pfInput, m_iBufferLength);
//        CHECK(Error_t::kNoError == m_pCInstance->calcNoveltyFromBlock(afResult, m_pfInput));
//        CHECK(0.F == Approx(CVectorFloat::getSum(afResult, 12)).margin(1e-6F).epsilon(1e-6F));
//        CHECK(0.F == Approx(CVectorFloat::getMin(afResult, 12)).margin(1e-6F).epsilon(1e-6F));
//
//        // sine 
//        m_fSampleRate = 44000;
//        CHECK(Error_t::kNoError == CNoveltyFromBlockIf::destroy(m_pCInstance));
//        CHECK(Error_t::kNoError == CNoveltyFromBlockIf::create(m_pCInstance, CNoveltyIf::kNoveltyPitchChroma, 1000, m_fSampleRate));
//        m_pfInput[static_cast<int>(CConversion::convertFreq2Bin(440, 2000, m_fSampleRate))] = 1;
//        CHECK(Error_t::kNoError == m_pCInstance->calcNoveltyFromBlock(afResult, m_pfInput));
//        CHECK(1.F == Approx(afResult[9]).margin(1e-6F).epsilon(1e-6F));
//
//        // sine 
//        float fNorm = 0;
//        int k0 = static_cast<int>(CConversion::convertFreq2Bin(440, 2000, m_fSampleRate));
//        m_fSampleRate = 44000;
//        CHECK(Error_t::kNoError == CNoveltyFromBlockIf::destroy(m_pCInstance));
//        CHECK(Error_t::kNoError == CNoveltyFromBlockIf::create(m_pCInstance, CNoveltyIf::kNoveltyPitchChroma, 1000, m_fSampleRate));
//        for (auto k = 0; k < 6; k++)
//        {
//            m_pfInput[(k + 1) * k0] = 1.F / std::sqrt(k + 1.F);
//            fNorm += 1.F / std::sqrt(k + 1.F);
//        }
//        CHECK(Error_t::kNoError == m_pCInstance->calcNoveltyFromBlock(afResult, m_pfInput));
//        CHECK(afResult[9] == Approx(CVectorFloat::getMax(afResult, 12)).margin(1e-6F).epsilon(1e-6F));
//        CHECK(afResult[1] > 0);
//        CHECK(afResult[4] > 0);
//        CHECK(1.F == Approx(CVectorFloat::getSum(afResult, 12)).margin(1e-6F).epsilon(1e-6F));
//    }
//
//    SECTION("SpectralMfccs")
//    {
//        float afResult[13] = { 0,0,0,0,0,0,0,0,0,0,0,0,0 };
//        m_fSampleRate = 32000;
//        CHECK(Error_t::kNoError == CNoveltyFromBlockIf::create(m_pCInstance, CNoveltyIf::kNoveltyMfccs, m_iBufferLength, m_fSampleRate));
//        CHECK(13 == m_pCInstance->getNoveltyDimensions());
//
//        // ones
//        CHECK(Error_t::kNoError == m_pCInstance->calcNoveltyFromBlock(afResult, m_pfInput));
//        CHECK(0.F == Approx(CVectorFloat::getSum(&afResult[1], 12)).margin(1e-2F).epsilon(1e-2F));
//        CHECK(afResult[0] == Approx(CVectorFloat::getMin(afResult, 13)).margin(1e-6F).epsilon(1e-6F));
//        CHECK(-7.55021658F == Approx(afResult[0]).margin(1e-6F).epsilon(1e-6F));
//
//        // zeros
//        CVectorFloat::setZero(m_pfInput, m_iBufferLength);
//        CHECK(Error_t::kNoError == m_pCInstance->calcNoveltyFromBlock(afResult, m_pfInput));
//        CHECK(0.F == Approx(CVectorFloat::getMean(&afResult[1], 12)).margin(1e-6F).epsilon(1e-6F));
//        CHECK(afResult[0] == Approx(CVectorFloat::getMin(afResult, 13)).margin(1e-6F).epsilon(1e-6F));
//        CHECK(afResult[0] < -100);
//    }
//
//    SECTION("TimeRms")
//    {
//        float afResult[2] = { 0,0 };
//        m_fSampleRate = 32000;
//        CHECK(Error_t::kNoError == CNoveltyFromBlockIf::create(m_pCInstance, CNoveltyIf::kNoveltyTimeRms, m_iBufferLength, m_fSampleRate));
//        CHECK(2 == m_pCInstance->getNoveltyDimensions());
//
//        // zeros
//        CVectorFloat::setZero(m_pfInput, m_iBufferLength);
//        CHECK(Error_t::kNoError == m_pCInstance->calcNoveltyFromBlock(afResult, m_pfInput));
//        CHECK(0.F == Approx(afResult[1]).margin(1e-6F).epsilon(1e-6F));
//
//        // ones
//        float fTmp = 0;
//        CVectorFloat::setValue(m_pfInput, 1.F, m_iBufferLength);
//        for (auto n = 0; n < 2000; n++)
//        {
//            CHECK(Error_t::kNoError == m_pCInstance->calcNoveltyFromBlock(afResult, m_pfInput));
//            CHECK(fTmp <= afResult[1]);
//            fTmp = afResult[1];
//        }
//        CHECK(1.F == Approx(afResult[1]).margin(1e-4F).epsilon(1e-4F));
//
//        // zeros
//        CVectorFloat::setZero(m_pfInput, m_iBufferLength);
//        CHECK(Error_t::kNoError == m_pCInstance->calcNoveltyFromBlock(afResult, m_pfInput));
//        float fAlpha = CSinglePoleLp::calcFilterParam(.3F, m_fSampleRate);
//        CHECK(std::sqrt(fAlpha) == Approx(afResult[1]).margin(1e-4F).epsilon(1e-4F));
//        CHECK(Error_t::kNoError == m_pCInstance->calcNoveltyFromBlock(afResult, m_pfInput));
//        CHECK(std::pow(fAlpha, m_iBufferLength / 2.) == Approx(afResult[1]).margin(1e-3F).epsilon(1e-3F));
//    }
//
//    SECTION("TimePeakEnvelope")
//    {
//        float afResult[2] = { 0,0 };
//        m_fSampleRate = 32000;
//        CHECK(Error_t::kNoError == CNoveltyFromBlockIf::create(m_pCInstance, CNoveltyIf::kNoveltyTimePeakEnvelope, m_iBufferLength, m_fSampleRate));
//        CHECK(2 == m_pCInstance->getNoveltyDimensions());
//
//        // zeros
//        CVectorFloat::setZero(m_pfInput, m_iBufferLength);
//        CHECK(Error_t::kNoError == m_pCInstance->calcNoveltyFromBlock(afResult, m_pfInput));
//        CHECK(0.F == Approx(afResult[1]).margin(1e-6F).epsilon(1e-6F));
//
//        // ones
//        float fTmp = 0;
//        CVectorFloat::setValue(m_pfInput, 1.F, m_iBufferLength);
//        for (auto n = 0; n < 2000; n++)
//        {
//            CHECK(Error_t::kNoError == m_pCInstance->calcNoveltyFromBlock(afResult, m_pfInput));
//            CHECK(fTmp <= afResult[1]);
//            fTmp = afResult[1];
//        }
//        CHECK(1.F == Approx(afResult[1]).margin(1e-4F).epsilon(1e-4F));
//
//        // zeros
//        CVectorFloat::setZero(m_pfInput, m_iBufferLength);
//        CHECK(Error_t::kNoError == m_pCInstance->calcNoveltyFromBlock(afResult, m_pfInput));
//        float fAlpha = CSinglePoleLp::calcFilterParam(1.5F, m_fSampleRate);
//        CHECK(fAlpha == Approx(afResult[1]).margin(1e-4F).epsilon(1e-4F));
//        CHECK(Error_t::kNoError == m_pCInstance->calcNoveltyFromBlock(afResult, m_pfInput));
//        CHECK(std::pow(fAlpha, m_iBufferLength) == Approx(afResult[1]).margin(1e-3F).epsilon(1e-3F));
//    }
//
//    CNoveltyFromBlockIf::destroy(m_pCInstance);
//
//    delete[] m_pfInput;
//}
//
//TEST_CASE("Noveltys (per array)", "[NoveltysClass]")
//{
//
//    CNoveltyIf* m_pCInstance = 0;
//    float* m_pfInput = 0;
//    float m_fSampleRate = 44100;
//    int m_iBlockLength = 1023,
//        m_iHopLength = 512,
//        m_iBufferLength = 96000;
//    
//    m_pfInput = new float[m_iBufferLength];
//    CVectorFloat::setValue(m_pfInput, 1.F, m_iBufferLength);
//
//
//    SECTION("Api")
//    {
//        for (auto f = 0; f < CNoveltyIf::kNumNoveltys; f++)
//        {
//            int aiDim[2] = { 0,0 };
//            float** ppfNovelty = 0;
//            CHECK(Error_t::kFunctionInvalidArgsError == CNoveltyIf::create(m_pCInstance, static_cast<CNoveltyIf::Novelty_t>(f), 0, m_iBufferLength, m_fSampleRate, m_iBlockLength, m_iHopLength));
//            CHECK(Error_t::kFunctionInvalidArgsError == CNoveltyIf::create(m_pCInstance, static_cast<CNoveltyIf::Novelty_t>(f), m_pfInput, 0, m_fSampleRate, m_iBlockLength, m_iHopLength));
//            CHECK(Error_t::kFunctionInvalidArgsError == CNoveltyIf::create(m_pCInstance, static_cast<CNoveltyIf::Novelty_t>(f), m_pfInput, -1, m_fSampleRate, m_iBlockLength, m_iHopLength));
//            CHECK(Error_t::kFunctionInvalidArgsError == CNoveltyIf::create(m_pCInstance, static_cast<CNoveltyIf::Novelty_t>(f), m_pfInput, m_iBufferLength, 0, m_iBlockLength, m_iHopLength));
//            CHECK(Error_t::kFunctionInvalidArgsError == CNoveltyIf::create(m_pCInstance, static_cast<CNoveltyIf::Novelty_t>(f), m_pfInput, m_iBufferLength, m_fSampleRate, 0, m_iHopLength));
//            CHECK(Error_t::kFunctionInvalidArgsError == CNoveltyIf::create(m_pCInstance, static_cast<CNoveltyIf::Novelty_t>(f), m_pfInput, m_iBufferLength, m_fSampleRate, m_iBlockLength, 0));
//
//            CHECK(Error_t::kNoError == CNoveltyIf::create(m_pCInstance, static_cast<CNoveltyIf::Novelty_t>(f), m_pfInput, m_iBufferLength, m_fSampleRate, m_iBlockLength, m_iHopLength));
//
//            CHECK_FALSE(m_pCInstance == 0);
//
//            CHECK(Error_t::kNoError == m_pCInstance->getNoveltyDimensions(aiDim[0], aiDim[1]));
//            CHECK(aiDim[0] > 0);
//            CHECK(aiDim[1] > 0);
//
//            ppfNovelty = new float* [aiDim[0]];
//            for (auto i = 0; i < aiDim[0]; i++)
//                ppfNovelty[i] = new float[aiDim[1]];
//
//            CHECK(Error_t::kFunctionInvalidArgsError == m_pCInstance->getNovelty1Dim(0));
//            CHECK(Error_t::kFunctionInvalidArgsError == m_pCInstance->getNoveltyNDim(0));
//
//
//            CHECK(Error_t::kNoError == CNoveltyIf::destroy(m_pCInstance));
//
//            if (ppfNovelty)
//            {
//                for (auto i = 0; i < aiDim[0]; i++)
//                    delete[] ppfNovelty[i];
//            }
//            delete[] ppfNovelty;
//        }
//    }
//    CNoveltyIf::destroy(m_pCInstance);
//
//    delete[] m_pfInput;
//}

#endif //WITH_TESTS
