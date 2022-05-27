#include "ACAConfig.h"

#ifdef WITH_TESTS
#include "Synthesis.h"
#include "Filter.h"

#include "catch.hpp"

TEST_CASE("Filter", "[Filter]")
{
    const int m_iDataLength = 55;
    float* m_pfIn = 0;
    float* m_pfOut = 0;
    float* m_pfCoeffs = 0;


    CFilter<float>* m_pCFilter = new CFilter<float>();

    //float fSampleFreq = 44100;

    m_pfIn = new float[m_iDataLength];
    m_pfOut = new float[m_iDataLength];
    m_pfCoeffs = new float[m_iDataLength];
    CVectorFloat::setZero(m_pfIn, m_iDataLength);
    CVectorFloat::setZero(m_pfOut, m_iDataLength);
    CVectorFloat::setZero(m_pfCoeffs, m_iDataLength);
    //CSynthesis::genSine(m_pfData, 20.F, fSampleFreq, m_iDataLength, .7F, static_cast<float>(M_PI_2));

    SECTION("Api")
    {
        CHECK(Error_t::kFunctionInvalidArgsError == m_pCFilter->init(0, m_pfIn, 3));
        CHECK(Error_t::kFunctionInvalidArgsError == m_pCFilter->init(m_pfIn, 0, 3));
        CHECK(Error_t::kFunctionInvalidArgsError == m_pCFilter->init(m_pfIn, m_pfIn, 0));

        CHECK(Error_t::kFunctionIllegalCallError == m_pCFilter->process(m_pfIn, m_pfIn, 5));

        CHECK(Error_t::kNoError == m_pCFilter->init(m_pfIn, m_pfIn, 3));
        CHECK(Error_t::kFunctionInvalidArgsError == m_pCFilter->process(0, m_pfIn, 5));
        CHECK(Error_t::kFunctionInvalidArgsError == m_pCFilter->process(m_pfIn, 0, 5));
        CHECK(Error_t::kFunctionInvalidArgsError == m_pCFilter->process(m_pfIn, m_pfIn, 0));

        CHECK(Error_t::kNoError == m_pCFilter->process(m_pfIn, m_pfIn, 5));
        CHECK(Error_t::kNoError == m_pCFilter->reset());
    }

    SECTION("FIR")
    {
        int iNumCoeffs = 4;
        int iDelay = 3;
        m_pfIn[iDelay] = 1;
        CVectorFloat::setValue(m_pfCoeffs, 1.F / iNumCoeffs, iNumCoeffs);

        CHECK(Error_t::kNoError == m_pCFilter->init(&m_pfCoeffs[0], &m_pfCoeffs[iNumCoeffs], iNumCoeffs));

        CHECK(Error_t::kNoError == m_pCFilter->process(m_pfOut, m_pfIn, m_iDataLength));

        CHECK(0.F == Approx(CVectorFloat::getSum(m_pfOut, iDelay)).margin(1e-6F).epsilon(1e-6F));
        CHECK(0.F == Approx(CVectorFloat::getSum(&m_pfOut[iDelay + iNumCoeffs], m_iDataLength - iDelay - iNumCoeffs)).margin(1e-6F).epsilon(1e-6F));
        CHECK(1.F / iNumCoeffs == Approx(CVectorFloat::getMean(&m_pfOut[iDelay], iNumCoeffs)).margin(1e-6F).epsilon(1e-6F));
        CHECK(1.F / iNumCoeffs == Approx(CVectorFloat::getMax(&m_pfOut[iDelay], iNumCoeffs)).margin(1e-6F).epsilon(1e-6F));
        CHECK(Error_t::kNoError == m_pCFilter->reset());
    }

    SECTION("IIR")
    {
        int iNumCoeffs = 2;
        int iDelay = 3;
        m_pfIn[iDelay] = 1;
        m_pfCoeffs[0] = 1;
        m_pfCoeffs[iNumCoeffs] = 1;
        m_pfCoeffs[iNumCoeffs+1] = -.2F;

        CHECK(Error_t::kNoError == m_pCFilter->init(&m_pfCoeffs[0], &m_pfCoeffs[iNumCoeffs], iNumCoeffs));

        CHECK(Error_t::kNoError == m_pCFilter->process(m_pfOut, m_pfIn, m_iDataLength));

        CHECK(0.F == Approx(CVectorFloat::getSum(m_pfOut, iDelay)).margin(1e-6F).epsilon(1e-6F));

        for(auto i = 0; i < 10; i++)
            CHECK(std::pow(-m_pfCoeffs[iNumCoeffs + 1], i) == Approx(m_pfOut[iDelay + i]).margin(1e-6F).epsilon(1e-6F));
    }

    delete[] m_pfIn;
    delete[] m_pfOut;
    delete[] m_pfCoeffs;
    delete m_pCFilter;
}

#endif //WITH_TESTS
