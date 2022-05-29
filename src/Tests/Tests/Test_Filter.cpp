#include "ACAConfig.h"

#ifdef WITH_TESTS
#include "Synthesis.h"
#include "Filter.h"

#include "catch.hpp"

TEST_CASE("Filter", "[Filter]")
{
    const int iDataLength = 55;
    float* pfIn = 0;
    float* pfOut = 0;
    float* pfCoeffs = 0;


    CFilter<float>* pCFilter = new CFilter<float>();

    //float fSampleFreq = 44100;

    pfIn = new float[iDataLength];
    pfOut = new float[iDataLength];
    pfCoeffs = new float[iDataLength];
    CVectorFloat::setZero(pfIn, iDataLength);
    CVectorFloat::setZero(pfOut, iDataLength);
    CVectorFloat::setZero(pfCoeffs, iDataLength);
    //CSynthesis::genSine(pfData, 20.F, fSampleFreq, iDataLength, .7F, static_cast<float>(PI_2));

    SECTION("Api")
    {
        CHECK(Error_t::kFunctionInvalidArgsError == pCFilter->init(0, pfIn, 3));
        CHECK(Error_t::kFunctionInvalidArgsError == pCFilter->init(pfIn, 0, 3));
        CHECK(Error_t::kFunctionInvalidArgsError == pCFilter->init(pfIn, pfIn, 0));

        CHECK(Error_t::kFunctionIllegalCallError == pCFilter->process(pfIn, pfIn, 5));

        CHECK(Error_t::kNoError == pCFilter->init(pfIn, pfIn, 3));
        CHECK(Error_t::kFunctionInvalidArgsError == pCFilter->process(0, pfIn, 5));
        CHECK(Error_t::kFunctionInvalidArgsError == pCFilter->process(pfIn, 0, 5));
        CHECK(Error_t::kFunctionInvalidArgsError == pCFilter->process(pfIn, pfIn, 0));

        CHECK(Error_t::kNoError == pCFilter->process(pfIn, pfIn, 5));
        CHECK(Error_t::kNoError == pCFilter->reset());
    }

    SECTION("FIR")
    {
        int iNumCoeffs = 4;
        int iDelay = 3;
        pfIn[iDelay] = 1;
        CVectorFloat::setValue(pfCoeffs, 1.F / iNumCoeffs, iNumCoeffs);

        CHECK(Error_t::kNoError == pCFilter->init(&pfCoeffs[0], &pfCoeffs[iNumCoeffs], iNumCoeffs));

        CHECK(Error_t::kNoError == pCFilter->process(pfOut, pfIn, iDataLength));

        CHECK(0.F == Approx(CVectorFloat::getSum(pfOut, iDelay)).margin(1e-6F).epsilon(1e-6F));
        CHECK(0.F == Approx(CVectorFloat::getSum(&pfOut[iDelay + iNumCoeffs], iDataLength - iDelay - iNumCoeffs)).margin(1e-6F).epsilon(1e-6F));
        CHECK(1.F / iNumCoeffs == Approx(CVectorFloat::getMean(&pfOut[iDelay], iNumCoeffs)).margin(1e-6F).epsilon(1e-6F));
        CHECK(1.F / iNumCoeffs == Approx(CVectorFloat::getMax(&pfOut[iDelay], iNumCoeffs)).margin(1e-6F).epsilon(1e-6F));
        CHECK(Error_t::kNoError == pCFilter->reset());
    }

    SECTION("IIR")
    {
        int iNumCoeffs = 2;
        int iDelay = 3;
        pfIn[iDelay] = 1;
        pfCoeffs[0] = 1;
        pfCoeffs[iNumCoeffs] = 1;
        pfCoeffs[iNumCoeffs + 1] = -.2F;

        CHECK(Error_t::kNoError == pCFilter->init(&pfCoeffs[0], &pfCoeffs[iNumCoeffs], iNumCoeffs));

        CHECK(Error_t::kNoError == pCFilter->process(pfOut, pfIn, iDataLength));

        CHECK(0.F == Approx(CVectorFloat::getSum(pfOut, iDelay)).margin(1e-6F).epsilon(1e-6F));

        for (auto i = 0; i < 10; i++)
            CHECK(std::pow(-pfCoeffs[iNumCoeffs + 1], i) == Approx(pfOut[iDelay + i]).margin(1e-6F).epsilon(1e-6F));

        CHECK(Error_t::kNoError == pCFilter->reset());
        CVectorFloat::setZero(pfOut, iDataLength);

        CHECK(Error_t::kNoError == pCFilter->init(&pfCoeffs[0], &pfCoeffs[iNumCoeffs], iNumCoeffs));

        for (auto i = 0; i < iDataLength; i++)
            CHECK(Error_t::kNoError == pCFilter->process(&pfOut[i], &pfIn[i], 1));

        CHECK(0.F == Approx(CVectorFloat::getSum(pfOut, iDelay)).margin(1e-6F).epsilon(1e-6F));

        for (auto i = 0; i < 10; i++)
            CHECK(std::pow(-pfCoeffs[iNumCoeffs + 1], i) == Approx(pfOut[iDelay + i]).margin(1e-6F).epsilon(1e-6F));
    }
    SECTION("Double")
    {
        CFilter<double>* pCdFilter = new CFilter<double>();
        double afB[3] = { 164225.696795232 , -170761.327326444 , 0 };
        double afA[3] = { 1, -1.98546427012533, 0.985897137467758 };
        int iNumCoeffs = 3;

        double* pdIn = new double[1000];
        double* pdOut = new double[1000];
        CSynthesis::genSine<double>(pdIn, 100, 32000, 1000);
        
        CHECK(Error_t::kNoError == pCdFilter->init(&afB[0], &afA[0], iNumCoeffs));

        CHECK(Error_t::kNoError == pCdFilter->process(pdOut, pdIn, 1000));

        CHECK(1e20 > pdOut[999]);

        delete pCdFilter;
        delete[]pdIn;
        delete[] pdOut;
    }

    delete[] pfIn;
    delete[] pfOut;
    delete[] pfCoeffs;
    delete pCFilter;
}

#endif //WITH_TESTS
