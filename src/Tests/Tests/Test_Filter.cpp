#include "AcaAll.h"

#ifdef WITH_TESTS
#include "catch.hpp"

TEST_CASE("Filter", "[Filter]")
{
    int iDataLength = 55;
    float* pfIn = 0;
    float* pfOut = 0;
    float* pfCoeffs = 0;


    CFilter<float>* pCFilter = new CFilter<float>();

    //float fSampleFreq = 44100;

    pfIn = new float[1000*iDataLength];
    pfOut = new float[1000*iDataLength];
    pfCoeffs = new float[iDataLength];
    CVector::setZero(pfIn, static_cast<long long>(1000*iDataLength));
    CVector::setZero(pfOut, static_cast<long long>(1000*iDataLength));
    CVector::setZero(pfCoeffs, iDataLength);
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
        CVector::setValue(pfCoeffs, 1.F / iNumCoeffs, iNumCoeffs);

        CHECK(Error_t::kNoError == pCFilter->init(&pfCoeffs[0], &pfCoeffs[iNumCoeffs], iNumCoeffs));

        CHECK(Error_t::kNoError == pCFilter->process(pfOut, pfIn, iDataLength));

        CHECK(0.F == Approx(CVector::getSum(pfOut, iDelay)).margin(1e-6F).epsilon(1e-6F));
        CHECK(0.F == Approx(CVector::getSum(&pfOut[iDelay + iNumCoeffs], iDataLength - iDelay - iNumCoeffs)).margin(1e-6F).epsilon(1e-6F));
        CHECK(1.F / iNumCoeffs == Approx(CVector::getMean(&pfOut[iDelay], iNumCoeffs)).margin(1e-6F).epsilon(1e-6F));
        CHECK(1.F / iNumCoeffs == Approx(CVector::getMax(&pfOut[iDelay], iNumCoeffs)).margin(1e-6F).epsilon(1e-6F));
        CHECK(Error_t::kNoError == pCFilter->reset());
    }

    SECTION("Biquad")
    {
        int iNumCoeffs = 3;
        pfCoeffs[0] = 0.0200833655642112F; pfCoeffs[1] = 0.0401667311284225F; pfCoeffs[2] = 0.0200833655642112F;
        pfCoeffs[iNumCoeffs] = 1; pfCoeffs[iNumCoeffs+1] = - 1.56101807580072F; pfCoeffs[iNumCoeffs+2] = 0.641351538057563F;
        //butterlp, cutoff .1*fs/2

        CSynthesis::genSine(pfIn, 50.5F, 48000.F, static_cast<long long>(1000)*iDataLength);

        CHECK(Error_t::kNoError == pCFilter->init(&pfCoeffs[0], &pfCoeffs[iNumCoeffs], iNumCoeffs));
        CHECK(Error_t::kNoError == pCFilter->process(pfOut, pfIn, 1000*iDataLength));

        CHECK(1.F == Approx(CVector::getMax(pfOut, 1000*iDataLength)).margin(1e-4F).epsilon(1e-4F));

        CSynthesis::genSine(pfIn, 2401.F, 48000.F, static_cast<long long>(1000) * iDataLength);

        CHECK(Error_t::kNoError == pCFilter->init(&pfCoeffs[0], &pfCoeffs[iNumCoeffs], iNumCoeffs));
        CHECK(Error_t::kNoError == pCFilter->process(pfOut, pfIn, 1000 * iDataLength));

        CHECK(1.F/sqrt(2.F) == Approx(CVector::getMax(pfOut, 1000 * iDataLength)).margin(1e-1F).epsilon(1e-1F));
        CHECK(Error_t::kNoError == pCFilter->processDFII(pfIn, pfIn, 1000 * iDataLength));
        CHECK(1.F / sqrt(2.F) == Approx(CVector::getMax(pfIn, static_cast<long long>(1000) * iDataLength)).margin(1e-1F).epsilon(1e-1F));
        CVector::sub_I(pfOut, pfIn, static_cast<long long>(1000 * iDataLength));
        CHECK(0.F == Approx(CVector::getMax(pfOut, 1000 * iDataLength, true)).margin(1e-4F).epsilon(1e-4F));
    }

    SECTION("SinglePole")
    {
        int iNumCoeffs = 2;
        int iDelay = 3;
        pfIn[iDelay] = 1;
        pfCoeffs[0] = 1;
        pfCoeffs[iNumCoeffs] = 1;
        pfCoeffs[iNumCoeffs + 1] = -.2F;

        CHECK(Error_t::kNoError == pCFilter->init(&pfCoeffs[0], &pfCoeffs[iNumCoeffs], iNumCoeffs));

        CHECK(Error_t::kNoError == pCFilter->process(pfOut, pfIn, iDataLength));

        CHECK(0.F == Approx(CVector::getSum(pfOut, iDelay)).margin(1e-6F).epsilon(1e-6F));

        for (auto i = 0; i < 10; i++)
            CHECK(std::pow(-pfCoeffs[iNumCoeffs + 1], i) == Approx(pfOut[iDelay + i]).margin(1e-6F).epsilon(1e-6F));

        CHECK(Error_t::kNoError == pCFilter->reset());
        CVector::setZero(pfOut, iDataLength);

        CHECK(Error_t::kNoError == pCFilter->init(&pfCoeffs[0], &pfCoeffs[iNumCoeffs], iNumCoeffs));

        for (auto i = 0; i < iDataLength; i++)
            CHECK(Error_t::kNoError == pCFilter->process(&pfOut[i], &pfIn[i], 1));

        CHECK(0.F == Approx(CVector::getSum(pfOut, iDelay)).margin(1e-6F).epsilon(1e-6F));

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

    SECTION("Butter")
    {
        int iNumCoeffs = 3;
        pfCoeffs[0] = 0.0200833655642112F; pfCoeffs[1] = 0.0401667311284225F; pfCoeffs[2] = 0.0200833655642112F;
        pfCoeffs[iNumCoeffs] = 1; pfCoeffs[iNumCoeffs + 1] = -1.56101807580072F; pfCoeffs[iNumCoeffs + 2] = 0.641351538057563F;
 
        CVector::setValue(pfOut, 5.F, iDataLength);
        CButterLp::calcCoeffs<float>(pfOut, &pfOut[iNumCoeffs], 2, 0.1F);

        CVector::sub_I(pfCoeffs, pfOut, static_cast<long long>(2*iNumCoeffs));
        CHECK(0.F == Approx(CVector::getSum(pfCoeffs, iNumCoeffs)).margin(1e-6F).epsilon(1e-6F));
        CHECK(0.F == Approx(CVector::getSum(&pfCoeffs[iNumCoeffs], iNumCoeffs)).margin(1e-6F).epsilon(1e-6F));

    }

    SECTION("FiltFilt1")
    {
        int iNumCoeffs = 4;
        CVector::setValue(pfIn, 1.F, iDataLength);

        CVector::setValue(pfCoeffs, 1.F / iNumCoeffs, iNumCoeffs);

        CHECK(Error_t::kNoError == pCFilter->init(&pfCoeffs[0], &pfCoeffs[iNumCoeffs], iNumCoeffs));

        CHECK(Error_t::kNoError == pCFilter->filtfilt(pfOut, pfIn, iDataLength));

        CHECK(1.F == Approx(CVector::getMean(pfOut, iNumCoeffs)).margin(1e-6F).epsilon(1e-6F));
        CHECK(1.F == Approx(CVector::getMin(pfOut, iNumCoeffs)).margin(1e-6F).epsilon(1e-6F));
        CHECK(Error_t::kNoError == pCFilter->reset());
    }

    SECTION("FiltFilt2")
    {
        float afB[5] = { 0.00239804F, 0.00959217F, 0.01438826F, 0.00959217F, 0.00239804F };
        float afA[5] = { 1.F, -2.66616371F,  2.81905717F, -1.37171418F,  0.25718941F };
        int iNumCoeffs = 5;
        iDataLength = 13;
        CVector::setValue(pfIn, 1.F, iDataLength);

        CHECK(Error_t::kNoError == pCFilter->init(afB, afA, iNumCoeffs));

        CHECK(Error_t::kFunctionIllegalCallError == pCFilter->filtfilt(pfOut, pfIn, 5));
        CHECK(Error_t::kNoError == pCFilter->filtfilt(pfOut, pfIn, iDataLength));

        CHECK(1.F == Approx(CVector::getMean(pfOut, iNumCoeffs)).margin(1e-4F).epsilon(1e-4F));
        CHECK(1.F == Approx(CVector::getMin(pfOut, iNumCoeffs)).margin(1e-4F).epsilon(1e-4F));
    }

    SECTION("FiltFilt2")
    {
        float afB[5] = { 0.00239804F, 0.00959217F, 0.01438826F, 0.00959217F, 0.00239804F };
        float afA[5] = { 1.F, -2.66616371F,  2.81905717F, -1.37171418F,  0.25718941F };
        int iNumCoeffs = 5;
        iDataLength = 13;
        CVector::setValue(pfIn, .5F, iDataLength);

        CHECK(Error_t::kNoError == pCFilter->init(afB, afA, iNumCoeffs));

        CHECK(Error_t::kFunctionIllegalCallError == pCFilter->filtfilt(pfOut, pfIn, 5));
        CHECK(Error_t::kNoError == pCFilter->filtfilt(pfOut, pfIn, iDataLength));

        CHECK(.5F == Approx(CVector::getMean(pfOut, iNumCoeffs)).margin(1e-4F).epsilon(1e-4F));
        CHECK(.5F == Approx(CVector::getMin(pfOut, iNumCoeffs)).margin(1e-4F).epsilon(1e-4F));
    }

    delete[] pfIn;
    delete[] pfOut;
    delete[] pfCoeffs;
    delete pCFilter;
}

#endif //WITH_TESTS
