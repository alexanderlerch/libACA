#include "AcaAll.h"

#ifdef WITH_TESTS
#include "catch.hpp"

TEST_CASE("Fft", "[FFT]")
{
    float* pfTime = 0;
    CFft::complex_t* pfFreq = 0;
    float* pfReal = 0;
    float* pfImag = 0;
    float* pfTmp = 0;

    static const int iFftLength = 1024;

    CFft* pCFftInstance = 0;

    pfTime = new float[iFftLength];
    pfFreq = new float[iFftLength];
    pfReal = new float[iFftLength];
    pfImag = new float[iFftLength];
    pfTmp = new float[iFftLength];

    pCFftInstance = new CFft();
    pCFftInstance->init(iFftLength, 1, CFft::kWindowHann, CFft::kNoWindow);


    SECTION("Impulse")
    {
        // impulse with impulse
        int iBlockLength = 4;
        CVector::setZero(pfTime, iBlockLength);
        pfTime[1] = 1;
        pCFftInstance->init(iBlockLength, 1, CFft::kWindowHann, CFft::kNoWindow);
        pCFftInstance->compFft(pfFreq, pfTime);
        pCFftInstance->compInvFft(pfTmp, pfFreq);

        CHECK(1.F == Approx(pfTmp[1]).margin(1e-6F).epsilon(1e-6F));
        CHECK(1.F == Approx(CVector::getSum(pfTmp, iBlockLength)).margin(1e-6F).epsilon(1e-6F));

        pCFftInstance->init(iBlockLength, 2, CFft::kWindowHann, CFft::kNoWindow);
        pfTime[0] = 1;
        pfTime[1] = 0;
        pCFftInstance->compFft(pfFreq, pfTime);
        pCFftInstance->compInvFft(pfTmp, pfFreq);

        CHECK(1.F == Approx(pfTmp[0]).margin(1e-6F).epsilon(1e-6F));
        CHECK(1.F == Approx(CVector::getSum(pfTmp, iBlockLength)).margin(1e-6F).epsilon(1e-6F));

        pCFftInstance->init(iFftLength, 1, CFft::kWindowHann, CFft::kNoWindow);
    }

    SECTION("SimpleSine")
    {
        CSynthesis::genSine<float>(pfTime, 2.F, 1.F * iFftLength, iFftLength, 1.F, 0);

        pCFftInstance->compFft(pfFreq, pfTime);

        pCFftInstance->getPhase(pfTmp, pfFreq);
        CHECK(-M_PI_2 == Approx(pfTmp[2]).margin(1e-3F).epsilon(1e-3F));

        pCFftInstance->getMagnitude(pfTmp, pfFreq);
        for (int i = 0; i < iFftLength / 2 + 1; i++)
        {
            if (i != 2)
            {
                CHECK(0.F == Approx(pfTmp[i]).margin(1e-3F).epsilon(1e-3F));
            }
            else
            {
                CHECK(.5F == Approx(pfTmp[i]).margin(1e-3F).epsilon(1e-3F));
            }
        }

        pCFftInstance->splitRealImag(pfReal, pfImag, pfFreq);

        for (int i = 0; i < iFftLength / 2 + 1; i++)
            CHECK(0.F == Approx(pfReal[i]).margin(1e-3F).epsilon(1e-3F));
        for (int i = 0; i < iFftLength / 2 - 1; i++)
        {
            if (i != 2)
            {
                CHECK(0.F == Approx(pfImag[i]).margin(1e-3F).epsilon(1e-3F));
            }
            else
            {
                CHECK(-.5F == Approx(pfImag[i]).margin(1e-3F).epsilon(1e-3F));
            }
        }

        pCFftInstance->mergeRealImag(pfFreq, pfReal, pfImag);
        pCFftInstance->compInvFft(pfTmp, pfFreq);

        for (auto i = 0; i < iFftLength; i++)
            CHECK(pfTime[i] == Approx(pfTmp[i]).margin(1e-3F).epsilon(1e-3F));
    }

    SECTION("SimpleCos")
    {
        CSynthesis::genSine<float>(pfTime, 2.F, 1.F * iFftLength, iFftLength, 1.F, static_cast<float>(M_PI_2));

        pCFftInstance->compFft(pfFreq, pfTime);

        pCFftInstance->getPhase(pfTmp, pfFreq);
        CHECK(0.F == Approx(pfTmp[2]).margin(1e-3F).epsilon(1e-3F));

        pCFftInstance->getMagnitude(pfTmp, pfFreq);
        for (int i = 0; i < iFftLength / 2 + 1; i++)
        {
            if (i != 2)
            {
                CHECK(0.F == Approx(pfTmp[i]).margin(1e-3F).epsilon(1e-3F));
            }
            else
            {
                CHECK(.5F == Approx(pfTmp[i]).margin(1e-3F).epsilon(1e-3F));
            }
        }

        pCFftInstance->splitRealImag(pfReal, pfImag, pfFreq);

        for (int i = 0; i < iFftLength / 2; i++)
            CHECK(0.F == Approx(pfImag[i]).margin(1e-3F).epsilon(1e-3F));

        for (int i = 0; i < iFftLength / 2 + 1; i++)
        {
            if (i != 2)
            {
                CHECK(0.F == Approx(pfReal[i]).margin(1e-3F).epsilon(1e-3F));
            }
            else
            {
                CHECK(.5F == Approx(pfReal[i]).margin(1e-3F).epsilon(1e-3F));
            }
        }
        pCFftInstance->mergeRealImag(pfFreq, pfReal, pfImag);
        pCFftInstance->compInvFft(pfTmp, pfFreq);

        for (auto i = 0; i < iFftLength; i++)
            CHECK(pfTime[i] == Approx(pfTmp[i]).margin(1e-3F).epsilon(1e-3F));
    }

    SECTION("Hann")
    {
        const int iDataLength = iFftLength >> 3;

        pCFftInstance->reset();
        pCFftInstance->init(iDataLength, 8, CFft::kWindowHann, CFft::kPreWindow);
        CSynthesis::genDc(pfTime, iDataLength, 1.F);

        pCFftInstance->compFft(pfFreq, pfTime);

        //reuse real-value buffer
        pCFftInstance->getMagnitude(pfReal, pfFreq);

        CHECK(64.F / iFftLength == Approx(pfReal[0]).margin(1e-3F).epsilon(1e-3F));
        CHECK(0.F == Approx(pfReal[16]).margin(1e-3F).epsilon(1e-3F));
        CHECK(1.7077F / iFftLength == Approx(pfReal[19]).margin(1e-4F).epsilon(1e-4F));
    }

    SECTION("Hamming")
    {
        const int iDataLength = iFftLength >> 3;

        pCFftInstance->reset();
        pCFftInstance->init(iDataLength, 8, CFft::kWindowHamming, CFft::kPreWindow);
        CSynthesis::genDc(pfTime, iDataLength, 1.F);

        pCFftInstance->compFft(pfFreq, pfTime);

        //pCFftInstance->getWindow(pfReal);

        //reuse real-value buffer
        pCFftInstance->getMagnitude(pfReal, pfFreq);

        CHECK(69.12F / iFftLength == Approx(pfReal[0]).margin(1e-3F).epsilon(1e-3F));
        CHECK(0.F == Approx(pfReal[16]).margin(1e-3F).epsilon(1e-3F));
        CHECK(0.5113F / iFftLength == Approx(pfReal[36]).margin(1e-4F).epsilon(1e-4F));
    }

    SECTION("Inplace")
    {
        CSynthesis::genNoise(pfTime, iFftLength, 1.F);

        // compute fft inplace and compare
        pCFftInstance->compFft(pfFreq, pfTime);
        CVector::copy(pfTmp, pfTime, iFftLength);
        pCFftInstance->compFft(pfTmp, pfTmp);
        for (auto i = 0; i < iFftLength; i++)
            CHECK(pfFreq[i] == Approx(pfTmp[i]).margin(1e-3F).epsilon(1e-3F));

        // get magnitude in-place and compare
        pCFftInstance->getMagnitude(pfReal, pfFreq);
        CVector::copy(pfTmp, reinterpret_cast<float*>(pfFreq), iFftLength);
        pCFftInstance->getMagnitude(pfTmp, pfTmp);
        for (auto i = 0; i < pCFftInstance->getLength(CFft::kLengthMagnitude); i++)
            CHECK(pfReal[i] == Approx(pfTmp[i]).margin(1e-3F).epsilon(1e-3F));

        // get phase in-place and compare
        pCFftInstance->getPhase(pfReal, pfFreq);
        CVector::copy(pfTmp, reinterpret_cast<float*>(pfFreq), iFftLength);
        pCFftInstance->getPhase(pfTmp, pfTmp);
        for (auto i = 0; i < pCFftInstance->getLength(CFft::kLengthPhase); i++)
            CHECK(pfReal[i] == Approx(pfTmp[i]).margin(1e-3F).epsilon(1e-3F));

    }

    pCFftInstance->reset();
    delete pCFftInstance;

    delete[] pfReal;
    delete[] pfImag;
    delete[] pfFreq;
    delete[] pfTime;
    delete[] pfTmp;

}

#endif //WITH_TESTS

