#include "ACAConfig.h"

#ifdef WITH_TESTS
#include "Synthesis.h"

#include "Vector.h"
#include "Fft.h"
#include "catch.hpp"

TEST_CASE("Fft", "[FFT]")
{
    float* m_pfTime = 0;
    CFft::complex_t* m_pfFreq = 0;
    float* m_pfReal = 0;
    float* m_pfImag = 0;
    float* m_pfTmp = 0;

    static const int m_iFftLength = 1024;

    CFft* m_pCFftInstance = 0;

    m_pfTime = new float[m_iFftLength];
    m_pfFreq = new float[m_iFftLength];
    m_pfReal = new float[m_iFftLength];
    m_pfImag = new float[m_iFftLength];
    m_pfTmp = new float[m_iFftLength];

    m_pCFftInstance = new CFft();
    m_pCFftInstance->init(m_iFftLength, 1, CFft::kWindowHann, CFft::kNoWindow);


    SECTION("Impulse")
    {
        // impulse with impulse
        int iBlockLength = 4;
        CVector::setZero(m_pfTime, iBlockLength);
        m_pfTime[1] = 1;
        m_pCFftInstance->init(iBlockLength, 1, CFft::kWindowHann, CFft::kNoWindow);
        m_pCFftInstance->doFft(m_pfFreq, m_pfTime);
        m_pCFftInstance->doInvFft(m_pfTmp, m_pfFreq);

        CHECK(1.F == Approx(m_pfTmp[1]).margin(1e-6F).epsilon(1e-6F));
        CHECK(1.F == Approx(CVectorFloat::getSum(m_pfTmp, iBlockLength)).margin(1e-6F).epsilon(1e-6F));

        m_pCFftInstance->init(iBlockLength, 2, CFft::kWindowHann, CFft::kNoWindow);
        m_pfTime[0] = 1;
        m_pfTime[1] = 0;
        m_pCFftInstance->doFft(m_pfFreq, m_pfTime);
        m_pCFftInstance->doInvFft(m_pfTmp, m_pfFreq);

        CHECK(1.F == Approx(m_pfTmp[0]).margin(1e-6F).epsilon(1e-6F));
        CHECK(1.F == Approx(CVectorFloat::getSum(m_pfTmp, iBlockLength)).margin(1e-6F).epsilon(1e-6F));

        m_pCFftInstance->init(m_iFftLength, 1, CFft::kWindowHann, CFft::kNoWindow);
    }

    SECTION("SimpleSine")
    {
        CSynthesis::generateSine(m_pfTime, 2.F, 1.F * m_iFftLength, m_iFftLength, 1.F, 0);

        m_pCFftInstance->doFft(m_pfFreq, m_pfTime);

        m_pCFftInstance->getPhase(m_pfTmp, m_pfFreq);
        CHECK(-M_PI_2 == Approx(m_pfTmp[2]).margin(1e-3F).epsilon(1e-3F));

        m_pCFftInstance->getMagnitude(m_pfTmp, m_pfFreq);
        for (int i = 0; i < m_iFftLength / 2 + 1; i++)
        {
            if (i != 2)
            {
                CHECK(0.F == Approx(m_pfTmp[i]).margin(1e-3F).epsilon(1e-3F));
            }
            else
            {
                CHECK(.5F == Approx(m_pfTmp[i]).margin(1e-3F).epsilon(1e-3F));
            }
        }

        m_pCFftInstance->splitRealImag(m_pfReal, m_pfImag, m_pfFreq);

        for (int i = 0; i < m_iFftLength / 2 + 1; i++)
            CHECK(0.F == Approx(m_pfReal[i]).margin(1e-3F).epsilon(1e-3F));
        for (int i = 0; i < m_iFftLength / 2 - 1; i++)
        {
            if (i != 2)
            {
                CHECK(0.F == Approx(m_pfImag[i]).margin(1e-3F).epsilon(1e-3F));
            }
            else
            {
                CHECK(-.5F == Approx(m_pfImag[i]).margin(1e-3F).epsilon(1e-3F));
            }
        }

        m_pCFftInstance->mergeRealImag(m_pfFreq, m_pfReal, m_pfImag);
        m_pCFftInstance->doInvFft(m_pfTmp, m_pfFreq);

        for (auto i = 0; i < m_iFftLength; i++)
            CHECK(m_pfTime[i] == Approx(m_pfTmp[i]).margin(1e-3F).epsilon(1e-3F));
    }

    SECTION("SimpleCos")
    {
        CSynthesis::generateSine(m_pfTime, 2.F, 1.F * m_iFftLength, m_iFftLength, 1.F, static_cast<float>(M_PI_2));

        m_pCFftInstance->doFft(m_pfFreq, m_pfTime);

        m_pCFftInstance->getPhase(m_pfTmp, m_pfFreq);
        CHECK(0.F == Approx(m_pfTmp[2]).margin(1e-3F).epsilon(1e-3F));

        m_pCFftInstance->getMagnitude(m_pfTmp, m_pfFreq);
        for (int i = 0; i < m_iFftLength / 2 + 1; i++)
        {
            if (i != 2)
            {
                CHECK(0.F == Approx(m_pfTmp[i]).margin(1e-3F).epsilon(1e-3F));
            }
            else
            {
                CHECK(.5F == Approx(m_pfTmp[i]).margin(1e-3F).epsilon(1e-3F));
            }
        }

        m_pCFftInstance->splitRealImag(m_pfReal, m_pfImag, m_pfFreq);

        for (int i = 0; i < m_iFftLength / 2; i++)
            CHECK(0.F == Approx(m_pfImag[i]).margin(1e-3F).epsilon(1e-3F));

        for (int i = 0; i < m_iFftLength / 2 + 1; i++)
        {
            if (i != 2)
            {
                CHECK(0.F == Approx(m_pfReal[i]).margin(1e-3F).epsilon(1e-3F));
            }
            else
            {
                CHECK(.5F == Approx(m_pfReal[i]).margin(1e-3F).epsilon(1e-3F));
            }
        }
        m_pCFftInstance->mergeRealImag(m_pfFreq, m_pfReal, m_pfImag);
        m_pCFftInstance->doInvFft(m_pfTmp, m_pfFreq);

        for (auto i = 0; i < m_iFftLength; i++)
            CHECK(m_pfTime[i] == Approx(m_pfTmp[i]).margin(1e-3F).epsilon(1e-3F));
    }

    SECTION("Hann")
    {
        const int iDataLength = m_iFftLength >> 3;

        m_pCFftInstance->reset();
        m_pCFftInstance->init(iDataLength, 8, CFft::kWindowHann, CFft::kPreWindow);
        CSynthesis::generateDc(m_pfTime, iDataLength, 1.F);

        m_pCFftInstance->doFft(m_pfFreq, m_pfTime);

        //reuse real-value buffer
        m_pCFftInstance->getMagnitude(m_pfReal, m_pfFreq);

        CHECK(64.F / m_iFftLength == Approx(m_pfReal[0]).margin(1e-3F).epsilon(1e-3F));
        CHECK(0.F == Approx(m_pfReal[16]).margin(1e-3F).epsilon(1e-3F));
        CHECK(1.7077F / m_iFftLength == Approx(m_pfReal[19]).margin(1e-4F).epsilon(1e-4F));
    }

    SECTION("Hamming")
    {
        const int iDataLength = m_iFftLength >> 3;

        m_pCFftInstance->reset();
        m_pCFftInstance->init(iDataLength, 8, CFft::kWindowHamming, CFft::kPreWindow);
        CSynthesis::generateDc(m_pfTime, iDataLength, 1.F);

        m_pCFftInstance->doFft(m_pfFreq, m_pfTime);

        //m_pCFftInstance->getWindow(m_pfReal);

        //reuse real-value buffer
        m_pCFftInstance->getMagnitude(m_pfReal, m_pfFreq);

        CHECK(69.12F / m_iFftLength == Approx(m_pfReal[0]).margin(1e-3F).epsilon(1e-3F));
        CHECK(0.F == Approx(m_pfReal[16]).margin(1e-3F).epsilon(1e-3F));
        CHECK(0.5113F / m_iFftLength == Approx(m_pfReal[36]).margin(1e-4F).epsilon(1e-4F));
    }

    SECTION("Inplace")
    {
        CSynthesis::generateNoise(m_pfTime, m_iFftLength, 1.F);

        // compute fft inplace and compare
        m_pCFftInstance->doFft(m_pfFreq, m_pfTime);
        CVectorFloat::copy(m_pfTmp, m_pfTime, m_iFftLength);
        m_pCFftInstance->doFft(m_pfTmp, m_pfTmp);
        for (auto i = 0; i < m_iFftLength; i++)
            CHECK(m_pfFreq[i] == Approx(m_pfTmp[i]).margin(1e-3F).epsilon(1e-3F));

        // get magnitude in-place and compare
        m_pCFftInstance->getMagnitude(m_pfReal, m_pfFreq);
        CVectorFloat::copy(m_pfTmp, reinterpret_cast<float*>(m_pfFreq), m_iFftLength);
        m_pCFftInstance->getMagnitude(m_pfTmp, m_pfTmp);
        for (auto i = 0; i < m_pCFftInstance->getLength(CFft::kLengthMagnitude); i++)
            CHECK(m_pfReal[i] == Approx(m_pfTmp[i]).margin(1e-3F).epsilon(1e-3F));

        // get phase in-place and compare
        m_pCFftInstance->getPhase(m_pfReal, m_pfFreq);
        CVectorFloat::copy(m_pfTmp, reinterpret_cast<float*>(m_pfFreq), m_iFftLength);
        m_pCFftInstance->getPhase(m_pfTmp, m_pfTmp);
        for (auto i = 0; i < m_pCFftInstance->getLength(CFft::kLengthPhase); i++)
            CHECK(m_pfReal[i] == Approx(m_pfTmp[i]).margin(1e-3F).epsilon(1e-3F));

    }

    m_pCFftInstance->reset();
    delete m_pCFftInstance;

    delete[] m_pfReal;
    delete[] m_pfImag;
    delete[] m_pfFreq;
    delete[] m_pfTime;
    delete[] m_pfTmp;

}

#endif //WITH_TESTS

