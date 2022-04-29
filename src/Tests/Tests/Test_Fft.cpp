#include "ACAConfig.h"

#ifdef WITH_TESTS
#include "Synthesis.h"

#include "Vector.h"
#include "Fft.h"

#include "gtest/gtest.h"


namespace fft_test {
    void CHECK_ARRAY_CLOSE(float* buffer1, float* buffer2, int iLength, float fTolerance)
    {
        for (int i = 0; i < iLength; i++)
        {
            EXPECT_NEAR(buffer1[i], buffer2[i], fTolerance);
        }
    }

    class Fft: public testing::Test
    {
    protected:
        void SetUp() override
        {
            m_pfTime = new float[m_iFftLength];
            m_pfFreq = new float[m_iFftLength];
            m_pfReal = new float[m_iFftLength];
            m_pfImag = new float[m_iFftLength];
            m_pfTmp = new float[m_iFftLength];

            m_pCFftInstance = new CFft();
            m_pCFftInstance->init(m_iFftLength, 1, CFft::kWindowHann, CFft::kNoWindow);
        }

        virtual void TearDown()
        {
            m_pCFftInstance->reset();
            delete m_pCFftInstance;

            delete[] m_pfReal;
            delete[] m_pfImag;
            delete[] m_pfFreq;
            delete[] m_pfTime;
            delete[] m_pfTmp;
        }

        float *m_pfTime;
        CFft::complex_t *m_pfFreq;
        float *m_pfReal;
        float *m_pfImag;
        float *m_pfTmp;

        static const int m_iFftLength  = 1024;

        CFft *m_pCFftInstance;
    };

    TEST_F(Fft, Impulse)
    {
        // impulse with impulse
        int iBlockLength = 4;
        CVector::setZero(m_pfTime, iBlockLength);
        m_pfTime[1] = 1;
        m_pCFftInstance->init(iBlockLength, 1, CFft::kWindowHann, CFft::kNoWindow);
        m_pCFftInstance->doFft(m_pfFreq, m_pfTime);
        m_pCFftInstance->doInvFft(m_pfTmp, m_pfFreq);

        EXPECT_NEAR(1.F, m_pfTmp[1], 1e-6);
        EXPECT_NEAR(1.F, CVectorFloat::getSum(m_pfTmp, iBlockLength), 1e-6);

        m_pCFftInstance->init(iBlockLength, 2, CFft::kWindowHann, CFft::kNoWindow);
        m_pfTime[0] = 1;
        m_pfTime[1] = 0;
        m_pCFftInstance->doFft(m_pfFreq, m_pfTime);
        m_pCFftInstance->doInvFft(m_pfTmp, m_pfFreq);

        EXPECT_NEAR(1.F, m_pfTmp[0], 1e-6);
        EXPECT_NEAR(1.F, CVectorFloat::getSum(m_pfTmp, iBlockLength), 1e-6);

        m_pCFftInstance->init(m_iFftLength, 1, CFft::kWindowHann, CFft::kNoWindow);
    }
    
    TEST_F(Fft, SimpleSine)
    {
        CSynthesis::generateSine(m_pfTime, 2.F, 1.F*m_iFftLength, m_iFftLength, 1.F, 0);
        
        m_pCFftInstance->doFft(m_pfFreq, m_pfTime);

        m_pCFftInstance->getPhase (m_pfTmp, m_pfFreq);
        EXPECT_NEAR(-M_PI_2, m_pfTmp[2], 1e-3F);

        m_pCFftInstance->getMagnitude (m_pfTmp, m_pfFreq);
        for (int i=0; i < m_iFftLength/2+1; i++)
        {
            if (i!=2)
            {
                EXPECT_NEAR(0, m_pfTmp[i], 1e-3F); 
            }
            else
            {
                EXPECT_NEAR(.5, m_pfTmp[i], 1e-3F); 
            }
        }

        m_pCFftInstance->splitRealImag (m_pfReal, m_pfImag, m_pfFreq);

        for (int i=0; i < m_iFftLength/2+1; i++)
            EXPECT_NEAR(0, m_pfReal[i], 1e-3F); 
        for (int i=0; i < m_iFftLength/2-1; i++)
        {
            if (i!=2)
            {
                EXPECT_NEAR(0, m_pfImag[i], 1e-3F); 
            }
            else
            {
                EXPECT_NEAR(-.5, m_pfImag[i], 1e-3F); 
            }
        }

        m_pCFftInstance->mergeRealImag (m_pfFreq, m_pfReal, m_pfImag);
        m_pCFftInstance->doInvFft(m_pfTmp, m_pfFreq);

        CHECK_ARRAY_CLOSE(m_pfTime, m_pfTmp, m_iFftLength, 1e-3F);
    }

    TEST_F(Fft, SimpleCos)
    {
        CSynthesis::generateSine(m_pfTime, 2.F, 1.F*m_iFftLength, m_iFftLength, 1.F, static_cast<float>(M_PI_2));

        m_pCFftInstance->doFft(m_pfFreq, m_pfTime);

        m_pCFftInstance->getPhase (m_pfTmp, m_pfFreq);
        EXPECT_NEAR(0, m_pfTmp[2], 1e-3F);

        m_pCFftInstance->getMagnitude (m_pfTmp, m_pfFreq);
        for (int i=0; i < m_iFftLength/2+1; i++)
        {
            if (i!=2)
            {
                EXPECT_NEAR(0, m_pfTmp[i], 1e-3F); 
            }
            else
            {
                EXPECT_NEAR(.5, m_pfTmp[i], 1e-3F); 
            }
        }

        m_pCFftInstance->splitRealImag (m_pfReal, m_pfImag, m_pfFreq);

        for (int i=0; i < m_iFftLength/2; i++)
            EXPECT_NEAR(0, m_pfImag[i], 1e-3F); 

        for (int i=0; i < m_iFftLength/2+1; i++)
        {
            if (i!=2)
            {
                EXPECT_NEAR(0, m_pfReal[i], 1e-3F); 
            }
            else
            {
                EXPECT_NEAR(.5, m_pfReal[i], 1e-3F); 
            }
        }
        m_pCFftInstance->mergeRealImag (m_pfFreq, m_pfReal, m_pfImag);
        m_pCFftInstance->doInvFft(m_pfTmp, m_pfFreq);

        CHECK_ARRAY_CLOSE(m_pfTime, m_pfTmp, m_iFftLength, 1e-3F);
    }

    TEST_F(Fft, Hann)
    {
        const int iDataLength = m_iFftLength>>3;

        m_pCFftInstance->reset();
        m_pCFftInstance->init(iDataLength, 8, CFft::kWindowHann, CFft::kPreWindow);
        CSynthesis::generateDc(m_pfTime, iDataLength, 1.F);

        m_pCFftInstance->doFft(m_pfFreq, m_pfTime);

        //reuse real-value buffer
        m_pCFftInstance->getMagnitude(m_pfReal, m_pfFreq);

        EXPECT_NEAR(64.F/m_iFftLength, m_pfReal[0], 1e-3F); 
        EXPECT_NEAR(0, m_pfReal[16], 1e-3F); 
        EXPECT_NEAR(1.7077F/m_iFftLength, m_pfReal[19], 1e-4F);
    }

    TEST_F(Fft, Hamming)
    {
        const int iDataLength = m_iFftLength>>3;

        m_pCFftInstance->reset();
        m_pCFftInstance->init(iDataLength, 8, CFft::kWindowHamming, CFft::kPreWindow);
        CSynthesis::generateDc(m_pfTime, iDataLength, 1.F);

        m_pCFftInstance->doFft(m_pfFreq, m_pfTime);

        //m_pCFftInstance->getWindow(m_pfReal);

        //reuse real-value buffer
        m_pCFftInstance->getMagnitude(m_pfReal, m_pfFreq);

        EXPECT_NEAR(69.12F/m_iFftLength, m_pfReal[0], 1e-3F); 
        EXPECT_NEAR(0.F, m_pfReal[16], 1e-3F); 
        EXPECT_NEAR(0.5113F/m_iFftLength, m_pfReal[36], 1e-4F);
    }

    TEST_F(Fft, Inplace)
    {
        CSynthesis::generateNoise(m_pfTime, m_iFftLength, 1.F);

        // compute fft inplace and compare
        m_pCFftInstance->doFft(m_pfFreq, m_pfTime);
        CVectorFloat::copy(m_pfTmp, m_pfTime, m_iFftLength);
        m_pCFftInstance->doFft(m_pfTmp, m_pfTmp);
        CHECK_ARRAY_CLOSE(m_pfFreq, m_pfTmp, m_iFftLength, 1e-3F);

        // get magnitude in-place and compare
        m_pCFftInstance->getMagnitude(m_pfReal, m_pfFreq);
        CVectorFloat::copy(m_pfTmp, reinterpret_cast<float*>(m_pfFreq), m_iFftLength);
        m_pCFftInstance->getMagnitude(m_pfTmp, m_pfTmp);
        CHECK_ARRAY_CLOSE(m_pfReal, m_pfTmp, m_pCFftInstance->getLength(CFft::kLengthMagnitude), 1e-3F);

        // get phase in-place and compare
        m_pCFftInstance->getPhase(m_pfReal, m_pfFreq);
        CVectorFloat::copy(m_pfTmp, reinterpret_cast<float*>(m_pfFreq), m_iFftLength);
        m_pCFftInstance->getPhase(m_pfTmp, m_pfTmp);
        CHECK_ARRAY_CLOSE(m_pfReal, m_pfTmp, m_pCFftInstance->getLength(CFft::kLengthPhase), 1e-3F);
    }
}

#endif //WITH_TESTS

