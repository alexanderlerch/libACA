#include "MUSI6106Config.h"

#ifdef WITH_TESTS
#include "Synthesis.h"
#include "RingBuffer.h"


#include "gtest/gtest.h"


namespace {
    void CHECK_ARRAY_CLOSE(float* buffer1, float* buffer2, int iLength, float fTolerance)
    {
        for (int i = 0; i < iLength; i++)
        {
            EXPECT_NEAR(buffer1[i], buffer2[i], fTolerance);
        }
    }

    class RingBuffer : public testing::Test
    {
    protected:
        void SetUp() override
        {
            const float fSampleFreq = 44100;

            m_pCRingBuffer = new CRingBuffer<float>(m_iRingBuffLength);
            m_pfData = new float[m_iDataLength];
            CSynthesis::generateSine(m_pfData, 20.F, fSampleFreq, m_iDataLength, .7F, static_cast<float>(M_PI_2));
        }

        virtual void TearDown()
        {
            delete[] m_pfData;
            delete m_pCRingBuffer;
        }

 
        static const int m_iRingBuffLength;
        static const int m_iDataLength;
        static const int m_iDelay;

        CRingBuffer<float> *m_pCRingBuffer = 0;

        float   *m_pfData = 0;
    };

    const int RingBuffer::m_iRingBuffLength = 16;
    const int RingBuffer::m_iDataLength     = 55;
    const int RingBuffer::m_iDelay          = 9;

    TEST_F(RingBuffer, RbDelay)
    {
        // put initial values into ring buffer
        for (int i = 0; i < m_iDelay; i++)
        {
            m_pCRingBuffer->putPostInc (m_pfData[i]);
        }

        for (int i = m_iDelay; i < m_iDataLength; i++)
        {
            EXPECT_EQ(m_iDelay, m_pCRingBuffer->getNumValuesInBuffer());
            m_pCRingBuffer->getPostInc (); // just to increment - ignore the value
            m_pCRingBuffer->putPostInc (m_pfData[i]);
        }
    }

    TEST_F(RingBuffer, RbValues)
    {
        int iDataBuffIdx    = 0;

        // put initial values into ring buffer
        for (int i = 0; i < m_iDelay; i++)
        {
            m_pCRingBuffer->putPostInc (m_pfData[i]);
        }

        for (int i = m_iDelay; i < m_iDataLength; i++, iDataBuffIdx++)
        {
            float fValue = m_pCRingBuffer->getPostInc ();
            EXPECT_NEAR(fValue, m_pfData[iDataBuffIdx], 1e-3F);
            m_pCRingBuffer->putPostInc (m_pfData[i]);
        }
    }

    TEST_F(RingBuffer, RbReset)
    {
        // put initial values into ring buffer
        for (int i = 0; i < m_iDelay; i++)
        {
            m_pCRingBuffer->putPostInc (m_pfData[i]);
        }
        m_pCRingBuffer->reset ();
        EXPECT_EQ(0, m_pCRingBuffer->getNumValuesInBuffer());

        for (int i = 0; i < m_iRingBuffLength; i++)
        {
            float fValue = m_pCRingBuffer->getPostInc ();
            EXPECT_EQ (0.F, fValue);

            EXPECT_EQ(m_iRingBuffLength-(i+1), m_pCRingBuffer->getNumValuesInBuffer());
        }
    }

    //===========================================
    //===========================================
    //===========================================
    TEST_F(RingBuffer, RbPutGetRandom)
    {
        // generate random new index (can go negative)
        int newIdx = rand() - (RAND_MAX)/2;

        // generate random new sample value (-1.0 to 1.0)
        float newValue = static_cast <float>(rand()) / static_cast <float>(RAND_MAX) * 2.F - 1.F;

        // set write index and put new value
        m_pCRingBuffer->setWriteIdx(newIdx);
        m_pCRingBuffer->put(newValue);

        // test reading it
        m_pCRingBuffer->setReadIdx(newIdx);
        EXPECT_EQ(newValue, m_pCRingBuffer->get());;

        // extra checking for the both index getters
        EXPECT_EQ(m_pCRingBuffer->getWriteIdx(), m_pCRingBuffer->getReadIdx());
    }

    TEST_F(RingBuffer, RbAccessors)
    {
        m_pCRingBuffer->setReadIdx(rand()%20);
        m_pCRingBuffer->setWriteIdx(m_pCRingBuffer->getReadIdx());
        EXPECT_EQ(0, m_pCRingBuffer->getNumValuesInBuffer());

    }

    TEST_F(RingBuffer, RbSetReadIdx)
    {
        // put initial values into ring buffer
        int i= 0;
        for (i = 0; i < m_iDelay; i++)
        {
            m_pCRingBuffer->putPostInc (m_pfData[i]);
        }
        m_pCRingBuffer->setReadIdx(i-1);
        EXPECT_EQ(m_pfData[i-1] , m_pCRingBuffer->get());
    }

    TEST_F(RingBuffer, RbGetSetIdx)
    {
        int test_Value = 10;

        m_pCRingBuffer->setReadIdx(test_Value);
        m_pCRingBuffer->setWriteIdx(test_Value);

        EXPECT_EQ(test_Value, m_pCRingBuffer->getReadIdx());
        EXPECT_EQ(test_Value, m_pCRingBuffer->getWriteIdx());

        m_pCRingBuffer->put(static_cast<float>(test_Value));
        m_pCRingBuffer->putPostInc(static_cast<float>(test_Value));

        EXPECT_EQ(static_cast<float>(test_Value), m_pCRingBuffer->get());
        EXPECT_EQ(static_cast<float>(test_Value), m_pCRingBuffer->getPostInc());
        EXPECT_EQ(static_cast<float>(test_Value), m_pCRingBuffer->get(-1));
        EXPECT_EQ(.0F, m_pCRingBuffer->get());

        //        Edge Cases
        m_pCRingBuffer->setReadIdx(RingBuffer::m_iRingBuffLength);
        m_pCRingBuffer->setWriteIdx(RingBuffer::m_iRingBuffLength);

        EXPECT_EQ(0, m_pCRingBuffer->getReadIdx());
        EXPECT_EQ(0, m_pCRingBuffer->getWriteIdx());

        m_pCRingBuffer->setReadIdx(RingBuffer::m_iRingBuffLength-1);
        m_pCRingBuffer->setWriteIdx(RingBuffer::m_iRingBuffLength-1);

        EXPECT_EQ(RingBuffer::m_iRingBuffLength - 1, m_pCRingBuffer->getReadIdx());
        EXPECT_EQ(RingBuffer::m_iRingBuffLength - 1, m_pCRingBuffer->getWriteIdx());

        m_pCRingBuffer->put(static_cast<float>(std::numeric_limits<int>::max()));
        m_pCRingBuffer->putPostInc(static_cast<float>(std::numeric_limits<int>::max()));

        EXPECT_EQ(static_cast<float>(std::numeric_limits<int>::max()), m_pCRingBuffer->get());
        EXPECT_EQ(static_cast<float>(std::numeric_limits<int>::max()), m_pCRingBuffer->getPostInc());        

        //        negative indices
        m_pCRingBuffer->setReadIdx(-1);
        m_pCRingBuffer->setWriteIdx(-1);
        EXPECT_EQ(RingBuffer::m_iRingBuffLength-1, m_pCRingBuffer->getReadIdx());
        EXPECT_EQ(RingBuffer::m_iRingBuffLength-1, m_pCRingBuffer->getWriteIdx());
        m_pCRingBuffer->setReadIdx(-RingBuffer::m_iRingBuffLength+1);
        m_pCRingBuffer->setWriteIdx(-RingBuffer::m_iRingBuffLength+1);
        EXPECT_EQ(1, m_pCRingBuffer->getReadIdx());
        EXPECT_EQ(1, m_pCRingBuffer->getWriteIdx());

        // extreme indices
        m_pCRingBuffer->setReadIdx(RingBuffer::m_iRingBuffLength * 5 - 1);
        m_pCRingBuffer->setWriteIdx(RingBuffer::m_iRingBuffLength * 5 - 1);
        EXPECT_EQ(RingBuffer::m_iRingBuffLength - 1, m_pCRingBuffer->getReadIdx());
        EXPECT_EQ(RingBuffer::m_iRingBuffLength - 1, m_pCRingBuffer->getWriteIdx());
        m_pCRingBuffer->setReadIdx(-RingBuffer::m_iRingBuffLength * 5 - 1);
        m_pCRingBuffer->setWriteIdx(-RingBuffer::m_iRingBuffLength * 5 - 1);
        EXPECT_EQ(RingBuffer::m_iRingBuffLength - 1, m_pCRingBuffer->getReadIdx());
        EXPECT_EQ(RingBuffer::m_iRingBuffLength - 1, m_pCRingBuffer->getWriteIdx());
    }

    // Simple test to check for overflow
    TEST_F(RingBuffer, overflowTest)
    {

        for (int i = 0; i < 2*m_iRingBuffLength; i++)
        {
            m_pCRingBuffer->putPostInc (static_cast<float>(i));
        }

        for (int i=0;i< m_iRingBuffLength; i++)
        {
            EXPECT_EQ(i + m_iRingBuffLength,m_pCRingBuffer->getPostInc());
        }
    }

    TEST_F(RingBuffer, RbReadBlock)
    {
        for (int i = 0; i < 2*m_iRingBuffLength; i++)
        {
            m_pCRingBuffer->putPostInc (static_cast<float>(i));
        }
        m_pCRingBuffer->setReadIdx(5);
        m_pCRingBuffer->getPostInc(m_pfData, m_iRingBuffLength);

        for (int i=0;i< m_iRingBuffLength; i++)
        {
            EXPECT_EQ((i+5)%m_iRingBuffLength + m_iRingBuffLength,m_pfData[i]);
        }
    }

    TEST_F(RingBuffer, RbWriteBlock)
    {
        m_pCRingBuffer->putPostInc (m_pfData, 11);
        m_pCRingBuffer->putPostInc (&m_pfData[11], m_iRingBuffLength);
        m_pCRingBuffer->setReadIdx(11);

        for (int i = 11; i < 11+m_iRingBuffLength; i++)
        {
            EXPECT_EQ(m_pfData[i], m_pCRingBuffer->getPostInc());
        }
    }

    TEST_F(RingBuffer, RbFracDelay)
    {
        for (int i = 0; i < m_iRingBuffLength; i++)
            m_pCRingBuffer->putPostInc (1.F*i);

        float fValue    = m_pCRingBuffer->get(.7F);
        EXPECT_NEAR(.7F, fValue, 1e-4F);

        fValue = m_pCRingBuffer->get(-.5F);
        EXPECT_NEAR(7.5, fValue, 1e-4F);

        fValue = m_pCRingBuffer->get(-1.8F);
        EXPECT_NEAR(14.2F, fValue, 1e-4F);

        m_pCRingBuffer->setReadIdx(1);
        fValue          = m_pCRingBuffer->get(-m_iRingBuffLength+1.F);
        EXPECT_NEAR(2.F, fValue, 1e-4F);
    }
}

#endif //WITH_TESTS
