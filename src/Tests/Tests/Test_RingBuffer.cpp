#include "ACAConfig.h"

#ifdef WITH_TESTS
#include "Synthesis.h"
#include "RingBuffer.h"

#include "catch.hpp"

TEST_CASE("RingBuffer", "[RingBuffer]")
{
    const int m_iRingBuffLength = 16;
    const int m_iDataLength = 55;
    const int m_iDelay = 9;

    CRingBuffer<float>* m_pCRingBuffer = 0;

    float* m_pfData = 0;
    const float fSampleFreq = 44100;

    m_pCRingBuffer = new CRingBuffer<float>(m_iRingBuffLength);
    m_pfData = new float[m_iDataLength];
    CSynthesis::generateSine(m_pfData, 20.F, fSampleFreq, m_iDataLength, .7F, static_cast<float>(M_PI_2));


SECTION("RbDelay")
    {
        // put initial values into ring buffer
        for (int i = 0; i < m_iDelay; i++)
        {
            m_pCRingBuffer->putPostInc(m_pfData[i]);
        }

        for (int i = m_iDelay; i < m_iDataLength; i++)
        {
            CHECK(m_iDelay == m_pCRingBuffer->getNumValuesInBuffer());
            m_pCRingBuffer->getPostInc(); // just to increment - ignore the value
            m_pCRingBuffer->putPostInc(m_pfData[i]);
        }
    }

    SECTION("RbValues")
    {
        int iDataBuffIdx = 0;

        // put initial values into ring buffer
        for (int i = 0; i < m_iDelay; i++)
        {
            m_pCRingBuffer->putPostInc(m_pfData[i]);
        }

        for (int i = m_iDelay; i < m_iDataLength; i++, iDataBuffIdx++)
        {
            float fValue = m_pCRingBuffer->getPostInc();
            CHECK(fValue == Approx(m_pfData[iDataBuffIdx]).margin(1e-3F).epsilon(1e-3F));
            m_pCRingBuffer->putPostInc(m_pfData[i]);
        }
    }

    SECTION("RbReset")
    {
        // put initial values into ring buffer
        for (int i = 0; i < m_iDelay; i++)
        {
            m_pCRingBuffer->putPostInc(m_pfData[i]);
        }
        m_pCRingBuffer->reset();
        CHECK(0 ==m_pCRingBuffer->getNumValuesInBuffer());

        for (int i = 0; i < m_iRingBuffLength; i++)
        {
            float fValue = m_pCRingBuffer->getPostInc();
            CHECK(0.F == Approx(fValue).margin(1e-6F).epsilon(1e-6F));

            CHECK(m_iRingBuffLength - (i + 1) == m_pCRingBuffer->getNumValuesInBuffer());
        }
    }

    //===========================================
    //===========================================
    //===========================================
    SECTION("RbPutGetRandom")
    {
        // generate random new index (can go negative)
        int newIdx = rand() - (RAND_MAX) / 2;

        // generate random new sample value (-1.0 to 1.0)
        float newValue = static_cast <float>(rand()) / static_cast <float>(RAND_MAX) * 2.F - 1.F;

        // set write index and put new value
        m_pCRingBuffer->setWriteIdx(newIdx);
        m_pCRingBuffer->put(newValue);

        // test reading it
        m_pCRingBuffer->setReadIdx(newIdx);
        CHECK(newValue == m_pCRingBuffer->get());;

        // extra checking for the both index getters
        CHECK(m_pCRingBuffer->getWriteIdx() == m_pCRingBuffer->getReadIdx());
    }

    SECTION("RbAccessors")
    {
        m_pCRingBuffer->setReadIdx(rand() % 20);
        m_pCRingBuffer->setWriteIdx(m_pCRingBuffer->getReadIdx());
        CHECK(0 == m_pCRingBuffer->getNumValuesInBuffer());

    }

    SECTION("RbSetReadIdx")
    {
        // put initial values into ring buffer
        int i = 0;
        for (i = 0; i < m_iDelay; i++)
        {
            m_pCRingBuffer->putPostInc(m_pfData[i]);
        }
        m_pCRingBuffer->setReadIdx(i - 1);
        CHECK(m_pfData[i - 1] == m_pCRingBuffer->get());
    }

    SECTION("RbGetSetIdx")
    {
        int test_Value = 10;

        m_pCRingBuffer->setReadIdx(test_Value);
        m_pCRingBuffer->setWriteIdx(test_Value);

        CHECK(test_Value ==m_pCRingBuffer->getReadIdx());
        CHECK(test_Value ==m_pCRingBuffer->getWriteIdx());

        m_pCRingBuffer->put(static_cast<float>(test_Value));
        m_pCRingBuffer->putPostInc(static_cast<float>(test_Value));

        CHECK(static_cast<float>(test_Value) == m_pCRingBuffer->get());
        CHECK(static_cast<float>(test_Value) == m_pCRingBuffer->getPostInc());
        CHECK(static_cast<float>(test_Value) == m_pCRingBuffer->get(-1));
        CHECK(0.F == Approx(m_pCRingBuffer->get()).margin(1e-6F).epsilon(1e-6F));

        //        Edge Cases
        m_pCRingBuffer->setReadIdx(m_iRingBuffLength);
        m_pCRingBuffer->setWriteIdx(m_iRingBuffLength);

        CHECK(0 == m_pCRingBuffer->getReadIdx());
        CHECK(0 == m_pCRingBuffer->getWriteIdx());

        m_pCRingBuffer->setReadIdx(m_iRingBuffLength - 1);
        m_pCRingBuffer->setWriteIdx(m_iRingBuffLength - 1);

        CHECK(m_iRingBuffLength - 1 == m_pCRingBuffer->getReadIdx());
        CHECK(m_iRingBuffLength - 1 == m_pCRingBuffer->getWriteIdx());

        m_pCRingBuffer->put(static_cast<float>(std::numeric_limits<int>::max()));
        m_pCRingBuffer->putPostInc(static_cast<float>(std::numeric_limits<int>::max()));

        CHECK(static_cast<float>(std::numeric_limits<int>::max()) == m_pCRingBuffer->get());
        CHECK(static_cast<float>(std::numeric_limits<int>::max()) == m_pCRingBuffer->getPostInc());

        //        negative indices
        m_pCRingBuffer->setReadIdx(-1);
        m_pCRingBuffer->setWriteIdx(-1);
        CHECK(m_iRingBuffLength - 1 == m_pCRingBuffer->getReadIdx());
        CHECK(m_iRingBuffLength - 1 == m_pCRingBuffer->getWriteIdx());
        m_pCRingBuffer->setReadIdx(-m_iRingBuffLength + 1);
        m_pCRingBuffer->setWriteIdx(-m_iRingBuffLength + 1);
        CHECK(1 == m_pCRingBuffer->getReadIdx());
        CHECK(1 == m_pCRingBuffer->getWriteIdx());

        // extreme indices
        m_pCRingBuffer->setReadIdx(m_iRingBuffLength * 5 - 1);
        m_pCRingBuffer->setWriteIdx(m_iRingBuffLength * 5 - 1);
        CHECK(m_iRingBuffLength - 1 == m_pCRingBuffer->getReadIdx());
        CHECK(m_iRingBuffLength - 1 == m_pCRingBuffer->getWriteIdx());
        m_pCRingBuffer->setReadIdx(-m_iRingBuffLength * 5 - 1);
        m_pCRingBuffer->setWriteIdx(-m_iRingBuffLength * 5 - 1);
        CHECK(m_iRingBuffLength - 1 == m_pCRingBuffer->getReadIdx());
        CHECK(m_iRingBuffLength - 1 == m_pCRingBuffer->getWriteIdx());
    }

    // Simple test to check for overflow
    SECTION("overflowTest")
    {

        for (int i = 0; i < 2 * m_iRingBuffLength; i++)
        {
            m_pCRingBuffer->putPostInc(static_cast<float>(i));
        }

        for (int i = 0; i < m_iRingBuffLength; i++)
        {
            CHECK(i + m_iRingBuffLength == m_pCRingBuffer->getPostInc());
        }
    }

    SECTION("RbReadBlock")
    {
        for (int i = 0; i < 2 * m_iRingBuffLength; i++)
        {
            m_pCRingBuffer->putPostInc(static_cast<float>(i));
        }
        m_pCRingBuffer->setReadIdx(5);
        m_pCRingBuffer->getPostInc(m_pfData, m_iRingBuffLength);

        for (int i = 0; i < m_iRingBuffLength; i++)
        {
            CHECK((i + 5) % m_iRingBuffLength + m_iRingBuffLength == m_pfData[i]);
        }
    }

    SECTION("RbWriteBlock")
    {
        m_pCRingBuffer->putPostInc(m_pfData, 11);
        m_pCRingBuffer->putPostInc(&m_pfData[11], m_iRingBuffLength);
        m_pCRingBuffer->setReadIdx(11);

        for (int i = 11; i < 11 + m_iRingBuffLength; i++)
        {
            CHECK(m_pfData[i] == m_pCRingBuffer->getPostInc());
        }
    }

    SECTION("RbFracDelay")
    {
        for (int i = 0; i < m_iRingBuffLength; i++)
            m_pCRingBuffer->putPostInc(1.F * i);

        float fValue = m_pCRingBuffer->get(.7F);
        CHECK(.7F == Approx(fValue).margin(1e-4F).epsilon(1e-4F));

        fValue = m_pCRingBuffer->get(-.5F);
        CHECK(7.5 == Approx(fValue).margin(1e-4F).epsilon(1e-4F));

        fValue = m_pCRingBuffer->get(-1.8F);
        CHECK(14.2F == Approx(fValue).margin(1e-4F).epsilon(1e-4F));

        m_pCRingBuffer->setReadIdx(1);
        fValue = m_pCRingBuffer->get(-m_iRingBuffLength + 1.F);
        CHECK(2.F == Approx(fValue).margin(1e-4F).epsilon(1e-4F));
    }
    delete[] m_pfData;
    delete m_pCRingBuffer;
}

#endif //WITH_TESTS
