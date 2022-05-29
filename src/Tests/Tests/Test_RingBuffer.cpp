#include "ACAConfig.h"

#ifdef WITH_TESTS
#include "Synthesis.h"
#include "RingBuffer.h"

#include "catch.hpp"

TEST_CASE("RingBuffer", "[RingBuffer]")
{
    const int iRingBuffLength = 16;
    const int iDataLength = 55;
    const int iDelay = 9;

    CRingBuffer<float>* pCRingBuffer = 0;

    float* pfData = 0;
    const float fSampleFreq = 44100;

    pCRingBuffer = new CRingBuffer<float>(iRingBuffLength);
    pfData = new float[iDataLength];
    CSynthesis::genSine(pfData, 20.F, fSampleFreq, iDataLength, .7F, static_cast<float>(M_PI_2));


SECTION("RbDelay")
    {
        // put initial values into ring buffer
        for (int i = 0; i < iDelay; i++)
        {
            pCRingBuffer->putPostInc(pfData[i]);
        }

        for (int i = iDelay; i < iDataLength; i++)
        {
            CHECK(iDelay == pCRingBuffer->getNumValuesInBuffer());
            pCRingBuffer->getPostInc(); // just to increment - ignore the value
            pCRingBuffer->putPostInc(pfData[i]);
        }
    }

    SECTION("RbValues")
    {
        int iDataBuffIdx = 0;

        // put initial values into ring buffer
        for (int i = 0; i < iDelay; i++)
        {
            pCRingBuffer->putPostInc(pfData[i]);
        }

        for (int i = iDelay; i < iDataLength; i++, iDataBuffIdx++)
        {
            float fValue = pCRingBuffer->getPostInc();
            CHECK(fValue == Approx(pfData[iDataBuffIdx]).margin(1e-3F).epsilon(1e-3F));
            pCRingBuffer->putPostInc(pfData[i]);
        }
    }

    SECTION("RbReset")
    {
        // put initial values into ring buffer
        for (int i = 0; i < iDelay; i++)
        {
            pCRingBuffer->putPostInc(pfData[i]);
        }
        pCRingBuffer->reset();
        CHECK(0 ==pCRingBuffer->getNumValuesInBuffer());

        for (int i = 0; i < iRingBuffLength; i++)
        {
            float fValue = pCRingBuffer->getPostInc();
            CHECK(0.F == Approx(fValue).margin(1e-6F).epsilon(1e-6F));

            CHECK(iRingBuffLength - (i + 1) == pCRingBuffer->getNumValuesInBuffer());
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
        pCRingBuffer->setWriteIdx(newIdx);
        pCRingBuffer->put(newValue);

        // test reading it
        pCRingBuffer->setReadIdx(newIdx);
        CHECK(newValue == pCRingBuffer->get());;

        // extra checking for the both index getters
        CHECK(pCRingBuffer->getWriteIdx() == pCRingBuffer->getReadIdx());
    }

    SECTION("RbAccessors")
    {
        pCRingBuffer->setReadIdx(rand() % 20);
        pCRingBuffer->setWriteIdx(pCRingBuffer->getReadIdx());
        CHECK(0 == pCRingBuffer->getNumValuesInBuffer());

    }

    SECTION("RbSetReadIdx")
    {
        // put initial values into ring buffer
        int i = 0;
        for (i = 0; i < iDelay; i++)
        {
            pCRingBuffer->putPostInc(pfData[i]);
        }
        pCRingBuffer->setReadIdx(i - 1);
        CHECK(pfData[i - 1] == pCRingBuffer->get());
    }

    SECTION("RbGetSetIdx")
    {
        int test_Value = 10;

        pCRingBuffer->setReadIdx(test_Value);
        pCRingBuffer->setWriteIdx(test_Value);

        CHECK(test_Value ==pCRingBuffer->getReadIdx());
        CHECK(test_Value ==pCRingBuffer->getWriteIdx());

        pCRingBuffer->put(static_cast<float>(test_Value));
        pCRingBuffer->putPostInc(static_cast<float>(test_Value));

        CHECK(static_cast<float>(test_Value) == pCRingBuffer->get());
        CHECK(static_cast<float>(test_Value) == pCRingBuffer->getPostInc());
        CHECK(static_cast<float>(test_Value) == pCRingBuffer->get(-1));
        CHECK(0.F == Approx(pCRingBuffer->get()).margin(1e-6F).epsilon(1e-6F));

        //        Edge Cases
        pCRingBuffer->setReadIdx(iRingBuffLength);
        pCRingBuffer->setWriteIdx(iRingBuffLength);

        CHECK(0 == pCRingBuffer->getReadIdx());
        CHECK(0 == pCRingBuffer->getWriteIdx());

        pCRingBuffer->setReadIdx(iRingBuffLength - 1);
        pCRingBuffer->setWriteIdx(iRingBuffLength - 1);

        CHECK(iRingBuffLength - 1 == pCRingBuffer->getReadIdx());
        CHECK(iRingBuffLength - 1 == pCRingBuffer->getWriteIdx());

        pCRingBuffer->put(static_cast<float>(std::numeric_limits<int>::max()));
        pCRingBuffer->putPostInc(static_cast<float>(std::numeric_limits<int>::max()));

        CHECK(static_cast<float>(std::numeric_limits<int>::max()) == pCRingBuffer->get());
        CHECK(static_cast<float>(std::numeric_limits<int>::max()) == pCRingBuffer->getPostInc());

        //        negative indices
        pCRingBuffer->setReadIdx(-1);
        pCRingBuffer->setWriteIdx(-1);
        CHECK(iRingBuffLength - 1 == pCRingBuffer->getReadIdx());
        CHECK(iRingBuffLength - 1 == pCRingBuffer->getWriteIdx());
        pCRingBuffer->setReadIdx(-iRingBuffLength + 1);
        pCRingBuffer->setWriteIdx(-iRingBuffLength + 1);
        CHECK(1 == pCRingBuffer->getReadIdx());
        CHECK(1 == pCRingBuffer->getWriteIdx());

        // extreme indices
        pCRingBuffer->setReadIdx(iRingBuffLength * 5 - 1);
        pCRingBuffer->setWriteIdx(iRingBuffLength * 5 - 1);
        CHECK(iRingBuffLength - 1 == pCRingBuffer->getReadIdx());
        CHECK(iRingBuffLength - 1 == pCRingBuffer->getWriteIdx());
        pCRingBuffer->setReadIdx(-iRingBuffLength * 5 - 1);
        pCRingBuffer->setWriteIdx(-iRingBuffLength * 5 - 1);
        CHECK(iRingBuffLength - 1 == pCRingBuffer->getReadIdx());
        CHECK(iRingBuffLength - 1 == pCRingBuffer->getWriteIdx());
    }

    // Simple test to check for overflow
    SECTION("overflowTest")
    {

        for (int i = 0; i < 2 * iRingBuffLength; i++)
        {
            pCRingBuffer->putPostInc(static_cast<float>(i));
        }

        for (int i = 0; i < iRingBuffLength; i++)
        {
            CHECK(i + iRingBuffLength == pCRingBuffer->getPostInc());
        }
    }

    SECTION("RbReadBlock")
    {
        for (int i = 0; i < 2 * iRingBuffLength; i++)
        {
            pCRingBuffer->putPostInc(static_cast<float>(i));
        }
        pCRingBuffer->setReadIdx(5);
        pCRingBuffer->getPostInc(pfData, iRingBuffLength);

        for (int i = 0; i < iRingBuffLength; i++)
        {
            CHECK((i + 5) % iRingBuffLength + iRingBuffLength == pfData[i]);
        }
    }

    SECTION("RbWriteBlock")
    {
        pCRingBuffer->putPostInc(pfData, 11);
        pCRingBuffer->putPostInc(&pfData[11], iRingBuffLength);
        pCRingBuffer->setReadIdx(11);

        for (int i = 11; i < 11 + iRingBuffLength; i++)
        {
            CHECK(pfData[i] == pCRingBuffer->getPostInc());
        }
    }

    SECTION("RbFracDelay")
    {
        for (int i = 0; i < iRingBuffLength; i++)
            pCRingBuffer->putPostInc(1.F * i);

        float fValue = pCRingBuffer->get(.7F);
        CHECK(.7F == Approx(fValue).margin(1e-4F).epsilon(1e-4F));

        fValue = pCRingBuffer->get(-.5F);
        CHECK(7.5 == Approx(fValue).margin(1e-4F).epsilon(1e-4F));

        fValue = pCRingBuffer->get(-1.8F);
        CHECK(14.2F == Approx(fValue).margin(1e-4F).epsilon(1e-4F));

        pCRingBuffer->setReadIdx(1);
        fValue = pCRingBuffer->get(-iRingBuffLength + 1.F);
        CHECK(2.F == Approx(fValue).margin(1e-4F).epsilon(1e-4F));
    }
    delete[] pfData;
    delete pCRingBuffer;
}

#endif //WITH_TESTS
