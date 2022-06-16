#include "AcaAll.h"

#ifdef WITH_TESTS
#include "catch.hpp"

TEST_CASE("RingBuffer", "[RingBuffer]")
{
    const int iRingBuffLength = 16;
    const int iDataLength = 55;
    const int iDelay = 9;

    CRingBuffer<float>* pCRingBuff = 0;

    float* pfData = 0;
    const float fSampleFreq = 44100;

    pCRingBuff = new CRingBuffer<float>(iRingBuffLength);
    pfData = new float[iDataLength];
    CSynthesis::genSine(pfData, 20.F, fSampleFreq, iDataLength, .7F, static_cast<float>(M_PI_2));


SECTION("RbDelay")
    {
        // put initial values into ring buffer
        for (int i = 0; i < iDelay; i++)
        {
            pCRingBuff->putPostInc(pfData[i]);
        }

        for (int i = iDelay; i < iDataLength; i++)
        {
            CHECK(iDelay == pCRingBuff->getNumValuesInBuffer());
            pCRingBuff->getPostInc(); // just to increment - ignore the value
            pCRingBuff->putPostInc(pfData[i]);
        }
    }

    SECTION("RbValues")
    {
        int iDataBuffIdx = 0;

        // put initial values into ring buffer
        for (int i = 0; i < iDelay; i++)
        {
            pCRingBuff->putPostInc(pfData[i]);
        }

        for (int i = iDelay; i < iDataLength; i++, iDataBuffIdx++)
        {
            float fValue = pCRingBuff->getPostInc();
            CHECK(fValue == Approx(pfData[iDataBuffIdx]).margin(1e-3F).epsilon(1e-3F));
            pCRingBuff->putPostInc(pfData[i]);
        }
    }

    SECTION("RbReset")
    {
        // put initial values into ring buffer
        for (int i = 0; i < iDelay; i++)
        {
            pCRingBuff->putPostInc(pfData[i]);
        }
        pCRingBuff->reset();
        CHECK(0 ==pCRingBuff->getNumValuesInBuffer());

        for (int i = 0; i < iRingBuffLength; i++)
        {
            float fValue = pCRingBuff->getPostInc();
            CHECK(0.F == Approx(fValue).margin(1e-6F).epsilon(1e-6F));

            CHECK(iRingBuffLength - (i + 1) == pCRingBuff->getNumValuesInBuffer());
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
        pCRingBuff->setWriteIdx(newIdx);
        pCRingBuff->put(newValue);

        // test reading it
        pCRingBuff->setReadIdx(newIdx);
        CHECK(newValue == pCRingBuff->get());;

        // extra checking for the both index getters
        CHECK(pCRingBuff->getWriteIdx() == pCRingBuff->getReadIdx());
    }

    SECTION("RbAccessors")
    {
        pCRingBuff->setReadIdx(rand() % 20);
        pCRingBuff->setWriteIdx(pCRingBuff->getReadIdx());
        CHECK(0 == pCRingBuff->getNumValuesInBuffer());

    }

    SECTION("RbSetReadIdx")
    {
        // put initial values into ring buffer
        int i = 0;
        for (i = 0; i < iDelay; i++)
        {
            pCRingBuff->putPostInc(pfData[i]);
        }
        pCRingBuff->setReadIdx(i - 1);
        CHECK(pfData[i - 1] == pCRingBuff->get());
    }

    SECTION("RbGetSetIdx")
    {
        int test_Value = 10;

        pCRingBuff->setReadIdx(test_Value);
        pCRingBuff->setWriteIdx(test_Value);

        CHECK(test_Value ==pCRingBuff->getReadIdx());
        CHECK(test_Value ==pCRingBuff->getWriteIdx());

        pCRingBuff->put(static_cast<float>(test_Value));
        pCRingBuff->putPostInc(static_cast<float>(test_Value));

        CHECK(static_cast<float>(test_Value) == pCRingBuff->get());
        CHECK(static_cast<float>(test_Value) == pCRingBuff->getPostInc());
        CHECK(static_cast<float>(test_Value) == pCRingBuff->get(-1));
        CHECK(0.F == Approx(pCRingBuff->get()).margin(1e-6F).epsilon(1e-6F));

        //        Edge Cases
        pCRingBuff->setReadIdx(iRingBuffLength);
        pCRingBuff->setWriteIdx(iRingBuffLength);

        CHECK(0 == pCRingBuff->getReadIdx());
        CHECK(0 == pCRingBuff->getWriteIdx());

        pCRingBuff->setReadIdx(iRingBuffLength - 1);
        pCRingBuff->setWriteIdx(iRingBuffLength - 1);

        CHECK(iRingBuffLength - 1 == pCRingBuff->getReadIdx());
        CHECK(iRingBuffLength - 1 == pCRingBuff->getWriteIdx());

        pCRingBuff->put(static_cast<float>(std::numeric_limits<int>::max()));
        pCRingBuff->putPostInc(static_cast<float>(std::numeric_limits<int>::max()));

        CHECK(static_cast<float>(std::numeric_limits<int>::max()) == pCRingBuff->get());
        CHECK(static_cast<float>(std::numeric_limits<int>::max()) == pCRingBuff->getPostInc());

        //        negative indices
        pCRingBuff->setReadIdx(-1);
        pCRingBuff->setWriteIdx(-1);
        CHECK(iRingBuffLength - 1 == pCRingBuff->getReadIdx());
        CHECK(iRingBuffLength - 1 == pCRingBuff->getWriteIdx());
        pCRingBuff->setReadIdx(-iRingBuffLength + 1);
        pCRingBuff->setWriteIdx(-iRingBuffLength + 1);
        CHECK(1 == pCRingBuff->getReadIdx());
        CHECK(1 == pCRingBuff->getWriteIdx());

        // extreme indices
        pCRingBuff->setReadIdx(iRingBuffLength * 5 - 1);
        pCRingBuff->setWriteIdx(iRingBuffLength * 5 - 1);
        CHECK(iRingBuffLength - 1 == pCRingBuff->getReadIdx());
        CHECK(iRingBuffLength - 1 == pCRingBuff->getWriteIdx());
        pCRingBuff->setReadIdx(-iRingBuffLength * 5 - 1);
        pCRingBuff->setWriteIdx(-iRingBuffLength * 5 - 1);
        CHECK(iRingBuffLength - 1 == pCRingBuff->getReadIdx());
        CHECK(iRingBuffLength - 1 == pCRingBuff->getWriteIdx());
    }

    // Simple test to check for overflow
    SECTION("overflowTest")
    {

        for (int i = 0; i < 2 * iRingBuffLength; i++)
        {
            pCRingBuff->putPostInc(static_cast<float>(i));
        }

        for (int i = 0; i < iRingBuffLength; i++)
        {
            CHECK(i + iRingBuffLength == pCRingBuff->getPostInc());
        }
    }

    SECTION("RbReadBlock")
    {
        for (int i = 0; i < 2 * iRingBuffLength; i++)
        {
            pCRingBuff->putPostInc(static_cast<float>(i));
        }
        pCRingBuff->setReadIdx(5);
        pCRingBuff->getPostInc(pfData, iRingBuffLength);

        for (int i = 0; i < iRingBuffLength; i++)
        {
            CHECK((i + 5) % iRingBuffLength + iRingBuffLength == pfData[i]);
        }
    }

    SECTION("RbWriteBlock")
    {
        pCRingBuff->putPostInc(pfData, 11);
        pCRingBuff->putPostInc(&pfData[11], iRingBuffLength);
        pCRingBuff->setReadIdx(11);

        for (int i = 11; i < 11 + iRingBuffLength; i++)
        {
            CHECK(pfData[i] == pCRingBuff->getPostInc());
        }
    }

    SECTION("RbFracDelay")
    {
        for (int i = 0; i < iRingBuffLength; i++)
            pCRingBuff->putPostInc(1.F * i);

        float fValue = pCRingBuff->get(.7F);
        CHECK(.7F == Approx(fValue).margin(1e-4F).epsilon(1e-4F));

        fValue = pCRingBuff->get(-.5F);
        CHECK(7.5 == Approx(fValue).margin(1e-4F).epsilon(1e-4F));

        fValue = pCRingBuff->get(-1.8F);
        CHECK(14.2F == Approx(fValue).margin(1e-4F).epsilon(1e-4F));

        pCRingBuff->setReadIdx(1);
        fValue = pCRingBuff->get(-iRingBuffLength + 1.F);
        CHECK(2.F == Approx(fValue).margin(1e-4F).epsilon(1e-4F));
    }
    delete[] pfData;
    delete pCRingBuff;
}

#endif //WITH_TESTS
