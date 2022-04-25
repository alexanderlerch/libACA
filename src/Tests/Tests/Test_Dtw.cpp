#include "MUSI6106Config.h"

#ifdef WITH_TESTS

#include "Vector.h"
#include "ToolSimpleDtw.h"

#include "gtest/gtest.h"


namespace {
    void CHECK_ARRAY_EQUAL(int *buffer1, int *buffer2, int iLength)
    {
        for (int i = 0; i < iLength; i++)
        {
            EXPECT_EQ(buffer1[i], buffer2[i]);
        }
    }

    class Dtw : public testing::Test
    {
    protected:
        void SetUp() override 
        {
            m_pCDtw = new CDtw();
            m_ppfData = 0;
            m_ppiPath = 0;
        }

        virtual void TearDown()
        {
            for (int i = 0; i < m_aiMatrixDimension[0]; i++)
            {
                delete[] m_ppfData[i];
            }
            delete[] m_ppfData;
            m_ppfData = 0;
            delete m_pCDtw;

            if (m_ppiPath)
                for (int k = 0; k < CDtw::kNumMatrixDimensions; k++)
                    delete[] m_ppiPath[k];
            delete[] m_ppiPath;
        }

        CDtw* m_pCDtw = 0;

        float** m_ppfData = 0;
        int** m_ppiPath = 0;

        int     m_aiMatrixDimension[2] = {0, 0};
    };
}


TEST_F(Dtw, AcaExample)
{
    int iPathLength = 0;


    int aiPathResultRow[5] = { 0, 1, 2, 3, 4 };
    int aiPathResultCol[5] = { 0, 0, 1, 2, 3 };

    m_aiMatrixDimension[0] = 5;
    m_aiMatrixDimension[1] = 4;

    m_pCDtw->init(m_aiMatrixDimension[0], m_aiMatrixDimension[1]);

    m_ppfData = new float* [m_aiMatrixDimension[0]];
    for (int i = 0; i < m_aiMatrixDimension[0]; i++)
        m_ppfData[i] = new float[m_aiMatrixDimension[1]];

    m_ppfData[0][0] = 0; m_ppfData[0][1] = 1; m_ppfData[0][2] = 2; m_ppfData[0][3] = 1;
    m_ppfData[1][0] = 1; m_ppfData[1][1] = 2; m_ppfData[1][2] = 3; m_ppfData[1][3] = 0;
    m_ppfData[2][0] = 1; m_ppfData[2][1] = 0; m_ppfData[2][2] = 1; m_ppfData[2][3] = 2;
    m_ppfData[3][0] = 2; m_ppfData[3][1] = 1; m_ppfData[3][2] = 0; m_ppfData[3][3] = 3;
    m_ppfData[4][0] = 0; m_ppfData[4][1] = 1; m_ppfData[4][2] = 2; m_ppfData[4][3] = 1;

    m_pCDtw->process(m_ppfData);

    iPathLength = m_pCDtw->getPathLength();
    EXPECT_EQ(5, iPathLength);
    m_ppiPath = new int* [CDtw::kNumMatrixDimensions];
    for (int k = 0; k < CDtw::kNumMatrixDimensions; k++)
        m_ppiPath[k] = new int[iPathLength];

    EXPECT_EQ(2.F, m_pCDtw->getPathCost());

    m_pCDtw->getPath(m_ppiPath);

    CHECK_ARRAY_EQUAL(aiPathResultRow, m_ppiPath[CDtw::kRow], iPathLength);
    CHECK_ARRAY_EQUAL(aiPathResultCol, m_ppiPath[CDtw::kCol], iPathLength);
}

TEST_F(Dtw, Api)
{
    m_aiMatrixDimension[0] = 5;
    m_aiMatrixDimension[1] = 4;

    m_ppfData = new float* [m_aiMatrixDimension[0]];
    CVector::setZero(m_ppfData, m_aiMatrixDimension[0]);
    EXPECT_EQ(Error_t::kNotInitializedError, m_pCDtw->process(m_ppfData));

    EXPECT_EQ(Error_t::kFunctionInvalidArgsError, m_pCDtw->init(-1, 4));
    EXPECT_EQ(Error_t::kFunctionInvalidArgsError, m_pCDtw->init(3, 0));

    EXPECT_EQ(Error_t::kNoError, m_pCDtw->init(m_aiMatrixDimension[0], m_aiMatrixDimension[1]));
    EXPECT_EQ(Error_t::kFunctionInvalidArgsError, m_pCDtw->process(0));
    EXPECT_EQ(0, m_pCDtw->getPathLength());
}

TEST_F(Dtw, SingleCol)
{
    int iPathLength = 0;

    m_aiMatrixDimension[0] = 3;
    m_aiMatrixDimension[1] = 1;

    m_ppfData = new float* [m_aiMatrixDimension[0]];
    for (int i = 0; i < m_aiMatrixDimension[0]; i++)
        m_ppfData[i] = new float[m_aiMatrixDimension[1]];

    m_ppfData[0][0] = 0.1F;
    m_ppfData[1][0] = 1;
    m_ppfData[2][0] = 2;

    m_pCDtw->init(m_aiMatrixDimension[0], m_aiMatrixDimension[1]);
    m_pCDtw->process(m_ppfData);

    iPathLength = m_pCDtw->getPathLength();
    EXPECT_EQ(m_aiMatrixDimension[0], iPathLength);

    EXPECT_EQ(3.1F, m_pCDtw->getPathCost());

    m_ppiPath = new int* [CDtw::kNumMatrixDimensions];
    for (int k = 0; k < CDtw::kNumMatrixDimensions; k++)
        m_ppiPath[k] = new int[iPathLength];

    m_pCDtw->getPath(m_ppiPath);

    for (int i = 0; i < iPathLength; i++)
    {
        EXPECT_EQ(0, m_ppiPath[CDtw::kCol][i]);
        EXPECT_EQ(i, m_ppiPath[CDtw::kRow][i]);
    }
}

TEST_F(Dtw, SingleRow)
{
    int iPathLength = 0;

    m_aiMatrixDimension[0] = 1;
    m_aiMatrixDimension[1] = 3;

    m_ppfData = new float* [m_aiMatrixDimension[0]];
    for (int i = 0; i < m_aiMatrixDimension[0]; i++)
        m_ppfData[i] = new float[m_aiMatrixDimension[1]];

    m_ppfData[0][0] = 0.1F; m_ppfData[0][1] = 1; m_ppfData[0][2] = 2;

    m_pCDtw->init(m_aiMatrixDimension[0], m_aiMatrixDimension[1]);
    m_pCDtw->process(m_ppfData);

    iPathLength = m_pCDtw->getPathLength();
    EXPECT_EQ(m_aiMatrixDimension[1], iPathLength);

    EXPECT_EQ(3.1F, m_pCDtw->getPathCost());

    m_ppiPath = new int* [CDtw::kNumMatrixDimensions];
    for (int k = 0; k < CDtw::kNumMatrixDimensions; k++)
        m_ppiPath[k] = new int[iPathLength];

    m_pCDtw->getPath(m_ppiPath);

    for (int i = 0; i < iPathLength; i++)
    {
        EXPECT_EQ(i, m_ppiPath[CDtw::kCol][i]);
        EXPECT_EQ(0, m_ppiPath[CDtw::kRow][i]);
    }
}


#endif //WITH_TESTS
