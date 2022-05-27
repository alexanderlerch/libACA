#include "ACAConfig.h"

#ifdef WITH_TESTS

#include "Vector.h"
#include "ToolSimpleDtw.h"

#include "catch.hpp"

TEST_CASE("dtw", "[Dtw]") 
{
    CDtw* m_pCDtw = new CDtw();

    float** m_ppfData = 0;
    int** m_ppiPath = 0;

    int     m_aiMatrixDimension[2] = { 0, 0 };

    SECTION("api")
    {
        m_aiMatrixDimension[0] = 5;
        m_aiMatrixDimension[1] = 4;

        m_ppfData = new float* [m_aiMatrixDimension[0]];
        CVector::setZero(m_ppfData, m_aiMatrixDimension[0]);

        CHECK(Error_t::kNotInitializedError == m_pCDtw->compDtw(m_ppfData));

        CHECK(Error_t::kFunctionInvalidArgsError == m_pCDtw->init(-1, 4));
        CHECK(Error_t::kFunctionInvalidArgsError == m_pCDtw->init(3, 0));

        CHECK(Error_t::kNoError == m_pCDtw->init(m_aiMatrixDimension[0], m_aiMatrixDimension[1]));
        CHECK(Error_t::kFunctionInvalidArgsError == m_pCDtw->compDtw(0));
        CHECK(0 == m_pCDtw->getPathLength());
    }

    SECTION("ACAExample") 
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

        m_pCDtw->compDtw(m_ppfData);

        iPathLength = m_pCDtw->getPathLength();
        CHECK(5 == iPathLength);
        m_ppiPath = new int* [CDtw::kNumMatrixDimensions];
        for (int k = 0; k < CDtw::kNumMatrixDimensions; k++)
            m_ppiPath[k] = new int[iPathLength];

        CHECK(2.F == m_pCDtw->getPathCost());

        m_pCDtw->getPath(m_ppiPath);

        for (auto n = 0; n < iPathLength; n++)
        {
            CHECK(aiPathResultRow[n] == m_ppiPath[CDtw::kRow][n]);
            CHECK(aiPathResultCol[n] == m_ppiPath[CDtw::kCol][n]);
        }
    }

    SECTION("single column")
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
        m_pCDtw->compDtw(m_ppfData);

        iPathLength = m_pCDtw->getPathLength();
        CHECK(m_aiMatrixDimension[0] == iPathLength);

        CHECK(3.1F == m_pCDtw->getPathCost());

        m_ppiPath = new int* [CDtw::kNumMatrixDimensions];
        for (int k = 0; k < CDtw::kNumMatrixDimensions; k++)
            m_ppiPath[k] = new int[iPathLength];

        m_pCDtw->getPath(m_ppiPath);

        for (int i = 0; i < iPathLength; i++)
        {
            CHECK(0 == m_ppiPath[CDtw::kCol][i]);
            CHECK(i == m_ppiPath[CDtw::kRow][i]);
        }
    }

    SECTION("single row")
    {
        int iPathLength = 0;

        m_aiMatrixDimension[0] = 1;
        m_aiMatrixDimension[1] = 3;

        m_ppfData = new float* [m_aiMatrixDimension[0]];
        for (int i = 0; i < m_aiMatrixDimension[0]; i++)
            m_ppfData[i] = new float[m_aiMatrixDimension[1]];

        m_ppfData[0][0] = 0.1F; m_ppfData[0][1] = 1; m_ppfData[0][2] = 2;

        m_pCDtw->init(m_aiMatrixDimension[0], m_aiMatrixDimension[1]);
        m_pCDtw->compDtw(m_ppfData);

        iPathLength = m_pCDtw->getPathLength();
        CHECK(m_aiMatrixDimension[1] == iPathLength);

        CHECK(3.1F == m_pCDtw->getPathCost());

        m_ppiPath = new int* [CDtw::kNumMatrixDimensions];
        for (int k = 0; k < CDtw::kNumMatrixDimensions; k++)
            m_ppiPath[k] = new int[iPathLength];

        m_pCDtw->getPath(m_ppiPath);

        for (int i = 0; i < iPathLength; i++)
        {
            CHECK(i == m_ppiPath[CDtw::kCol][i]);
            CHECK(0 == m_ppiPath[CDtw::kRow][i]);
        }
    }

    delete m_pCDtw;

    for (int i = 0; i < m_aiMatrixDimension[0]; i++)
        delete[] m_ppfData[i];
    delete[] m_ppfData;
    m_ppfData = 0;

    if (m_ppiPath)
        for (int k = 0; k < CDtw::kNumMatrixDimensions; k++)
            delete[] m_ppiPath[k];
    delete[] m_ppiPath;


}

#endif //WITH_TESTS
