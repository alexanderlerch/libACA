#include "AcaAll.h"

#ifdef WITH_TESTS
#include "catch.hpp"

TEST_CASE("Dtw", "[Dtw]") 
{
    CDtw* pCDtw = new CDtw();

    float** ppfData = 0;
    int** ppiPath = 0;

    int     aiMatrixDimension[2] = { 0, 0 };

    SECTION("api")
    {
        aiMatrixDimension[0] = 5;
        aiMatrixDimension[1] = 4;

        ppfData = new float* [aiMatrixDimension[0]];
        CVector::setZero(ppfData, aiMatrixDimension[0]);

        CHECK(Error_t::kNotInitializedError == pCDtw->compDtw(ppfData));

        CHECK(Error_t::kFunctionInvalidArgsError == pCDtw->init(-1, 4));
        CHECK(Error_t::kFunctionInvalidArgsError == pCDtw->init(3, 0));

        CHECK(Error_t::kNoError == pCDtw->init(aiMatrixDimension[0], aiMatrixDimension[1]));
        CHECK(Error_t::kFunctionInvalidArgsError == pCDtw->compDtw(0));
        CHECK(0 == pCDtw->getPathLength());
    }

    SECTION("ACAExample") 
    {
        int iPathLength = 0;


        int aiPathResultRow[5] = { 0, 1, 2, 3, 4 };
        int aiPathResultCol[5] = { 0, 0, 1, 2, 3 };

        aiMatrixDimension[0] = 5;
        aiMatrixDimension[1] = 4;

        pCDtw->init(aiMatrixDimension[0], aiMatrixDimension[1]);

        ppfData = new float* [aiMatrixDimension[0]];
        for (int i = 0; i < aiMatrixDimension[0]; i++)
            ppfData[i] = new float[aiMatrixDimension[1]];

        ppfData[0][0] = 0; ppfData[0][1] = 1; ppfData[0][2] = 2; ppfData[0][3] = 1;
        ppfData[1][0] = 1; ppfData[1][1] = 2; ppfData[1][2] = 3; ppfData[1][3] = 0;
        ppfData[2][0] = 1; ppfData[2][1] = 0; ppfData[2][2] = 1; ppfData[2][3] = 2;
        ppfData[3][0] = 2; ppfData[3][1] = 1; ppfData[3][2] = 0; ppfData[3][3] = 3;
        ppfData[4][0] = 0; ppfData[4][1] = 1; ppfData[4][2] = 2; ppfData[4][3] = 1;

        pCDtw->compDtw(ppfData);

        iPathLength = pCDtw->getPathLength();
        CHECK(5 == iPathLength);
        ppiPath = new int* [CDtw::kNumMatrixDimensions];
        for (int k = 0; k < CDtw::kNumMatrixDimensions; k++)
            ppiPath[k] = new int[iPathLength];

        CHECK(2.F == pCDtw->getPathCost());

        pCDtw->getPath(ppiPath);

        for (auto n = 0; n < iPathLength; n++)
        {
            CHECK(aiPathResultRow[n] == ppiPath[CDtw::kRow][n]);
            CHECK(aiPathResultCol[n] == ppiPath[CDtw::kCol][n]);
        }
    }

    SECTION("single column")
    {
        int iPathLength = 0;

        aiMatrixDimension[0] = 3;
        aiMatrixDimension[1] = 1;

        ppfData = new float* [aiMatrixDimension[0]];
        for (int i = 0; i < aiMatrixDimension[0]; i++)
            ppfData[i] = new float[aiMatrixDimension[1]];

        ppfData[0][0] = 0.1F;
        ppfData[1][0] = 1;
        ppfData[2][0] = 2;

        pCDtw->init(aiMatrixDimension[0], aiMatrixDimension[1]);
        pCDtw->compDtw(ppfData);

        iPathLength = pCDtw->getPathLength();
        CHECK(aiMatrixDimension[0] == iPathLength);

        CHECK(3.1F == pCDtw->getPathCost());

        ppiPath = new int* [CDtw::kNumMatrixDimensions];
        for (int k = 0; k < CDtw::kNumMatrixDimensions; k++)
            ppiPath[k] = new int[iPathLength];

        pCDtw->getPath(ppiPath);

        for (int i = 0; i < iPathLength; i++)
        {
            CHECK(0 == ppiPath[CDtw::kCol][i]);
            CHECK(i == ppiPath[CDtw::kRow][i]);
        }
    }

    SECTION("single row")
    {
        int iPathLength = 0;

        aiMatrixDimension[0] = 1;
        aiMatrixDimension[1] = 3;

        ppfData = new float* [aiMatrixDimension[0]];
        for (int i = 0; i < aiMatrixDimension[0]; i++)
            ppfData[i] = new float[aiMatrixDimension[1]];

        ppfData[0][0] = 0.1F; ppfData[0][1] = 1; ppfData[0][2] = 2;

        pCDtw->init(aiMatrixDimension[0], aiMatrixDimension[1]);
        pCDtw->compDtw(ppfData);

        iPathLength = pCDtw->getPathLength();
        CHECK(aiMatrixDimension[1] == iPathLength);

        CHECK(3.1F == pCDtw->getPathCost());

        ppiPath = new int* [CDtw::kNumMatrixDimensions];
        for (int k = 0; k < CDtw::kNumMatrixDimensions; k++)
            ppiPath[k] = new int[iPathLength];

        pCDtw->getPath(ppiPath);

        for (int i = 0; i < iPathLength; i++)
        {
            CHECK(i == ppiPath[CDtw::kCol][i]);
            CHECK(0 == ppiPath[CDtw::kRow][i]);
        }
    }

    delete pCDtw;

    for (int i = 0; i < aiMatrixDimension[0]; i++)
        delete[] ppfData[i];
    delete[] ppfData;
    ppfData = 0;

    if (ppiPath)
        for (int k = 0; k < CDtw::kNumMatrixDimensions; k++)
            delete[] ppiPath[k];
    delete[] ppiPath;


}

#endif //WITH_TESTS
