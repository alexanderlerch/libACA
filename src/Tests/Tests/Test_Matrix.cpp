#include "AcaAll.h"

#ifdef WITH_TESTS
#include "catch.hpp"

TEST_CASE("Matrix", "[Matrix]") 
{
    float** ppfMatrix = 0;
    int aiDims[2] = { 0 };

    SECTION("memory") 
    {
        aiDims[0] = 5;
        aiDims[1] = 7;

        CMatrix::alloc(ppfMatrix, aiDims[0], aiDims[1]);

        for (auto m = 0; m < aiDims[0]; m++)
            for (auto n = 0; n < aiDims[1]; n++)
                ppfMatrix[m][n] = 1.F*m + n;

    }

    SECTION("mulmatcol")
    {
        float* pfIn = 0;
        float* pfOut = 0;
        aiDims[0] = 4;
        aiDims[1] = 3;

        CVector::alloc(pfIn, aiDims[1]);
        CVector::alloc(pfOut, aiDims[0]);
        CMatrix::alloc(ppfMatrix, aiDims[0], aiDims[1]);

        for (auto m = 0; m < aiDims[0]; m++)
            for (auto n = 0; n < aiDims[1]; n++)
                ppfMatrix[m][n] = m * 1.F / (n + 1);

        for (auto n = 0; n < aiDims[1]; n++)
            pfIn[n] = 1.F * (n + 1);

        CMatrix::mulMatColvec(pfOut, ppfMatrix, pfIn, aiDims[0], aiDims[1]);

        for (auto m = 0; m < aiDims[0]; m++)
            CHECK(m * aiDims[1] == pfOut[m]);

        CVector::free(pfIn);
        CVector::free(pfOut);
    }

    SECTION("mulrowmat")
    {
        float* pfIn = 0;
        float* pfOut = 0;
        aiDims[0] = 3;
        aiDims[1] = 4;

        CVector::alloc(pfIn, aiDims[0]);
        CVector::alloc(pfOut, aiDims[1]);
        CMatrix::alloc(ppfMatrix, aiDims[0], aiDims[1]);

        for (auto m = 0; m < aiDims[0]; m++)
            for (auto n = 0; n < aiDims[1]; n++)
                ppfMatrix[m][n] = n * 1.F / (m + 1);

        for (auto n = 0; n < aiDims[0]; n++)
            pfIn[n] = 1.F * (n + 1);

        CMatrix::mulRowvecMat(pfOut, pfIn, ppfMatrix, aiDims[0], aiDims[1]);

        for (auto m = 0; m < aiDims[0]; m++)
            CHECK(m * aiDims[0] == pfOut[m]);

        CVector::free(pfIn);
        CVector::free(pfOut);
    }

    SECTION("inv")
    {
        aiDims[0] = 3;
        aiDims[1] = 3;

        float** ppfRes = 0;
        float** ppfIn = 0;

        CMatrix::alloc(ppfIn, aiDims[0], aiDims[1]);
        CMatrix::alloc(ppfRes, aiDims[0], aiDims[1]);
        CMatrix::alloc(ppfMatrix, aiDims[0], aiDims[1]);

        ppfMatrix[0][0] = 1.F; ppfMatrix[0][1] = 0.F; ppfMatrix[0][2] = 2.F;
        ppfMatrix[1][0] = -1.F; ppfMatrix[1][1] = 5.F; ppfMatrix[1][2] = 0.F;
        ppfMatrix[2][0] = 0.F; ppfMatrix[2][1] = 3.F; ppfMatrix[2][2] = -9.F;

        CMatrix::copy(ppfIn, ppfMatrix, aiDims[0], aiDims[1]);
        CMatrix::inv_I(ppfMatrix, aiDims[0], aiDims[1]);


        CMatrix::mulMatMat(ppfRes, ppfIn, ppfMatrix, aiDims[0], aiDims[1], aiDims[0], aiDims[1]);

        for (auto m = 0; m < aiDims[0]; m++)
        {
            CHECK(1.F == Approx(CVector::getSum(ppfRes[m], aiDims[1])).margin(1e-6F).epsilon(1e-6F));
            CHECK(1.F == Approx(ppfRes[m][m]).margin(1e-6F).epsilon(1e-6F));
        }

        ppfMatrix[0][0] = 1.F; ppfMatrix[0][1] = -1.F; ppfMatrix[0][2] = 0.F;
        ppfMatrix[1][0] = 0.F; ppfMatrix[1][1] = 1.F; ppfMatrix[1][2] = -1.F;
        ppfMatrix[2][0] = 0.F; ppfMatrix[2][1] = 0.F; ppfMatrix[2][2] = 1.F;
        CMatrix::inv_I(ppfMatrix, aiDims[0], aiDims[1]);

        CHECK(1.F == Approx(ppfMatrix[0][0]).margin(1e-6F).epsilon(1e-6F));
        CHECK(1.F == Approx(ppfMatrix[0][1]).margin(1e-6F).epsilon(1e-6F));
        CHECK(1.F == Approx(ppfMatrix[0][2]).margin(1e-6F).epsilon(1e-6F));
        CHECK(1.F == Approx(ppfMatrix[1][1]).margin(1e-6F).epsilon(1e-6F));
        CHECK(1.F == Approx(ppfMatrix[1][2]).margin(1e-6F).epsilon(1e-6F));
        CHECK(1.F == Approx(ppfMatrix[2][2]).margin(1e-6F).epsilon(1e-6F));
        CHECK(0.F == Approx(ppfMatrix[1][0]).margin(1e-6F).epsilon(1e-6F));
        CHECK(0.F == Approx(ppfMatrix[2][0]).margin(1e-6F).epsilon(1e-6F));
        CHECK(0.F == Approx(ppfMatrix[2][1]).margin(1e-6F).epsilon(1e-6F));

        CMatrix::free(ppfIn, aiDims[0]);
        CMatrix::free(ppfRes, aiDims[0]);
    }

    SECTION("det")
    {
        aiDims[0] = 3;
        aiDims[1] = 3;

        CMatrix::alloc(ppfMatrix, aiDims[0], aiDims[1]);

        ppfMatrix[0][0] = 1.F; ppfMatrix[0][1] = 2.F;
        ppfMatrix[1][0] = 3.F; ppfMatrix[1][1] = 4.F;
        CHECK(-2.F == CMatrix::det(ppfMatrix, 2, 2));

        ppfMatrix[0][0] = -2.F; ppfMatrix[0][1] = 2.F;
        ppfMatrix[1][0] = -6.F; ppfMatrix[1][1] = 4.F;
        CHECK(4.F == CMatrix::det(ppfMatrix, 2, 2));

        ppfMatrix[0][0] = 1.F; ppfMatrix[0][1] = 0.F;
        ppfMatrix[1][0] = 0.F; ppfMatrix[1][1] = 1.F;
        CHECK(1 == CMatrix::det(ppfMatrix, 2, 2));

        ppfMatrix[0][0] = 1.F; ppfMatrix[0][1] = -2.F; ppfMatrix[0][2] = 4.F;
        ppfMatrix[1][0] = -5.F; ppfMatrix[1][1] = 2.F; ppfMatrix[1][2] = 0.F;
        ppfMatrix[2][0] = 1.F; ppfMatrix[2][1] = 0.F; ppfMatrix[2][2] = 3.F;
        CHECK(-32.F == CMatrix::det(ppfMatrix, aiDims[0], aiDims[1]));

        ppfMatrix[0][0] = 1.F; ppfMatrix[0][1] = -2.F; ppfMatrix[0][2] = 4.F;
        ppfMatrix[1][0] = -5.F; ppfMatrix[1][1] = 2.F; ppfMatrix[1][2] = 0.F;
        ppfMatrix[2][0] = -5.F; ppfMatrix[2][1] = 2.F; ppfMatrix[2][2] = 0.F;
        CHECK(0.F == CMatrix::det(ppfMatrix, aiDims[0], aiDims[1]));
    }

    CMatrix::free(ppfMatrix, aiDims[0]);
}

TEST_CASE("MatrixVector", "[Vector]")
{
    float* pfVec1 = 0,
        * pfVec2 = 0;
    int iLength = 3;

    CVector::alloc(pfVec1, iLength);
    CVector::alloc(pfVec2, iLength);

    SECTION("Euc")
    {
        // zero check
        CHECK(0 == CVector::distEuclidean(pfVec1, pfVec2, iLength));

        pfVec1[0] = 3.F;
        pfVec1[1] = 0.F;
        pfVec1[2] = 4.F;

        pfVec2[0] = 4.F;
        pfVec2[1] = -2.F;
        pfVec2[2] = 2.F;

        CHECK(3.F == CVector::distEuclidean(pfVec1, pfVec2, iLength));

    }

    SECTION("sort")
    {
        int* piIndices = 0;

        CVector::alloc(piIndices, iLength);

        pfVec1[0] = 1.F;
        pfVec1[1] = 2.F;
        pfVec1[2] = 3.F;

        pfVec2[0] = -4.F;
        pfVec2[1] = .1F;
        pfVec2[2] = 0.F;

        CVector::sort_I(pfVec1, 0, iLength, true);
        for (auto i = 1; i < iLength; i++)
            CHECK(pfVec1[i - 1] < pfVec1[i]);

        CVector::sort_I(pfVec1, 0, iLength, false);
        for (auto i = 1; i < iLength; i++)
            CHECK(pfVec1[i - 1] > pfVec1[i]);

        CVector::sort_I(pfVec2, piIndices, iLength, true);
        CHECK(0 == piIndices[0]);
        CHECK(2 == piIndices[1]);
        CHECK(1 == piIndices[2]);

        CVector::sort_I(pfVec2, piIndices, iLength, true);
        CHECK(0 == piIndices[0]);
        CHECK(1 == piIndices[1]);
        CHECK(2 == piIndices[2]);

        CVector::free(piIndices);
    }

    CVector::free(pfVec1);
    CVector::free(pfVec2);
}
#endif //WITH_TESTS
