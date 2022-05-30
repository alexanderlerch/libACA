#include "ACAConfig.h"

#ifdef WITH_TESTS

#include "Matrix.h"

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

        CVectorFloat::alloc(pfIn, aiDims[1]);
        CVectorFloat::alloc(pfOut, aiDims[0]);
        CMatrix::alloc(ppfMatrix, aiDims[0], aiDims[1]);

        for (auto m = 0; m < aiDims[0]; m++)
            for (auto n = 0; n < aiDims[1]; n++)
                ppfMatrix[m][n] = m * 1.F / (n + 1);

        for (auto n = 0; n < aiDims[1]; n++)
            pfIn[n] = 1.F * (n + 1);

        CMatrix::mulMatColVec(pfOut, ppfMatrix, pfIn, aiDims[0], aiDims[1]);

        for (auto m = 0; m < aiDims[0]; m++)
            CHECK(m * aiDims[1] == pfOut[m]);

        CVectorFloat::free(pfIn);
        CVectorFloat::free(pfOut);
    }

    SECTION("mulrowmat")
    {
        float* pfIn = 0;
        float* pfOut = 0;
        aiDims[0] = 3;
        aiDims[1] = 4;

        CVectorFloat::alloc(pfIn, aiDims[0]);
        CVectorFloat::alloc(pfOut, aiDims[1]);
        CMatrix::alloc(ppfMatrix, aiDims[0], aiDims[1]);

        for (auto m = 0; m < aiDims[0]; m++)
            for (auto n = 0; n < aiDims[1]; n++)
                ppfMatrix[m][n] = n * 1.F / (m + 1);

        for (auto n = 0; n < aiDims[0]; n++)
            pfIn[n] = 1.F * (n + 1);

        CMatrix::mulRowVecMat(pfOut, pfIn, ppfMatrix, aiDims[0], aiDims[1]);

        for (auto m = 0; m < aiDims[0]; m++)
            CHECK(m * aiDims[0] == pfOut[m]);

        CVectorFloat::free(pfIn);
        CVectorFloat::free(pfOut);
    }

    SECTION("inv")
    {
        aiDims[0] = 3;
        aiDims[1] = 3;

        float** ppfResult = 0;
        float** ppfInput = 0;

        CMatrix::alloc(ppfInput, aiDims[0], aiDims[1]);
        CMatrix::alloc(ppfResult, aiDims[0], aiDims[1]);
        CMatrix::alloc(ppfMatrix, aiDims[0], aiDims[1]);

        ppfMatrix[0][0] = 1.F; ppfMatrix[0][1] = 0.F; ppfMatrix[0][2] = 2.F;
        ppfMatrix[1][0] = -1.F; ppfMatrix[1][1] = 5.F; ppfMatrix[1][2] = 0.F;
        ppfMatrix[2][0] = 0.F; ppfMatrix[2][1] = 3.F; ppfMatrix[2][2] = -9.F;

        CMatrix::copy(ppfInput, ppfMatrix, aiDims[0], aiDims[1]);
        CMatrix::inv_I(ppfMatrix, aiDims[0], aiDims[1]);


        CMatrix::mulMatMat(ppfResult, ppfInput, ppfMatrix, aiDims[0], aiDims[1], aiDims[0], aiDims[1]);

        for (auto m = 0; m < aiDims[0]; m++)
        {
            CHECK(1.F == Approx(CVectorFloat::getSum(ppfResult[m], aiDims[1])).margin(1e-6F).epsilon(1e-6F));
            CHECK(1.F == Approx(ppfResult[m][m]).margin(1e-6F).epsilon(1e-6F));
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

        CMatrix::free(ppfInput, aiDims[0]);
        CMatrix::free(ppfResult, aiDims[0]);
    }

    CMatrix::free(ppfMatrix, aiDims[0]);
}

#endif //WITH_TESTS
