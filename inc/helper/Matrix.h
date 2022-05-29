#if !defined(__MatrixFloat_hdr__)
#define __MatrixFloat_hdr__

#include <cassert>

#include "Vector.h"

/*! \brief class with static functions for matrix operations with type float (functionality only added when needed)
*/
class CMatrix
{
public:

    static void alloc(float** &ppfMat, int iNumRows, int iNumCols)
    {
        assert(iNumRows > 0);
        assert(iNumCols > 0);

        ppfMat = new float* [iNumRows];
        assert(ppfMat);
 
        for (auto m = 0; m < iNumRows; m++)
        {
            ppfMat[m] = new float[iNumCols];
            assert(ppfMat[m]);

            CVectorFloat::setZero(ppfMat[m], iNumCols);
        }
    }

    static void free(float**& ppfMat, int iNumRows)
    {
        assert(iNumRows > 0);
        if (!ppfMat) return;

        for (auto m = 0; m < iNumRows; m++)
            delete[] ppfMat[m];

        delete[] ppfMat;
        ppfMat = 0;
    }

    static void mulMatColVec(float* pfDestColVec, float** ppfMatrix, const float* pfSrcColVec, int iNumMatRows, int iNumMatCols)
    {
        assert(iNumMatRows > 0);
        assert(iNumMatCols > 0);
        assert(pfDestColVec);
        assert(pfSrcColVec);
        assert(ppfMatrix);
        assert(ppfMatrix[0]);

        for (auto m = 0; m < iNumMatRows; m++)
            pfDestColVec[m] = CVectorFloat::mulScalar(ppfMatrix[m], pfSrcColVec, iNumMatCols);
    }

    static void mulRowVecMat(float* pfDestColVec, const float* pfSrcRowVec, float** ppfMatrix, int iNumMatRows, int iNumMatCols)
    {
        assert(iNumMatRows > 0);
        assert(iNumMatCols > 0);
        assert(pfDestColVec);
        assert(pfSrcRowVec);
        assert(ppfMatrix);
        assert(ppfMatrix[0]);

        CVectorFloat::setZero(pfDestColVec, iNumMatCols);
        for (auto n = 0; n < iNumMatCols; n++)
            for (auto m = 0; m < iNumMatRows; m++)
                pfDestColVec[n] += pfSrcRowVec[m] * ppfMatrix[m][n];
    }

    static void mulMatMat(float** ppfDest, float** ppfSrc1, float** ppfSrc2, int iNum1Rows, int iNum1Cols, int iNum2Rows, int iNum2Cols)
    {
        assert(iNum1Rows > 0);
        assert(iNum1Cols > 0);
        assert(iNum2Rows > 0);
        assert(iNum2Cols > 0);
        assert(iNum1Cols == iNum2Rows);
        assert(ppfDest);
        assert(ppfDest[0]);
        assert(ppfSrc1);
        assert(ppfSrc1[0]);
        assert(ppfSrc2);
        assert(ppfSrc2[0]);

        for (auto m = 0; m < iNum1Rows; m++)
        {
            for (auto n = 0; n < iNum2Cols; n++)
            {
                ppfDest[m][n] = 0.F;
                for (auto k = 0; k < iNum1Cols; k++)
                    ppfDest[m][n] += ppfSrc1[m][k] * ppfSrc2[k][n];
            }
        }
    }

    static void setEye(float** ppfDest, int iNumRows, int iNumCols)
    {
        assert(iNumRows > 0);
        assert(iNumRows == iNumCols);
        assert(ppfDest);
        assert(ppfDest[0]);

        for (auto m = 0; m < iNumRows; m++)
        {
            CVectorFloat::setZero(ppfDest[m], iNumCols);
            ppfDest[m][m] = 1.F;
        }

    }

    static void swapRowCol(float** ppfSrcDest, int iRowIdx, int iColIdx, int iNumRows, int iNumCols)
    {
        assert(iRowIdx > 0);
        assert(iColIdx > 0);
        assert(iNumRows > 0);
        assert(iNumCols > 0);
        assert(ppfSrcDest);
        assert(ppfSrcDest[0]);

        for (auto n = 0; n < iNumCols; n++)
        {
            float fTmp = ppfSrcDest[iRowIdx][n];
            //ppfSrcDest[iRowIdx][n] = ppfSrcDest[n][iColIdx];
            ppfSrcDest[iRowIdx][n] = ppfSrcDest[iColIdx][n];
            ppfSrcDest[iColIdx][n] = fTmp;
        }
    }

    static void copy(float** ppfDest, float** ppfSrc, int iNumRows, int iNumCols)
    {
        assert(iNumRows > 0);
        assert(iNumCols > 0);
        assert(ppfSrc);
        assert(ppfSrc[0]);
        assert(ppfDest);
        assert(ppfDest[0]);

        for (auto m = 0; m < iNumRows; m++)
            CVectorFloat::copy(ppfDest[m], ppfSrc[m], iNumCols);
    }

    static void inv_I (float **ppfSrcDest, int iNumRows, int iNumCols)
    {
        assert(iNumRows > 0);
        assert(iNumRows == iNumCols);
        assert(ppfSrcDest);
        assert(ppfSrcDest[0]);

        const float kSingularityThresh = 1e-15F;
        float** ppfTmp = 0;
        float** ppfEye = 0;
        int    i, j;
        double dDet = 1;

        alloc(ppfTmp, iNumRows, iNumCols);
        copy(ppfTmp, ppfSrcDest, iNumRows, iNumCols);

        alloc(ppfEye, iNumRows, iNumCols);
        setEye(ppfEye, iNumRows, iNumCols);

        if (ppfSrcDest[0][0] == 0)
        {
            i = 1;
            while (i < iNumRows)
            {
                if (ppfSrcDest[i][0] != 0)
                {
                    swapRowCol(ppfSrcDest, 0, i, iNumRows, iNumCols);
                    swapRowCol(ppfEye, 0, i, iNumRows, iNumCols);
                    dDet *= -1;
                    break;
                }
                i++;
            }
        }

        dDet *= ppfSrcDest[0][0];

        CVectorFloat::mulC_I(ppfEye[0], 1.F / ppfSrcDest[0][0], iNumCols);
        CVectorFloat::mulC_I(ppfSrcDest[0], 1.F / ppfSrcDest[0][0], iNumCols);

        for (i = 1; i < iNumRows; i++)
        {
            j = 0;

            if (dDet < kSingularityThresh * kSingularityThresh)
                dDet = 0;

            while (j < i)
            {
                CVectorFloat::subW_I(ppfEye[i], ppfEye[j], ppfSrcDest[i][j], iNumCols);
                CVectorFloat::subW_I(ppfSrcDest[i], ppfSrcDest[j], ppfSrcDest[i][j], iNumCols);
                j++;
            }

            if (ppfSrcDest[i][i] != 0)
            {
                dDet *= ppfSrcDest[i][i];
                CVectorFloat::mulC_I(ppfEye[i], 1.F / ppfSrcDest[i][i], iNumCols);
                CVectorFloat::mulC_I(ppfSrcDest[i], 1.F / ppfSrcDest[i][i], iNumCols);
            }

            if (ppfSrcDest[i][i] == 0)
            {
                for (int j1 = i + 1; j1 < iNumCols; j1++)
                {
                    if (ppfSrcDest[i][j1] != 0)			// Column pivotting not supported
                    {
                        for (int i1 = 0; i1 < iNumRows; i1++)
                        {
                            for (j = 0; j < iNumCols; j++)
                                ppfSrcDest[i1][j] = ppfTmp[i1][j];
                        }
                        inv_I(ppfTmp, iNumRows, iNumCols);
                        copy(ppfSrcDest, ppfTmp, iNumRows, iNumCols);

                        return;
                    }
                }
            }
        }

        for (i = iNumRows - 1; i > 0; i--)
        {
            for (j = i - 1; j >= 0; j--)
            {
                CVectorFloat::subW_I(ppfEye[j], ppfEye[i], ppfSrcDest[j][i], iNumCols);
                CVectorFloat::subW_I(ppfSrcDest[j], ppfSrcDest[i], ppfSrcDest[j][i], iNumCols);
            }
        }

        copy(ppfSrcDest, ppfEye, iNumRows, iNumCols);
    }
};

#endif // __MatrixFloat_hdr__