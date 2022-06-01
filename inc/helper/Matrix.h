#if !defined(__MatrixFloat_hdr__)
#define __MatrixFloat_hdr__

#include <cassert>

#include "Vector.h"

/*! \brief class with static functions for matrix operations with type float (functionality only added when needed)
*/
class CMatrix
{
public:

    /*! allocates a float matrix and inits it with zeros
    \param ppfMat (empty double pointer, to be allocated)
    \param iNumRows number of rows
    \param iNumCols number of columns
    \return 
    */
    template<typename T>
    static void alloc(T** &ppfMat, int iNumRows, int iNumCols)
    {
        assert(iNumRows > 0);
        assert(iNumCols > 0);

        CVector::alloc(ppfMat, iNumRows);
        assert(ppfMat);
 
        for (auto m = 0; m < iNumRows; m++)
            CVector::alloc(ppfMat[m], iNumCols);
    }

    /*! frees a float matrix 
    \param ppfMat (empty double pointer, to be set to zero)
    \param iNumRows number of rows
    \return
    */
    template<typename T>
    static void free(T**& ppfMat, int iNumRows)
    {
        if (!ppfMat) return;
        assert(iNumRows > 0);

        for (auto m = 0; m < iNumRows; m++)
            CVector::free(ppfMat[m]);

        CVector::free(ppfMat);
        ppfMat = 0;
    }

    /*! multiplies a matrix with a column vector (MAT * VEC)
    \param pfDestColVec resulting (column) vector of length iNumMatRows (to be written, user allocated)
    \param ppfMatrix matrix to be multiplied
    \param pfSrcColVec (column) vector to be multiplied
    \param iNumMatRows number of rows in the matrix
    \param iNumMatCols number of columns in the matrix
    \return
    */
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

    /*! multiplies a row vector with a matrix (VEC * MAT)
    \param pfDestRowVec resulting (row) vector of length iNumMatCols (to be written, user allocated)
    \param pfSrcRowVec (column) vector to be multiplied
    \param ppfMatrix matrix to be multiplied
    \param iNumMatRows number of rows in the matrix
    \param iNumMatCols number of columns in the matrix
    \return
    */
    static void mulRowVecMat(float* pfDestRowVec, const float* pfSrcRowVec, float** ppfMatrix, int iNumMatRows, int iNumMatCols)
    {
        assert(iNumMatRows > 0);
        assert(iNumMatCols > 0);
        assert(pfDestRowVec);
        assert(pfSrcRowVec);
        assert(ppfMatrix);
        assert(ppfMatrix[0]);

        CVectorFloat::setZero(pfDestRowVec, iNumMatCols);
        for (auto n = 0; n < iNumMatCols; n++)
            for (auto m = 0; m < iNumMatRows; m++)
                pfDestRowVec[n] += pfSrcRowVec[m] * ppfMatrix[m][n];
    }

    /*! multiplies a matrix with amatrix (MAT1 * MAT2)
    \param ppfDest resulting matrix of dimension iNum1Rows x iNum2Cols (to be written, user allocated)
    \param ppfSrc1 first matrix to be multiplied
    \param ppfSrc2 second matrix to be multiplied
    \param iNum1Rows number of rows in matrix 1
    \param iNum1Cols number of columns in matrix 1 (has to equal iNum2Rows)
    \param iNum2Rows number of rows in matrix 2 (has to equal iNum1Col)
    \param iNum2Cols number of columns in matrix 2
    \return
    */
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

    /*! fills matrix with ones on the diagonal, zeros elsewhere
    \param ppfDest resulting matrix of dimension iNumRows x iNumCols (to be written, user allocated)
    \param iNumRows number of rows in the matrix
    \param iNumCols number of columns in the matrix
    \return
    */
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


    /*! swaps a matrix row with a column
    \param ppfSrcDest resulting matrix (to be modified)
    \param iRowIdx index of row
    \param iColIdx index of columns
    \param iNumRows number of rows in the matrix
    \param iNumCols number of columns in the matrix
    \return
    */
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

    /*! copies a matrix 
    \param ppfDest resulting matrix(to be written, user allocated)
    \param ppfSrc first matrix to be multiplied
    \param iNumRows number of rows in both matrices
    \param iNumCols number of columns in both matrices
    \return
    */
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

    /*! computes inverse of square matrix
    \param ppfSrcDest input and output matrix (to be modified, user allocated)
    \param iNumRows number of rows in the matrix
    \param iNumCols number of columns in the matrix
    \return
    */
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

            if (dDet < kSingularityThresh *1.* kSingularityThresh)
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

                        CMatrix::free(ppfTmp, iNumRows);
                        CMatrix::free(ppfEye, iNumRows);
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
        CMatrix::free(ppfTmp, iNumRows);
        CMatrix::free(ppfEye, iNumRows);
    }
};

#endif // __MatrixFloat_hdr__