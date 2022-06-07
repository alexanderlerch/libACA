#if !defined(__MatrixFloat_hdr__)
#define __MatrixFloat_hdr__

#include <cassert>
#include <algorithm>

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

    /*! sets a float matrix to zero
    \param ppfMat pointer to memory to be set
    \param iNumRows number of rows
    \param iNumCols number of columns
    \return
    */
    template<typename T>
    static void setZero(T** ppfMat, int iNumRows, int iNumCols)
    {
        assert(iNumRows > 0);
        assert(iNumCols > 0);
        assert(ppfMat);
        assert(ppfMat[0]);

        for (auto m = 0; m < iNumRows; m++)
            CVector::setZero(ppfMat[m], iNumCols);
    }

    /*! sets a float matrix to zero
    \param ppfMat pointer to memory to be set
    \param tValue  value to use
    \param iNumRows number of rows
    \param iNumCols number of columns
    \return
    */
    template<typename T>
    static void setValue(T** ppfMat, T tValue, int iNumRows, int iNumCols)
    {
        assert(iNumRows > 0);
        assert(iNumCols > 0);
        assert(ppfMat);
        assert(ppfMat[0]);

        for (auto m = 0; m < iNumRows; m++)
            CVector::setValue(ppfMat[m], tValue, iNumCols);
    }

    /*! sets a float matrix to random
    \param ppfMat pointer to memory to be set
    \param iNumRows number of rows
    \param iNumCols number of columns
    \return
    */
    template<typename T>
    static void setRand(T** ppfMat, int iNumRows, int iNumCols)
    {
        assert(iNumRows > 0);
        assert(iNumCols > 0);
        assert(ppfMat);
        assert(ppfMat[0]);

        for (auto m = 0; m < iNumRows; m++)
            CVector::setRand(ppfMat[m], iNumCols);
    }

    /*! copies vector content to a column
    \param ppfDestMat (destination matrix, user allocated)
    \param pfSrcVec (source vector)
    \param iColIdx index of row to copy to
    \param iNumRows number of columns
    \return
    */
    template<typename T>
    static void setCol(T** ppfDestMat, T* pfSrcVec, int iColIdx, int iNumRows)
    {
        assert(ppfDestMat);
        assert(pfSrcVec);
        assert(iColIdx >= 0);
        assert(iNumRows > 0);

        for (auto m = 0; m < iNumRows; m++)
            ppfDestMat[m][iColIdx] = pfSrcVec[m];
    }

    /*! sets all elements in a matrix below a threshold to zero
    \param ppfMat pointer to memory to be set
    \param iNumRows number of rows
    \param iNumCols number of columns
    \param tThresh threshold
    \return
    */
    template<typename T>
    static void setZeroBelowThresh(T** ppfMat, int iNumRows, int iNumCols, T tThresh)
    {
        assert(iNumRows > 0);
        assert(iNumCols > 0);
        assert(ppfMat);
        assert(ppfMat[0]);

        for (auto m = 0; m < iNumRows; m++)
            CVector::setZeroBelowThresh(ppfMat[m], iNumCols, tThresh);
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

    /*! adds all matrix elements
    \param ppfMat input matrix of dimension iNumRows x iNumCols (to be written, user allocated)
    \param iNumRows number of rows in the matrix
    \param iNumCols number of columns in the matrix
    \param bAbs flag to indicate whether to sum the absolute values
    \return
    */
    static float getSum(float** ppfMat, int iNumRows, int iNumCols, bool bAbs = false)
    {
        assert(iNumRows > 0);
        assert(iNumCols > 0);
        assert(ppfMat);
        assert(ppfMat[0]);

        float fResult = 0;

        for (auto m = 0; m < iNumRows; m++)
            fResult += CVectorFloat::getSum(ppfMat[m], iNumCols, bAbs);

        return fResult;
    }

    /*! return the matrix norm (p=1)
    \param ppfMat input matrix of dimension iNumRows x iNumCols (to be written, user allocated)
    \param iNumRows number of rows in the matrix
    \param iNumCols number of columns in the matrix
    \return
    */
    static float getNorm(float** ppfMat, int iNumRows, int iNumCols)
    {
        assert(iNumRows > 0);
        assert(iNumCols > 0);
        assert(ppfMat);
        assert(ppfMat[0]);

        float fResult = 0;

        for (auto n = 0; n < iNumCols; n++)
        {
            float fTmp = 0;
            for (auto m = 0; m < iNumRows; m++)
                fTmp += std::abs(ppfMat[m][n]);

            if (fTmp > fResult)
                fResult = fTmp;
        }

        return fResult;
    }

    /*! normalizes each column of the matrix (p=1)
    \param ppfSrcDest resulting matrix of dimension iNumRows x iNumCols (to be written, user allocated)
    \param iNumRows number of rows in the matrix
    \param iNumCols number of columns in the matrix
    \return
    */
    static void vecnorm_I(float** ppfSrcDest, int iNumRows, int iNumCols)
    {
        assert(iNumRows > 0);
        assert(iNumCols > 0);
        assert(ppfSrcDest);
        assert(ppfSrcDest[0]);

        for (auto n = 0; n < iNumCols; n++)
        {
            float fNorm = 0;
            for (auto m = 0; m < iNumRows; m++)
                fNorm += std::abs(ppfSrcDest[m][n]);
            if (fNorm > 0)
                for (auto m = 0; m < iNumRows; m++)
                    ppfSrcDest[m][n] /= fNorm;
        }
    }

    /*! comnputes kl divergence between two matrices
    \param ppfSrc1 matrix of dimension iNumRows x iNumCols
    \param ppfSrc2 matrix of dimension iNumRows x iNumCols
    \param iNumRows number of rows in the matrix
    \param iNumCols number of columns in the matrix
    \return
    */
    static float calcKlDivergence(float** ppfSrc1, float** ppfSrc2, int iNumRows, int iNumCols)
    {
        assert(iNumRows > 0);
        assert(iNumCols > 0);
        assert(ppfSrc1);
        assert(ppfSrc1[0]);
        assert(ppfSrc2);
        assert(ppfSrc2[0]);

        float fResult = 0;

        for (int i = 0; i < iNumRows; i++)
        {
            for (int j = 0; j < iNumCols; j++)
                fResult += ppfSrc1[i][j] * (std::log((ppfSrc1[i][j] + 1e-24F) / (ppfSrc2[i][j] + 1e-24F)) - 1.F) + ppfSrc2[i][j];
        }

        return fResult;
    }

    /*! transposes matrix ppfSrc and write the result to ppfDest
    \param ppfDest resulting matrix of dimension iNumSrcCols x iNumSrcRows (to be written, user allocated)
    \param ppfSrc input matrix of dimension iNumSrcRows x iNumCols
    \param iNumSrcRows number of rows in the matrix
    \param iNumSrcCols number of columns in the matrix
    \return
    */
    static void transpose(float** ppfDest, float** ppfSrc, int iNumSrcRows, int iNumSrcCols)
    {
        assert(iNumSrcRows > 0);
        assert(iNumSrcCols > 0);
        assert(ppfSrc);
        assert(ppfSrc[0]);
        assert(ppfDest);
        assert(ppfDest[0]);

        for (auto m = 0; m < iNumSrcRows; m++)
        {
            for (auto n = 0; n < iNumSrcCols; n++)
                ppfDest[n][m] = ppfSrc[m][n];
        }
    }

    /*! rearrange the rows of a matrix according to a row index vector
    \param ppfSrcDest resorted matrix of dimension iNumRows x ?
    \param piRowIndices new indices iNumRows
    \param iNumRows number of rows in the matrix
    \return
    */
    static void rearrangeRows(float** ppfSrcDest, int* piRowIndices, int iNumRows)
    {
        assert(iNumRows > 0);
        assert(piRowIndices);
        assert(ppfSrcDest);
        assert(ppfSrcDest[0]);

        float** ppfTmp = 0;
        CVector::alloc(ppfTmp, iNumRows);

        for (auto m = 0; m < iNumRows; m++)
        {
            assert(piRowIndices[m] < iNumRows);
            assert(piRowIndices[m] >= 0);

            ppfTmp[m] = ppfSrcDest[piRowIndices[m]];
        }
        CVector::copy(ppfSrcDest, ppfTmp, iNumRows);

        CVector::free(ppfTmp);
    }

    /*! returns matrix diagonal as vector
    \param pfDest resulting vector of dimension min(iNumSrcRows, iNumCols) (to be written, user allocated)
    \param ppfSrc input matrix of dimension iNumSrcRows x iNumCols
    \param iNumSrcRows number of rows in the matrix
    \param iNumSrcCols number of columns in the matrix
    \return
    */
    static void diag(float* pfDest, float** ppfSrc, int iNumSrcRows, int iNumSrcCols)
    {
        assert(iNumSrcRows > 0);
        assert(iNumSrcCols > 0);
        assert(ppfSrc);
        assert(ppfSrc[0]);
        assert(pfDest);

        int iLen = std::min(iNumSrcRows, iNumSrcCols);

        for (auto m = 0; m < iLen; m++)
            pfDest[m] = ppfSrc[m][m];
    }

    /*! multiplies a matrix with a column vector (MAT * VEC)
    \param pfDestColVec resulting (column) vector of length iNumMatRows (to be written, user allocated)
    \param ppfMat matrix to be multiplied
    \param pfSrcColVec (column) vector to be multiplied
    \param iNumMatRows number of rows in the matrix
    \param iNumMatCols number of columns in the matrix
    \return
    */
    static void mulMatColVec(float* pfDestColVec, float** ppfMat, const float* pfSrcColVec, int iNumMatRows, int iNumMatCols)
    {
        assert(iNumMatRows > 0);
        assert(iNumMatCols > 0);
        assert(pfDestColVec);
        assert(pfSrcColVec);
        assert(ppfMat);
        assert(ppfMat[0]);

        for (auto m = 0; m < iNumMatRows; m++)
            pfDestColVec[m] = CVectorFloat::mulScalar(ppfMat[m], pfSrcColVec, iNumMatCols);
    }

    /*! multiplies a row vector with a matrix (VEC * MAT)
    \param pfDestRowVec resulting (row) vector of length iNumMatCols (to be written, user allocated)
    \param pfSrcRowVec (column) vector to be multiplied
    \param ppfMat matrix to be multiplied
    \param iNumMatRows number of rows in the matrix
    \param iNumMatCols number of columns in the matrix
    \return
    */
    static void mulRowVecMat(float* pfDestRowVec, const float* pfSrcRowVec, float** ppfMat, int iNumMatRows, int iNumMatCols)
    {
        assert(iNumMatRows > 0);
        assert(iNumMatCols > 0);
        assert(pfDestRowVec);
        assert(pfSrcRowVec);
        assert(ppfMat);
        assert(ppfMat[0]);

        CVectorFloat::setZero(pfDestRowVec, iNumMatCols);
        for (auto n = 0; n < iNumMatCols; n++)
            for (auto m = 0; m < iNumMatRows; m++)
                pfDestRowVec[n] += pfSrcRowVec[m] * ppfMat[m][n];
    }

    /*! multiplies a matrix with a matrix (MAT1 * MAT2)
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

        iNum2Rows = iNum1Cols; // avoid compiler warning

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

    /*! elementwise multiplication of two matrices inplace
    \param ppfSrcDest resulting matrix
    \param ppfSrc matrix to be multiplied
    \param iNumRows number of rows
    \param iNumCols number of columns
    \return
    */
    static void mul_I(float** ppfSrcDest, float** ppfSrc, int iNumRows, int iNumCols)
    {
        assert(iNumRows > 0);
        assert(iNumRows > 0);
        assert(ppfSrcDest);
        assert(ppfSrcDest[0]);
        assert(ppfSrc);
        assert(ppfSrc[0]);

        for (auto m = 0; m < iNumRows; m++)
        {
            for (auto n = 0; n < iNumCols; n++)
                ppfSrcDest[m][n] *= ppfSrc[m][n];
        }
    }

    /*! elementwise addition of two matrices inplace
    \param ppfSrcDest resulting matrix
    \param ppfSrc matrix to be multiplied
    \param iNumRows number of rows
    \param iNumCols number of columns
    \return
    */
    static void add_I(float** ppfSrcDest, float** ppfSrc, int iNumRows, int iNumCols)
    {
        assert(iNumRows > 0);
        assert(iNumRows > 0);
        assert(ppfSrcDest);
        assert(ppfSrcDest[0]);
        assert(ppfSrc);
        assert(ppfSrc[0]);

        for (auto m = 0; m < iNumRows; m++)
        {
            for (auto n = 0; n < iNumCols; n++)
                ppfSrcDest[m][n] += ppfSrc[m][n];
        }
    }

    /*! elementwise subtraction of two matrices inplace
    \param ppfSrcDest resulting matrix
    \param ppfSrc matrix to be multiplied
    \param iNumRows number of rows
    \param iNumCols number of columns
    \return
    */
    static void sub_I(float** ppfSrcDest, float** ppfSrc, int iNumRows, int iNumCols)
    {
        assert(iNumRows > 0);
        assert(iNumRows > 0);
        assert(ppfSrcDest);
        assert(ppfSrcDest[0]);
        assert(ppfSrc);
        assert(ppfSrc[0]);

        for (auto m = 0; m < iNumRows; m++)
        {
            for (auto n = 0; n < iNumCols; n++)
                ppfSrcDest[m][n] -= ppfSrc[m][n];
        }
    }

    /*! elementwise division of two matrices inplace
    \param ppfSrcDest resulting matrix
    \param ppfSrc matrix to be multiplied
    \param iNumRows number of rows
    \param iNumCols number of columns
    \param bAddSmallConst a small floating point number is added before division if true
    \return
    */
    static void div_I(float** ppfSrcDest, float** ppfSrc, int iNumRows, int iNumCols, bool bAddSmallConst = false)
    {
        assert(iNumRows > 0);
        assert(iNumRows > 0);
        assert(ppfSrcDest);
        assert(ppfSrcDest[0]);
        assert(ppfSrc);
        assert(ppfSrc[0]);

        float fEpsilon = bAddSmallConst? 1e-30F : .0F;

        for (auto m = 0; m < iNumRows; m++)
        {
            for (auto n = 0; n < iNumCols; n++)
                ppfSrcDest[m][n] /= (ppfSrc[m][n] + fEpsilon);
        }
    }

    /*! return maximum value in a matrix
    \param ppfMat matrix to analyze
    \param iNumRows number of rows in the matrix
    \param iNumCols number of columns in the matrix
    \return
    */
    static float getMax(float** ppfMat, int iNumRows, int iNumCols)
    {
        assert(iNumRows > 0);
        assert(iNumCols > 0);
        assert(ppfMat);
        assert(ppfMat[0]);

        float fGlobalMax = CVectorFloat::getMax(ppfMat[0], iNumCols);
        for (auto m = 1; m < iNumRows; m++)
        {
            float fMax = CVectorFloat::getMax(ppfMat[m], iNumCols);
            if (fMax > fGlobalMax)
                fGlobalMax = fMax;
        }
        return fGlobalMax;
    }

    /*! adds a single value to all matrix elements
    \param ppfMat matrix to analyze
    \param fAdd scaling factor to apply
    \param iNumRows number of rows in the matrix
    \param iNumCols number of columns in the matrix
    \return
    */
    static void addC_I(float** ppfMat, float fAdd, int iNumRows, int iNumCols)
    {
        assert(iNumRows > 0);
        assert(iNumCols > 0);
        assert(ppfMat);
        assert(ppfMat[0]);

        for (auto m = 0; m < iNumRows; m++)
            CVectorFloat::addC_I(ppfMat[m], fAdd, iNumCols);
    }

    /*! multiplies the whole matrix with a single factor
    \param ppfMat matrix to analyze
    \param fScale scaling factor to apply
    \param iNumRows number of rows in the matrix
    \param iNumCols number of columns in the matrix
    \return
    */
    static void mulC_I(float** ppfMat, float fScale, int iNumRows, int iNumCols)
    {
        assert(iNumRows > 0);
        assert(iNumCols > 0);
        assert(ppfMat);
        assert(ppfMat[0]);

        for (auto m = 0; m < iNumRows; m++)
            CVectorFloat::mulC_I(ppfMat[m], fScale, iNumCols);
    }


    /*! swaps a matrix row with a column in a square matrix
    \param ppfSrcDest resulting matrix (to be modified)
    \param iRowIdx index of row
    \param iColIdx index of columns
    \param iNumCols number of columns in the matrix
    \return
    */
    static void swapRowCol(float** ppfSrcDest, int iRowIdx, int iColIdx, int iNumCols)
    {
        assert(iRowIdx > 0);
        assert(iColIdx > 0);
        assert(iNumCols > 0);
        assert(ppfSrcDest);
        assert(ppfSrcDest[0]);

        for (auto n = 0; n < iNumCols; n++)
        {
            float fTmp = ppfSrcDest[iRowIdx][n];
            ppfSrcDest[iRowIdx][n] = ppfSrcDest[iColIdx][n];
            ppfSrcDest[iColIdx][n] = fTmp;
        }
    }

    /*! copies matrix content to another matrix
    \param ppfDestMat (destination matrix, user allocated)
    \param ppfSrcMat (source matrix)
    \param iNumRows number of rows
    \param iNumCols number of columns
    \return
    */
    template<typename T>
    static void copy(T** ppfDestMat, T** ppfSrcMat, int iNumRows, int iNumCols)
    {
        assert(ppfDestMat);
        assert(ppfSrcMat);
        assert(iNumRows > 0);
        assert(iNumCols > 0);

        for (auto m = 0; m < iNumRows; m++)
            CVector::copy(ppfDestMat[m], ppfSrcMat[m], iNumCols);
    }

    /*! copies vector content to a row
    \param ppfDestMat (destination matrix, user allocated)
    \param pfSrcVec (source vector)
    \param iRowIdx index of row to copy to
    \param iNumCols number of columns
    \return
    */
    template<typename T>
    static void setRow(T** ppfDestMat, T* pfSrcVec, int iRowIdx, int iNumCols)
    {
        assert(ppfDestMat);
        assert(pfSrcVec);
        assert(iRowIdx >= 0);
        assert(iNumCols > 0);

        CVector::copy(ppfDestMat[iRowIdx], pfSrcVec, iNumCols);
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
                    swapRowCol(ppfSrcDest, 0, i, iNumCols);
                    swapRowCol(ppfEye, 0, i, iNumCols);
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