#if !defined(__ACA_Matrix_HEADER_INCLUDED__)
#define __ACA_Matrix_HEADER_INCLUDED__

#include <cassert>
#include <algorithm>

#include "Vector.h"

/*! \brief class with static functions for matrix operations with type float (functionality only added when needed)
*/
class CMatrix
{
public:

    /*! allocates a float matrix and inits it with zeros
    \param pptMat (empty double pointer, to be allocated)
    \param iNumRows number of rows
    \param iNumCols number of columns
    \return 
    */
    template<typename T>
    static void alloc(T** &pptMat, int iNumRows, int iNumCols)
    {
        assert(iNumRows > 0);
        assert(iNumCols > 0);

        CVector::alloc(pptMat, iNumRows);
        assert(pptMat);
 
        for (auto m = 0; m < iNumRows; m++)
            CVector::alloc(pptMat[m], iNumCols);
    }

    /*! frees a float matrix 
    \param pptMat (empty double pointer, to be set to zero)
    \param iNumRows number of rows
    \return
    */
    template<typename T>
    static void free(T**& pptMat, int iNumRows)
    {
        if (!pptMat) return;
        assert(iNumRows > 0);

        for (auto m = 0; m < iNumRows; m++)
            CVector::free(pptMat[m]);

        CVector::free(pptMat);
        pptMat = 0;
    }

    /*! sets a float matrix to zero
    \param pptMat pointer to memory to be set
    \param iNumRows number of rows
    \param iNumCols number of columns
    \return
    */
    template<typename T>
    static void setZero(T** pptMat, int iNumRows, int iNumCols)
    {
        assert(iNumRows > 0);
        assert(iNumCols > 0);
        assert(pptMat);
        assert(pptMat[0]);

        for (auto m = 0; m < iNumRows; m++)
            CVector::setZero(pptMat[m], iNumCols);
    }

    /*! sets a float matrix to zero
    \param pptMat pointer to memory to be set
    \param tValue  value to use
    \param iNumRows number of rows
    \param iNumCols number of columns
    \return
    */
    template<typename T>
    static void setValue(T** pptMat, T tValue, int iNumRows, int iNumCols)
    {
        assert(iNumRows > 0);
        assert(iNumCols > 0);
        assert(pptMat);
        assert(pptMat[0]);

        for (auto m = 0; m < iNumRows; m++)
            CVector::setValue(pptMat[m], tValue, iNumCols);
    }

    /*! sets a float matrix to random
    \param pptMat pointer to memory to be set
    \param iNumRows number of rows
    \param iNumCols number of columns
    \return
    */
    template<typename T>
    static void setRand(T** pptMat, int iNumRows, int iNumCols)
    {
        assert(iNumRows > 0);
        assert(iNumCols > 0);
        assert(pptMat);
        assert(pptMat[0]);

        for (auto m = 0; m < iNumRows; m++)
            CVector::setRand(pptMat[m], iNumCols);
    }

    /*! copies vector content to a column
    \param pptDestMat (destination matrix, user allocated)
    \param ptSrcVec (source vector)
    \param iColIdx index of row to copy to
    \param iNumRows number of columns
    \return
    */
    template<typename T>
    static void setCol(T** pptDestMat, T* ptSrcVec, int iColIdx, int iNumRows)
    {
        assert(pptDestMat);
        assert(ptSrcVec);
        assert(iColIdx >= 0);
        assert(iNumRows > 0);

        for (auto m = 0; m < iNumRows; m++)
            pptDestMat[m][iColIdx] = ptSrcVec[m];
    }

    /*! sets all elements in a matrix below a threshold to zero
    \param pptMat pointer to memory to be set
    \param iNumRows number of rows
    \param iNumCols number of columns
    \param tThresh threshold
    \return
    */
    template<typename T>
    static void setZeroBelowThresh(T** pptMat, int iNumRows, int iNumCols, T tThresh)
    {
        assert(iNumRows > 0);
        assert(iNumCols > 0);
        assert(pptMat);
        assert(pptMat[0]);

        for (auto m = 0; m < iNumRows; m++)
            CVector::setZeroBelowThresh(pptMat[m], iNumCols, tThresh);
    }

    /*! fills matrix with ones on the diagonal, zeros elsewhere
    \param pptDest resulting matrix of dimension iNumRows x iNumCols (to be written, user allocated)
    \param iNumRows number of rows in the matrix
    \param iNumCols number of columns in the matrix
    \return
    */
    static void setEye(float** pptDest, int iNumRows, int iNumCols)
    {
        assert(iNumRows > 0);
        assert(iNumRows == iNumCols);
        assert(pptDest);
        assert(pptDest[0]);

        for (auto m = 0; m < iNumRows; m++)
        {
            CVector::setZero(pptDest[m], iNumCols);
            pptDest[m][m] = 1.F;
        }
    }

    /*! returns a column as vector
    \param ptDest resulting vector of dimension iNumRows (to be written, user allocated)
    \param pptMat input matrix of dimension iNumRows x iNumCols (to be written, user allocated)
    \param iColIdx index of column to retrieve
    \param iNumRows number of rows in the matrix
    \param iNumCols number of columns in the matrix
    */
    static void getCol(float *ptDest, float** pptMat, int iColIdx, int iNumRows, int iNumCols)
    {
        assert(iColIdx >= 0 && iColIdx < iNumCols);
        assert(iNumRows > 0);
        assert(iNumCols > 0);
        assert(pptMat);
        assert(ptDest);
        assert(pptMat[0]);

        for (auto m = 0; m < iNumRows; m++)
            ptDest[m] = pptMat[m][iColIdx];

        iColIdx = iNumCols; // trying to avoid compiler warnings
    }

    /*! adds all matrix elements
    \param pptMat input matrix of dimension iNumRows x iNumCols (to be written, user allocated)
    \param iNumRows number of rows in the matrix
    \param iNumCols number of columns in the matrix
    \param bAbs flag to indicate whether to sum the absolute values
    \return
    */
    static float getSum(float** pptMat, int iNumRows, int iNumCols, bool bAbs = false)
    {
        assert(iNumRows > 0);
        assert(iNumCols > 0);
        assert(pptMat);
        assert(pptMat[0]);

        float fResult = 0;

        for (auto m = 0; m < iNumRows; m++)
            fResult += CVector::getSum(pptMat[m], iNumCols, bAbs);

        return fResult;
    }

    /*! return the matrix norm (p=1)
    \param pptMat input matrix of dimension iNumRows x iNumCols (to be written, user allocated)
    \param iNumRows number of rows in the matrix
    \param iNumCols number of columns in the matrix
    \return
    */
    static float getNorm(float** pptMat, int iNumRows, int iNumCols)
    {
        assert(iNumRows > 0);
        assert(iNumCols > 0);
        assert(pptMat);
        assert(pptMat[0]);

        float fResult = 0;

        for (auto n = 0; n < iNumCols; n++)
        {
            float fTmp = 0;
            for (auto m = 0; m < iNumRows; m++)
                fTmp += std::abs(pptMat[m][n]);

            if (fTmp > fResult)
                fResult = fTmp;
        }

        return fResult;
    }

    /*! normalizes each column of the matrix (p=1)
    \param pptSrcDest resulting matrix of dimension iNumRows x iNumCols (to be written, user allocated)
    \param iNumRows number of rows in the matrix
    \param iNumCols number of columns in the matrix
    \return
    */
    static void vecnorm_I(float** pptSrcDest, int iNumRows, int iNumCols)
    {
        assert(iNumRows > 0);
        assert(iNumCols > 0);
        assert(pptSrcDest);
        assert(pptSrcDest[0]);

        for (auto n = 0; n < iNumCols; n++)
        {
            float fNorm = 0;
            for (auto m = 0; m < iNumRows; m++)
                fNorm += std::abs(pptSrcDest[m][n]);
            if (fNorm > 0)
                for (auto m = 0; m < iNumRows; m++)
                    pptSrcDest[m][n] /= fNorm;
        }
    }

    /*! comnputes kl divergence between two matrices
    \param pptSrc1 matrix of dimension iNumRows x iNumCols
    \param pptSrc2 matrix of dimension iNumRows x iNumCols
    \param iNumRows number of rows in the matrix
    \param iNumCols number of columns in the matrix
    \return
    */
    static float calcKlDivergence(float** pptSrc1, float** pptSrc2, int iNumRows, int iNumCols)
    {
        assert(iNumRows > 0);
        assert(iNumCols > 0);
        assert(pptSrc1);
        assert(pptSrc1[0]);
        assert(pptSrc2);
        assert(pptSrc2[0]);

        float fResult = 0;

        for (int i = 0; i < iNumRows; i++)
        {
            for (int j = 0; j < iNumCols; j++)
                fResult += pptSrc1[i][j] * (std::log((pptSrc1[i][j] + 1e-24F) / (pptSrc2[i][j] + 1e-24F)) - 1.F) + pptSrc2[i][j];
        }

        return fResult;
    }

    /*! transposes matrix pptSrc and write the result to pptDest
    \param pptDest resulting matrix of dimension iNumSrcCols x iNumSrcRows (to be written, user allocated)
    \param pptSrc input matrix of dimension iNumSrcRows x iNumCols
    \param iNumSrcRows number of rows in the matrix
    \param iNumSrcCols number of columns in the matrix
    \return
    */
    static void transpose(float** pptDest, float** pptSrc, int iNumSrcRows, int iNumSrcCols)
    {
        assert(iNumSrcRows > 0);
        assert(iNumSrcCols > 0);
        assert(pptSrc);
        assert(pptSrc[0]);
        assert(pptDest);
        assert(pptDest[0]);

        for (auto m = 0; m < iNumSrcRows; m++)
        {
            for (auto n = 0; n < iNumSrcCols; n++)
                pptDest[n][m] = pptSrc[m][n];
        }
    }

    /*! rearrange the rows of a matrix according to a row index vector
    \param pptSrcDest resorted matrix of dimension iNumRows x ?
    \param piRowIndices new indices iNumRows
    \param iNumRows number of rows in the matrix
    \return
    */
    static void rearrangeRows(float** pptSrcDest, int* piRowIndices, int iNumRows)
    {
        assert(iNumRows > 0);
        assert(piRowIndices);
        assert(pptSrcDest);
        assert(pptSrcDest[0]);

        float** ppfTmp = 0;
        CVector::alloc(ppfTmp, iNumRows);

        for (auto m = 0; m < iNumRows; m++)
        {
            assert(piRowIndices[m] < iNumRows);
            assert(piRowIndices[m] >= 0);

            ppfTmp[m] = pptSrcDest[piRowIndices[m]];
        }
        CVector::copy(pptSrcDest, ppfTmp, iNumRows);

        CVector::free(ppfTmp);
    }

    /*! returns matrix diagonal as vector
    \param ptDest resulting vector of dimension min(iNumSrcRows, iNumCols) (to be written, user allocated)
    \param pptSrc input matrix of dimension iNumSrcRows x iNumCols
    \param iNumSrcRows number of rows in the matrix
    \param iNumSrcCols number of columns in the matrix
    \return
    */
    static void diag(float* ptDest, float** pptSrc, int iNumSrcRows, int iNumSrcCols)
    {
        assert(iNumSrcRows > 0);
        assert(iNumSrcCols > 0);
        assert(pptSrc);
        assert(pptSrc[0]);
        assert(ptDest);

        int iLen = std::min(iNumSrcRows, iNumSrcCols);

        for (auto m = 0; m < iLen; m++)
            ptDest[m] = pptSrc[m][m];
    }

    /*! multiplies a matrix with a column vector (MAT * VEC)
    \param ptDestColVec resulting (column) vector of length iNumMatRows (to be written, user allocated)
    \param pptMat matrix to be multiplied
    \param ptSrcColVec (column) vector to be multiplied
    \param iNumMatRows number of rows in the matrix
    \param iNumMatCols number of columns in the matrix
    \return
    */
    static void mulMatColVec(float* ptDestColVec, float** pptMat, const float* ptSrcColVec, int iNumMatRows, int iNumMatCols)
    {
        assert(iNumMatRows > 0);
        assert(iNumMatCols > 0);
        assert(ptDestColVec);
        assert(ptSrcColVec);
        assert(pptMat);
        assert(pptMat[0]);

        for (auto m = 0; m < iNumMatRows; m++)
            ptDestColVec[m] = CVector::mulScalar(pptMat[m], ptSrcColVec, iNumMatCols);
    }

    /*! multiplies a row vector with a matrix (VEC * MAT)
    \param ptDestRowVec resulting (row) vector of length iNumMatCols (to be written, user allocated)
    \param ptSrcRowVec (column) vector to be multiplied
    \param pptMat matrix to be multiplied
    \param iNumMatRows number of rows in the matrix
    \param iNumMatCols number of columns in the matrix
    \return
    */
    static void mulRowVecMat(float* ptDestRowVec, const float* ptSrcRowVec, float** pptMat, int iNumMatRows, int iNumMatCols)
    {
        assert(iNumMatRows > 0);
        assert(iNumMatCols > 0);
        assert(ptDestRowVec);
        assert(ptSrcRowVec);
        assert(pptMat);
        assert(pptMat[0]);

        CVector::setZero(ptDestRowVec, iNumMatCols);
        for (auto n = 0; n < iNumMatCols; n++)
            for (auto m = 0; m < iNumMatRows; m++)
                ptDestRowVec[n] += ptSrcRowVec[m] * pptMat[m][n];
    }

    /*! multiplies a matrix with a matrix (MAT1 * MAT2)
    \param pptDest resulting matrix of dimension iNum1Rows x iNum2Cols (to be written, user allocated)
    \param pptSrc1 first matrix to be multiplied
    \param pptSrc2 second matrix to be multiplied
    \param iNum1Rows number of rows in matrix 1
    \param iNum1Cols number of columns in matrix 1 (has to equal iNum2Rows)
    \param iNum2Rows number of rows in matrix 2 (has to equal iNum1Col)
    \param iNum2Cols number of columns in matrix 2
    \return
    */
    static void mulMatMat(float** pptDest, float** pptSrc1, float** pptSrc2, int iNum1Rows, int iNum1Cols, int iNum2Rows, int iNum2Cols)
    {
        assert(iNum1Rows > 0);
        assert(iNum1Cols > 0);
        assert(iNum2Rows > 0);
        assert(iNum2Cols > 0);
        assert(iNum1Cols == iNum2Rows);
        assert(pptDest);
        assert(pptDest[0]);
        assert(pptSrc1);
        assert(pptSrc1[0]);
        assert(pptSrc2);
        assert(pptSrc2[0]);

        iNum2Rows = iNum1Cols; // avoid compiler warning

        for (auto m = 0; m < iNum1Rows; m++)
        {
            for (auto n = 0; n < iNum2Cols; n++)
            {
                pptDest[m][n] = 0.F;
                for (auto k = 0; k < iNum1Cols; k++)
                    pptDest[m][n] += pptSrc1[m][k] * pptSrc2[k][n];
            }
        }
    }

    /*! elementwise multiplication of two matrices inplace
    \param pptSrcDest resulting matrix
    \param pptSrc matrix to be multiplied
    \param iNumRows number of rows
    \param iNumCols number of columns
    \return
    */
    static void mul_I(float** pptSrcDest, float** pptSrc, int iNumRows, int iNumCols)
    {
        assert(iNumRows > 0);
        assert(iNumRows > 0);
        assert(pptSrcDest);
        assert(pptSrcDest[0]);
        assert(pptSrc);
        assert(pptSrc[0]);

        for (auto m = 0; m < iNumRows; m++)
        {
            for (auto n = 0; n < iNumCols; n++)
                pptSrcDest[m][n] *= pptSrc[m][n];
        }
    }

    /*! elementwise addition of two matrices inplace
    \param pptSrcDest resulting matrix
    \param pptSrc matrix to be multiplied
    \param iNumRows number of rows
    \param iNumCols number of columns
    \return
    */
    static void add_I(float** pptSrcDest, float** pptSrc, int iNumRows, int iNumCols)
    {
        assert(iNumRows > 0);
        assert(iNumRows > 0);
        assert(pptSrcDest);
        assert(pptSrcDest[0]);
        assert(pptSrc);
        assert(pptSrc[0]);

        for (auto m = 0; m < iNumRows; m++)
        {
            for (auto n = 0; n < iNumCols; n++)
                pptSrcDest[m][n] += pptSrc[m][n];
        }
    }

    /*! elementwise subtraction of two matrices inplace
    \param pptSrcDest resulting matrix
    \param pptSrc matrix to be multiplied
    \param iNumRows number of rows
    \param iNumCols number of columns
    \return
    */
    static void sub_I(float** pptSrcDest, float** pptSrc, int iNumRows, int iNumCols)
    {
        assert(iNumRows > 0);
        assert(iNumRows > 0);
        assert(pptSrcDest);
        assert(pptSrcDest[0]);
        assert(pptSrc);
        assert(pptSrc[0]);

        for (auto m = 0; m < iNumRows; m++)
        {
            for (auto n = 0; n < iNumCols; n++)
                pptSrcDest[m][n] -= pptSrc[m][n];
        }
    }

    /*! elementwise division of two matrices inplace
    \param pptSrcDest resulting matrix
    \param pptSrc matrix to be multiplied
    \param iNumRows number of rows
    \param iNumCols number of columns
    \param bAddSmallConst a small floating point number is added before division if true
    \return
    */
    static void div_I(float** pptSrcDest, float** pptSrc, int iNumRows, int iNumCols, bool bAddSmallConst = false)
    {
        assert(iNumRows > 0);
        assert(iNumRows > 0);
        assert(pptSrcDest);
        assert(pptSrcDest[0]);
        assert(pptSrc);
        assert(pptSrc[0]);

        float fEpsilon = bAddSmallConst? 1e-30F : .0F;

        for (auto m = 0; m < iNumRows; m++)
        {
            for (auto n = 0; n < iNumCols; n++)
                pptSrcDest[m][n] /= (pptSrc[m][n] + fEpsilon);
        }
    }

    /*! return maximum value in a matrix
    \param pptMat matrix to analyze
    \param iNumRows number of rows in the matrix
    \param iNumCols number of columns in the matrix
    \return
    */
    static float getMax(float** pptMat, int iNumRows, int iNumCols)
    {
        assert(iNumRows > 0);
        assert(iNumCols > 0);
        assert(pptMat);
        assert(pptMat[0]);

        float fGlobalMax = CVector::getMax(pptMat[0], iNumCols);
        for (auto m = 1; m < iNumRows; m++)
        {
            float fMax = CVector::getMax(pptMat[m], iNumCols);
            if (fMax > fGlobalMax)
                fGlobalMax = fMax;
        }
        return fGlobalMax;
    }

    /*! adds a single value to all matrix elements
    \param pptMat matrix to analyze
    \param fAdd scaling factor to apply
    \param iNumRows number of rows in the matrix
    \param iNumCols number of columns in the matrix
    \return
    */
    static void addC_I(float** pptMat, float fAdd, int iNumRows, int iNumCols)
    {
        assert(iNumRows > 0);
        assert(iNumCols > 0);
        assert(pptMat);
        assert(pptMat[0]);

        for (auto m = 0; m < iNumRows; m++)
            CVector::addC_I(pptMat[m], fAdd, iNumCols);
    }

    /*! multiplies the whole matrix with a single factor
    \param pptMat matrix to analyze
    \param fScale scaling factor to apply
    \param iNumRows number of rows in the matrix
    \param iNumCols number of columns in the matrix
    \return
    */
    static void mulC_I(float** pptMat, float fScale, int iNumRows, int iNumCols)
    {
        assert(iNumRows > 0);
        assert(iNumCols > 0);
        assert(pptMat);
        assert(pptMat[0]);

        for (auto m = 0; m < iNumRows; m++)
            CVector::mulC_I(pptMat[m], fScale, iNumCols);
    }


    /*! swaps a matrix row with a column in a square matrix
    \param pptSrcDest resulting matrix (to be modified)
    \param iRowIdx index of row
    \param iColIdx index of columns
    \param iNumCols number of columns in the matrix
    \return
    */
    static void swapRowCol(float** pptSrcDest, int iRowIdx, int iColIdx, int iNumCols)
    {
        assert(iRowIdx > 0);
        assert(iColIdx > 0);
        assert(iNumCols > 0);
        assert(pptSrcDest);
        assert(pptSrcDest[0]);

        for (auto n = 0; n < iNumCols; n++)
        {
            float fTmp = pptSrcDest[iRowIdx][n];
            pptSrcDest[iRowIdx][n] = pptSrcDest[iColIdx][n];
            pptSrcDest[iColIdx][n] = fTmp;
        }
    }

    /*! copies matrix content to another matrix
    \param pptDestMat (destination matrix, user allocated)
    \param pptSrcMat (source matrix)
    \param iNumRows number of rows
    \param iNumCols number of columns
    \return
    */
    template<typename T>
    static void copy(T** pptDestMat, T** pptSrcMat, int iNumRows, int iNumCols)
    {
        assert(pptDestMat);
        assert(pptSrcMat);
        assert(iNumRows > 0);
        assert(iNumCols > 0);

        for (auto m = 0; m < iNumRows; m++)
            CVector::copy(pptDestMat[m], pptSrcMat[m], iNumCols);
    }

    /*! copies vector content to a row
    \param pptDestMat (destination matrix, user allocated)
    \param ptSrcVec (source vector)
    \param iRowIdx index of row to copy to
    \param iNumCols number of columns
    \return
    */
    template<typename T>
    static void setRow(T** pptDestMat, T* ptSrcVec, int iRowIdx, int iNumCols)
    {
        assert(pptDestMat);
        assert(ptSrcVec);
        assert(iRowIdx >= 0);
        assert(iNumCols > 0);

        CVector::copy(pptDestMat[iRowIdx], ptSrcVec, iNumCols);
    }

    template<typename T>
    static T det(T** pptMat, int iNumRows, int iNumCols)
    {
        assert(pptMat);
        assert(iNumRows == iNumCols);

        const float kSingularityThresh = 1e-15F;
        T **pptTmp = 0;
        double dDet = 1;


        if (iNumRows == 2)
            return ((pptMat[0][0] * pptMat[1][1]) - (pptMat[0][1] * pptMat[1][0]));

        CMatrix::alloc(pptTmp, iNumRows, iNumCols);
        CMatrix::copy(pptTmp, pptMat, iNumRows, iNumCols);

        if (pptTmp[0][0] == 0)
        {
            auto i = 1;
            while (i < iNumRows)
            {
                if (pptTmp[i][0] != 0)
                {
                    swapRowCol(pptTmp, 0, i, iNumCols);
                    dDet *= -1;
                    break;
                }
                i++;
            }
        }

        if (pptTmp[0][0] == 0)
        {
            CMatrix::free(pptTmp, iNumRows);
            return 0;
        }

        dDet *= pptTmp[0][0];

        CVector::mulC_I(pptTmp[0], 1.F / pptTmp[0][0], iNumCols);
        for (auto i = 1; i < iNumRows; i++)
        {
            auto j = 0;

            if (std::abs(dDet) < kSingularityThresh * 1. * kSingularityThresh)
                dDet = 0;

            while (j < i)
            {
                CVector::addW_I(pptTmp[i], pptTmp[j], -pptTmp[i][j], iNumCols);
                j++;
            }

            if (pptTmp[i][i] != 0)
            {
                dDet *= pptTmp[i][i];
                CVector::mulC_I(pptTmp[i], 1.F / pptTmp[i][i], iNumCols);
            }

            if (pptTmp[i][i] == 0)
            {
                for (j = i + 1; j < iNumCols; j++)
                {
                    if (pptTmp[i][j] != 0)
                    {
                        CVector::add_I(pptTmp[i], pptTmp[j], iNumCols);

                        dDet *= pptTmp[i][i];
                        CVector::mulC_I(pptTmp[i], 1.F / pptTmp[i][i], iNumCols);
                        break;
                    }
                }
            }

            if (pptTmp[i][i] == 0)
            {
                CMatrix::free(pptTmp, iNumRows);
                return 0;
            }
        }

        CMatrix::free(pptTmp, iNumRows);

        return static_cast<T>(dDet);
    }

    /*! computes inverse of square matrix
    \param pptSrcDest input and output matrix (to be modified, user allocated)
    \param iNumRows number of rows in the matrix
    \param iNumCols number of columns in the matrix
    \return
    */
    template<typename T>
    static void inv_I (T **pptSrcDest, int iNumRows, int iNumCols)
    {
        assert(iNumRows > 0);
        assert(iNumRows == iNumCols);
        assert(pptSrcDest);
        assert(pptSrcDest[0]);

        const float kSingularityThresh = 1e-15F;
        T** ppfTmp = 0;
        T** ppfEye = 0;
        int    i, j;
        double dDet = 1;

        alloc(ppfTmp, iNumRows, iNumCols);
        copy(ppfTmp, pptSrcDest, iNumRows, iNumCols);

        alloc(ppfEye, iNumRows, iNumCols);
        setEye(ppfEye, iNumRows, iNumCols);

        if (pptSrcDest[0][0] == 0)
        {
            i = 1;
            while (i < iNumRows)
            {
                if (pptSrcDest[i][0] != 0)
                {
                    swapRowCol(pptSrcDest, 0, i, iNumCols);
                    swapRowCol(ppfEye, 0, i, iNumCols);
                    dDet *= -1;
                    break;
                }
                i++;
            }
        }

        dDet *= pptSrcDest[0][0];

        CVector::mulC_I(ppfEye[0], 1.F / pptSrcDest[0][0], iNumCols);
        CVector::mulC_I(pptSrcDest[0], 1.F / pptSrcDest[0][0], iNumCols);

        for (i = 1; i < iNumRows; i++)
        {
            j = 0;

            if (std::abs(dDet) < kSingularityThresh * 1.* kSingularityThresh)
                dDet = 0;

            while (j < i)
            {
                CVector::subW_I(ppfEye[i], ppfEye[j], pptSrcDest[i][j], iNumCols);
                CVector::subW_I(pptSrcDest[i], pptSrcDest[j], pptSrcDest[i][j], iNumCols);
                j++;
            }

            if (pptSrcDest[i][i] != 0)
            {
                dDet *= pptSrcDest[i][i];
                CVector::mulC_I(ppfEye[i], 1.F / pptSrcDest[i][i], iNumCols);
                CVector::mulC_I(pptSrcDest[i], 1.F / pptSrcDest[i][i], iNumCols);
            }

            if (pptSrcDest[i][i] == 0)
            {
                for (int j1 = i + 1; j1 < iNumCols; j1++)
                {
                    if (pptSrcDest[i][j1] != 0)			// Column pivotting not supported
                    {
                        for (int i1 = 0; i1 < iNumRows; i1++)
                        {
                            for (j = 0; j < iNumCols; j++)
                                pptSrcDest[i1][j] = ppfTmp[i1][j];
                        }
                        inv_I(ppfTmp, iNumRows, iNumCols);
                        copy(pptSrcDest, ppfTmp, iNumRows, iNumCols);

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
                CVector::subW_I(ppfEye[j], ppfEye[i], pptSrcDest[j][i], iNumCols);
                CVector::subW_I(pptSrcDest[j], pptSrcDest[i], pptSrcDest[j][i], iNumCols);
            }
        }

        copy(pptSrcDest, ppfEye, iNumRows, iNumCols);
        CMatrix::free(ppfTmp, iNumRows);
        CMatrix::free(ppfEye, iNumRows);
    }
};

#endif // __ACA_Matrix_HEADER_INCLUDED__