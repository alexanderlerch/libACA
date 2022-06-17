#if !defined(__ACA_Matrix_HEADER_INCLUDED__)
#define __ACA_Matrix_HEADER_INCLUDED__

#include <cassert>
#include <algorithm>

#include "Vector.h"

/*! \brief class with static functions for matrix operations, works with both float and double (functionality only added when needed)
*/
class CMatrix
{
public:

    //////////////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////////////
    /*! allocates a matrix and inits it with zeros
    \param pptMat (empty double pointer, to be allocated)
    \param iNumRows number of rows
    \param iNumCols number of columns
    */
    template<typename T>
    static inline void alloc(T **&pptMat, int iNumRows, int iNumCols)
    {
        assert(iNumRows > 0);
        assert(iNumCols > 0);

        CVector::alloc(pptMat, iNumRows);
        assert(pptMat);

        for (auto m = 0; m < iNumRows; m++)
            CVector::alloc(pptMat[m], iNumCols);
    }

    /*! copies matrix content to another matrix
    \param pptDestMat (destination matrix, user allocated)
    \param pptSrcMat (source matrix)
    \param iNumRows number of rows
    \param iNumCols number of columns
    */
    template<typename T>
    static inline void copy(T **pptDestMat, const T *const *const pptSrcMat, int iNumRows, int iNumCols)
    {
        assert(pptDestMat);
        assert(pptSrcMat);
        assert(iNumRows > 0);
        assert(iNumCols > 0);

        for (auto m = 0; m < iNumRows; m++)
            CVector::copy(pptDestMat[m], pptSrcMat[m], iNumCols);
    }

    /*! frees a matrix
    \param pptMat (empty double pointer, to be set to zero)
    \param iNumRows number of rows
    */
    template<typename T>
    static inline void free(T **&pptMat, int iNumRows)
    {
        if (!pptMat) return;
        assert(iNumRows > 0);

        for (auto m = 0; m < iNumRows; m++)
            CVector::free(pptMat[m]);

        CVector::free(pptMat);
        pptMat = 0;
    }

    //////////////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////////////
    /*! copies vector content to a column
    \param pptDestMat (destination matrix, user allocated)
    \param ptSrcVec (source vector)
    \param iColIdx index of row to copy to
    \param iNumRows number of columns
    */
    template<typename T>
    static inline void setCol(T **pptDestMat, const T *ptSrcVec, int iColIdx, int iNumRows)
    {
        assert(pptDestMat);
        assert(ptSrcVec);
        assert(iColIdx >= 0);
        assert(iNumRows > 0);

        for (auto m = 0; m < iNumRows; m++)
            pptDestMat[m][iColIdx] = ptSrcVec[m];
    }

    /*! fills matrix with ones on the diagonal, zeros elsewhere
    \param pptDest resulting matrix of dimension iNumRows x iNumCols (to be written, user allocated)
    \param iNumRows number of rows in the matrix
    \param iNumCols number of columns in the matrix
    */
    template<typename T>
    static inline void setEye(T **pptDest, int iNumRows, int iNumCols)
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

    /*! sets all matrix elements to a random value
    \param pptMat pointer to memory to be set
    \param iNumRows number of rows
    \param iNumCols number of columns
    */
    template<typename T>
    static inline void setRand(T **pptMat, int iNumRows, int iNumCols)
    {
        assert(iNumRows > 0);
        assert(iNumCols > 0);
        assert(pptMat);
        assert(pptMat[0]);

        for (auto m = 0; m < iNumRows; m++)
            CVector::setRand(pptMat[m], iNumCols);
    }

    /*! copies vector content to a row
    \param pptDestMat (destination matrix, user allocated)
    \param ptSrcVec (source vector)
    \param iRowIdx index of row to copy to
    \param iNumCols number of columns
    */
    template<typename T>
    static inline void setRow(T **pptDestMat, const T *ptSrcVec, int iRowIdx, int iNumCols)
    {
        assert(pptDestMat);
        assert(ptSrcVec);
        assert(iRowIdx >= 0);
        assert(iNumCols > 0);

        CVector::copy(pptDestMat[iRowIdx], ptSrcVec, iNumCols);
    }

    /*! sets all matrix elements to a value
    \param pptMat pointer to memory to be set
    \param tValue  value to use
    \param iNumRows number of rows
    \param iNumCols number of columns
    */
    template<typename T>
    static inline void setValue(T **pptMat, T tValue, int iNumRows, int iNumCols)
    {
        assert(iNumRows > 0);
        assert(iNumCols > 0);
        assert(pptMat);
        assert(pptMat[0]);

        for (auto m = 0; m < iNumRows; m++)
            CVector::setValue(pptMat[m], tValue, iNumCols);
    }

    /*! sets all matrix elements to zero
    \param pptMat pointer to memory to be set
    \param iNumRows number of rows
    \param iNumCols number of columns
    */
    template<typename T>
    static inline void setZero(T **pptMat, int iNumRows, int iNumCols)
    {
        assert(iNumRows > 0);
        assert(iNumCols > 0);
        assert(pptMat);
        assert(pptMat[0]);

        for (auto m = 0; m < iNumRows; m++)
            CVector::setZero(pptMat[m], iNumCols);
    }

    /*! sets all elements in a matrix below a threshold to zero
    \param pptMat pointer to memory to be set
    \param iNumRows number of rows
    \param iNumCols number of columns
    \param tThresh threshold
    */
    template<typename T>
    static inline void setZeroBelowThresh(T **pptMat, int iNumRows, int iNumCols, T tThresh)
    {
        assert(iNumRows > 0);
        assert(iNumCols > 0);
        assert(pptMat);
        assert(pptMat[0]);

        for (auto m = 0; m < iNumRows; m++)
            CVector::setZeroBelowThresh(pptMat[m], iNumCols, tThresh);
    }

    /*! normalizes each column of the matrix (p=1)
    \param pptSrcDest resulting matrix of dimension iNumRows x iNumCols (to be written, user allocated)
    \param iNumRows number of rows in the matrix
    \param iNumCols number of columns in the matrix
    */
    template<typename T>
    static inline void vecnorm_I(T **pptSrcDest, int iNumRows, int iNumCols)
    {
        assert(iNumRows > 0);
        assert(iNumCols > 0);
        assert(pptSrcDest);
        assert(pptSrcDest[0]);

        for (auto n = 0; n < iNumCols; n++)
        {
            T tNorm = 0;
            for (auto m = 0; m < iNumRows; m++)
                tNorm += std::abs(pptSrcDest[m][n]);
            if (tNorm > 0)
                for (auto m = 0; m < iNumRows; m++)
                    pptSrcDest[m][n] /= tNorm;
        }
    }

    //////////////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////////////
    /*! returns a column as vector
    \param ptDest resulting vector of dimension iNumRows (to be written, user allocated)
    \param pptMat input matrix of dimension iNumRows x iNumCols (to be written, user allocated)
    \param iColIdx index of column to retrieve
    \param iNumRows number of rows in the matrix
    */
    template<typename T>
    static inline void getCol(T *ptDest, const T *const *const pptMat, int iColIdx, int iNumRows)
    {
        assert(iColIdx >= 0);
        assert(iNumRows > 0);
        assert(pptMat);
        assert(ptDest);
        assert(pptMat[0]);

        for (auto m = 0; m < iNumRows; m++)
            ptDest[m] = pptMat[m][iColIdx];
    }

    /*! returns matrix diagonal as vector
    \param ptDest resulting vector of dimension min(iNumSrcRows, iNumCols) (to be written, user allocated)
    \param pptSrc input matrix of dimension iNumSrcRows x iNumCols
    \param iNumSrcRows number of rows in the matrix
    \param iNumSrcCols number of columns in the matrix
    */
    template<typename T>
    static inline void getDiag(T *ptDest, const T *const *const pptSrc, int iNumSrcRows, int iNumSrcCols)
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

    /*! return maximum value in a matrix
    \param pptMat matrix to analyze
    \param iNumRows number of rows in the matrix
    \param iNumCols number of columns in the matrix
    \return max
    */
    template<typename T>
    static inline T getMax(const T *const *const pptMat, int iNumRows, int iNumCols)
    {
        assert(iNumRows > 0);
        assert(iNumCols > 0);
        assert(pptMat);
        assert(pptMat[0]);

        T fGlobalMax = CVector::getMax(pptMat[0], iNumCols);
        for (auto m = 1; m < iNumRows; m++)
        {
            T fMax = CVector::getMax(pptMat[m], iNumCols);
            if (fMax > fGlobalMax)
                fGlobalMax = fMax;
        }
        return fGlobalMax;
    }

    /*! return the matrix norm (p=1)
    \param pptMat input matrix of dimension iNumRows x iNumCols (to be written, user allocated)
    \param iNumRows number of rows in the matrix
    \param iNumCols number of columns in the matrix
    \return norm
    */
    template<typename T>
    static inline T getNorm(const T *const *const pptMat, int iNumRows, int iNumCols)
    {
        assert(iNumRows > 0);
        assert(iNumCols > 0);
        assert(pptMat);
        assert(pptMat[0]);

        T tResult = 0;

        for (auto n = 0; n < iNumCols; n++)
        {
            T tTmp = 0;
            for (auto m = 0; m < iNumRows; m++)
                tTmp += std::abs(pptMat[m][n]);

            if (tTmp > tResult)
                tResult = tTmp;
        }

        return tResult;
    }

    /*! adds all matrix elements
    \param pptMat input matrix of dimension iNumRows x iNumCols (to be written, user allocated)
    \param iNumRows number of rows in the matrix
    \param iNumCols number of columns in the matrix
    \param bAbs flag to indicate whether to sum the absolute values
    \return sum
    */
    template<typename T>
    static inline T getSum(T **pptMat, int iNumRows, int iNumCols, bool bAbs = false)
    {
        assert(iNumRows > 0);
        assert(iNumCols > 0);
        assert(pptMat);
        assert(pptMat[0]);

        T tResult = 0;

        for (auto m = 0; m < iNumRows; m++)
            tResult += CVector::getSum(pptMat[m], iNumCols, bAbs);

        return tResult;
    }

    /*! adds all elements in one columns
    \param pptMat input matrix of dimension iNumRows x iNumCols
    \param iColIdx column of interest
    \param iNumRows number of rows in the matrix
    \param bAbs flag to indicate whether to sum the absolute values
    \return sum
    */
    template<typename T>
    static inline T getSumCol(T **pptMat, int iColIdx, int iNumRows, bool bAbs = false)
    {
        assert(iNumRows > 0);
        assert(pptMat);
        assert(pptMat[0]);

        T tResult = 0;

        if (bAbs)
        {
            for (auto m = 0; m < iNumRows; m++)
                tResult += std::abs(pptMat[m][iColIdx]);
        }
        else
        {
            for (auto m = 0; m < iNumRows; m++)
                tResult += pptMat[m][iColIdx];
        }

        return tResult;
    }

    //////////////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////////////
    /*! comnputes kl divergence between two matrices
    \param pptSrc1 matrix of dimension iNumRows x iNumCols
    \param pptSrc2 matrix of dimension iNumRows x iNumCols
    \param iNumRows number of rows in the matrix
    \param iNumCols number of columns in the matrix
    \return kld
    */
    template<typename T>
    static T calcKlDivergence(const T *const *const pptSrc1, const T *const *const pptSrc2, int iNumRows, int iNumCols)
    {
        assert(iNumRows > 0);
        assert(iNumCols > 0);
        assert(pptSrc1);
        assert(pptSrc1[0]);
        assert(pptSrc2);
        assert(pptSrc2[0]);

        T tResult = 0;

        for (int i = 0; i < iNumRows; i++)
        {
            for (int j = 0; j < iNumCols; j++)
                tResult += pptSrc1[i][j] * (std::log((pptSrc1[i][j] + 1e-24F) / (pptSrc2[i][j] + 1e-24F)) - 1.F) + pptSrc2[i][j];
        }

        return tResult;
    }

    //////////////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////////////
    /*! elementwise addition of two matrices inplace
    \param pptSrcDest resulting matrix
    \param pptSrc matrix to be multiplied
    \param iNumRows number of rows
    \param iNumCols number of columns
    */
    template<typename T>
    static inline void add_I(T **pptSrcDest, const T *const *const pptSrc, int iNumRows, int iNumCols)
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

    /*! adds a single value to all matrix elements
    \param pptMat matrix to analyze
    \param tAdd scaling factor to apply
    \param iNumRows number of rows in the matrix
    \param iNumCols number of columns in the matrix
    */
    template<typename T>
    static inline void addC_I(T **pptMat, T tAdd, int iNumRows, int iNumCols)
    {
        assert(iNumRows > 0);
        assert(iNumCols > 0);
        assert(pptMat);
        assert(pptMat[0]);

        for (auto m = 0; m < iNumRows; m++)
            CVector::addC_I(pptMat[m], tAdd, iNumCols);
    }

    /*! elementwise division of two matrices inplace
    \param pptSrcDest resulting matrix
    \param pptSrc matrix to be multiplied
    \param iNumRows number of rows
    \param iNumCols number of columns
    \param bAddSmallConst a small floating point number is added before division if true
    */
    template<typename T>
    static inline void div_I(T **pptSrcDest, const T *const *const pptSrc, int iNumRows, int iNumCols, bool bAddSmallConst = false)
    {
        assert(iNumRows > 0);
        assert(iNumRows > 0);
        assert(pptSrcDest);
        assert(pptSrcDest[0]);
        assert(pptSrc);
        assert(pptSrc[0]);

        T fEpsilon = bAddSmallConst ? 1e-30F : .0F;

        for (auto m = 0; m < iNumRows; m++)
        {
            for (auto n = 0; n < iNumCols; n++)
                pptSrcDest[m][n] /= (pptSrc[m][n] + fEpsilon);
        }
    }

    /*! elementwise multiplication of two matrices inplace
    \param pptSrcDest resulting matrix
    \param pptSrc matrix to be multiplied
    \param iNumRows number of rows
    \param iNumCols number of columns
    */
    template<typename T>
    static inline void mul_I(T **pptSrcDest, const T *const *const pptSrc, int iNumRows, int iNumCols)
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

    /*! multiplies the whole matrix with a single factor
    \param pptMat matrix to analyze
    \param tScale scaling factor to apply
    \param iNumRows number of rows in the matrix
    \param iNumCols number of columns in the matrix
    */
    template<typename T>
    static inline void mulC_I(T **pptMat, T tScale, int iNumRows, int iNumCols)
    {
        assert(iNumRows > 0);
        assert(iNumCols > 0);
        assert(pptMat);
        assert(pptMat[0]);

        for (auto m = 0; m < iNumRows; m++)
            CVector::mulC_I(pptMat[m], tScale, iNumCols);
    }


    /*! multiplies a column of a matrix with a constant
    \param pptMat matrix with column to be multiplied
    \param fValue value to multiply the column with
    \param iCol index of column to be multiplied
    \param iNumRows number of rows in the matrix
    */
    template<typename T>
    static inline void mulColC_I(T **pptMat, T fValue, int iCol, int iNumRows)
    {
        assert(iNumRows > 0);
        assert(iCol >= 0);
        assert(pptMat);
        assert(pptMat[0]);

        for (auto m = 0; m < iNumRows; m++)
            pptMat[m][iCol] *= fValue;
    }

    /*! multiplies a column vec with a row vec (VEC * VEC)
    \param pptDestMat resulting matrix of dimension iNumMatRows X iNumMatCols (to be written, user allocated)
    \param ptSrcColVec (column) vector to be multiplied
    \param ptSrcRowVec (row) vector to be multiplied
    \param iNumMatRows number of rows in the matrix
    \param iNumMatCols number of columns in the matrix
    */
    template<typename T>
    static inline void mulColvecRowvec(T **pptDestMat, const T *ptSrcColVec, const T *ptSrcRowVec, int iNumMatRows, int iNumMatCols)
    {
        assert(iNumMatRows > 0);
        assert(iNumMatCols > 0);
        assert(ptSrcRowVec);
        assert(ptSrcColVec);
        assert(pptDestMat);
        assert(pptDestMat[0]);

        for (auto m = 0; m < iNumMatRows; m++)
        {
            for (auto n = 0; n < iNumMatCols; n++)
                pptDestMat[m][n] = ptSrcColVec[m] * ptSrcRowVec[n];
        }
    }

    /*! multiplies a matrix with a column vector (MAT * VEC)
    \param ptDestColVec resulting (column) vector of length iNumMatRows (to be written, user allocated)
    \param pptMat matrix to be multiplied
    \param ptSrcColVec (column) vector to be multiplied
    \param iNumMatRows number of rows in the matrix
    \param iNumMatCols number of columns in the matrix
    */
    template<typename T>
    static inline void mulMatColvec(T *ptDestColVec, const T *const *const pptMat, const T *ptSrcColVec, int iNumMatRows, int iNumMatCols)
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

    /*! multiplies a matrix with a matrix (MAT1 * MAT2)
    \param pptDest resulting matrix of dimension iNum1Rows x iNum2Cols (to be written, user allocated)
    \param pptSrc1 first matrix to be multiplied
    \param pptSrc2 second matrix to be multiplied
    \param iNum1Rows number of rows in matrix 1
    \param iNum1Cols number of columns in matrix 1 (has to equal iNum2Rows)
    \param iNum2Rows number of rows in matrix 2 (has to equal iNum1Col)
    \param iNum2Cols number of columns in matrix 2
    */
    template<typename T>
    static inline void mulMatMat(T **pptDest, const T *const *const pptSrc1, const T *const *const pptSrc2, int iNum1Rows, int iNum1Cols, int iNum2Rows, int iNum2Cols)
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

    /*! multiplies a row vector with a matrix (VEC * MAT)
    \param ptDestRowVec resulting (row) vector of length iNumMatCols (to be written, user allocated)
    \param ptSrcRowVec (column) vector to be multiplied
    \param pptMat matrix to be multiplied
    \param iNumMatRows number of rows in the matrix
    \param iNumMatCols number of columns in the matrix
    */
    template<typename T>
    static inline void mulRowvecMat(T *ptDestRowVec, const T *ptSrcRowVec, const T *const *const pptMat, int iNumMatRows, int iNumMatCols)
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

    /*! elementwise subtraction of two matrices inplace
    \param pptSrcDest resulting matrix
    \param pptSrc matrix to be multiplied
    \param iNumRows number of rows
    \param iNumCols number of columns
    */
    template<typename T>
    static inline void sub_I(T **pptSrcDest, const T *const *const pptSrc, int iNumRows, int iNumCols)
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

    //////////////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////////////
    template<typename T>
    static T det(const T *const *const pptMat, int iNumRows, int iNumCols)
    {
        assert(pptMat);
        assert(iNumRows == iNumCols);

        const T kSingularityThresh = 1e-15F;
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
    */
    template<typename T>
    static inline void inv_I(T **pptSrcDest, int iNumRows, int iNumCols)
    {
        assert(iNumRows > 0);
        assert(iNumRows == iNumCols);
        assert(pptSrcDest);
        assert(pptSrcDest[0]);

        const T kSingularityThresh = 1e-15F;
        T **ppfTmp = 0;
        T **ppfEye = 0;
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

            if (std::abs(dDet) < kSingularityThresh * 1. * kSingularityThresh)
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
                    if (pptSrcDest[i][j1] != 0)	
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

    /*! rearrange the rows of a matrix according to a row index vector
    \param pptSrcDest resorted matrix of dimension iNumRows x ?
    \param piRowIndices new indices iNumRows
    \param iNumRows number of rows in the matrix
    */
    template<typename T>
    static inline void rearrangeRows(T **pptSrcDest, int *piRowIndices, int iNumRows)
    {
        assert(iNumRows > 0);
        assert(piRowIndices);
        assert(pptSrcDest);
        assert(pptSrcDest[0]);

        T **pptTmp = 0;
        CVector::alloc(pptTmp, iNumRows);

        for (auto m = 0; m < iNumRows; m++)
        {
            assert(piRowIndices[m] < iNumRows);
            assert(piRowIndices[m] >= 0);

            pptTmp[m] = pptSrcDest[piRowIndices[m]];
        }
        CVector::copy(pptSrcDest, pptTmp, iNumRows);

        CVector::free(pptTmp);
    }

    /*! swaps a matrix row with a column in a square matrix
    \param pptSrcDest resulting matrix (to be modified)
    \param iRowIdx index of row
    \param iColIdx index of columns
    \param iNumCols number of columns in the matrix
    */
    template<typename T>
    static inline void swapRowCol(T **pptSrcDest, int iRowIdx, int iColIdx, int iNumCols)
    {
        assert(iRowIdx > 0);
        assert(iColIdx > 0);
        assert(iNumCols > 0);
        assert(pptSrcDest);
        assert(pptSrcDest[0]);

        for (auto n = 0; n < iNumCols; n++)
        {
            T fTmp = pptSrcDest[iRowIdx][n];
            pptSrcDest[iRowIdx][n] = pptSrcDest[iColIdx][n];
            pptSrcDest[iColIdx][n] = fTmp;
        }
    }

    /*! transposes matrix pptSrc and write the result to pptDest
    \param pptDest resulting matrix of dimension iNumSrcCols x iNumSrcRows (to be written, user allocated)
    \param pptSrc input matrix of dimension iNumSrcRows x iNumCols
    \param iNumSrcRows number of rows in the matrix
    \param iNumSrcCols number of columns in the matrix
    */
    template<typename T>
    static inline void transpose(T **pptDest, const T *const *const pptSrc, int iNumSrcRows, int iNumSrcCols)
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
};

#endif // __ACA_Matrix_HEADER_INCLUDED__
