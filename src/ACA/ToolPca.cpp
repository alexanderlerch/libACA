
#include "Vector.h"
#include "Matrix.h"

#include "ToolPca.h"

CPca::~CPca(void) { reset(); }

Error_t CPca::init(int iNumFeatures, int iNumObs)
{
    if (iNumFeatures <= 0 || iNumObs <= 0)
        return Error_t::kFunctionInvalidArgsError;

    m_iNumFeatures = iNumFeatures;
    m_iNumObs = iNumObs;

    CMatrix::alloc(m_ppfProcTmp, m_iNumFeatures, m_iNumFeatures);
    CMatrix::alloc(m_ppfU, m_iNumFeatures, m_iNumFeatures);
    CMatrix::alloc(m_ppfW, m_iNumFeatures, m_iNumFeatures);
    CMatrix::alloc(m_ppfV, m_iNumFeatures, m_iNumFeatures);

    CVector::alloc(piSortIndices, m_iNumFeatures);

    return Error_t::kNoError;
}

Error_t CPca::reset()
{
    CMatrix::free(m_ppfProcTmp, m_iNumFeatures);
    CMatrix::free(m_ppfU, m_iNumFeatures);
    CMatrix::free(m_ppfW, m_iNumFeatures);
    CMatrix::free(m_ppfV, m_iNumFeatures);

    CVector::free(piSortIndices);

    return Error_t::kNoError;
}

Error_t CPca::compPca(float **ppfRes, float *pfEigenValues, const float *const *const ppfIn)
{
    if (m_iNumFeatures <= 1 || m_iNumObs <= 1)
        return Error_t::kFunctionInvalidArgsError;
    if (!ppfIn || !ppfRes || !pfEigenValues)
        return Error_t::kFunctionInvalidArgsError;
    if (!ppfIn[0] || !ppfRes[0])
        return Error_t::kFunctionInvalidArgsError;

    compCov(m_ppfProcTmp, ppfIn, m_iNumFeatures, m_iNumObs);

    calcSVD(m_ppfU, m_ppfW, m_ppfV, m_ppfProcTmp, m_iNumFeatures, m_iNumFeatures);

    // extract eigenvalues
    CMatrix::getDiag(pfEigenValues, m_ppfW, m_iNumFeatures, m_iNumFeatures);

    // sort eigenvalues
    CVector::sort_I(pfEigenValues, piSortIndices, m_iNumFeatures, false);

    // transpose T
    CMatrix::transpose(m_ppfProcTmp, m_ppfV, m_iNumFeatures, m_iNumFeatures);

    // rearrange transformation matrix
    CMatrix::rearrangeRows(m_ppfProcTmp, piSortIndices, m_iNumFeatures);

    //compute components
    CMatrix::mulMatMat(ppfRes, m_ppfProcTmp, ppfIn, m_iNumFeatures, m_iNumFeatures, m_iNumFeatures, m_iNumObs);

    return Error_t::kNoError;
}

//each row is a variable

Error_t CPca::compCov(float **ppfCovOut, const float *const *const ppfIn, int iNumRows, int iNumCols)
{
    if (iNumRows <= 1 || iNumCols <= 1)
        return Error_t::kFunctionInvalidArgsError;
    if (!ppfIn || !ppfCovOut)
        return Error_t::kFunctionInvalidArgsError;
    if (!ppfIn[0] || !ppfCovOut[0])
        return Error_t::kFunctionInvalidArgsError;

    CMatrix::setZero(ppfCovOut, iNumRows, iNumRows);

    for (auto m = 0; m < iNumRows; m++)
    {
        float fMean1 = CVector::getMean(ppfIn[m], iNumCols);
        for (auto n = 0; n <= m; n++)
        {
            float fMean2 = CVector::getMean(ppfIn[n], iNumCols);
            ppfCovOut[m][n] = 0;
            for (auto j = 0; j < iNumCols; j++)
                ppfCovOut[m][n] += (ppfIn[m][j] - fMean1) * (ppfIn[n][j] - fMean2);
            ppfCovOut[n][m] = ppfCovOut[m][n];
        }
    }
    CMatrix::mulC_I(ppfCovOut, 1.F / (iNumCols - 1), iNumRows, iNumRows);

    return Error_t::kNoError;
}

Error_t CPca::calcSVD(float **ppfU, float **ppfW, float **ppfV, const float *const *const ppfMat, int iNumRows, int iNumCols, int iMaxIterations)
{

    int        i, k, l = 0;

    float *pdRV1;
    float    dTmp,
        dF,
        dH,
        dS,
        dNorm = 0,
        dG = 0,
        dScale = 0;

    if (iNumRows < iNumCols)
        return Error_t::kFunctionIllegalCallError;

    CVector::alloc(pdRV1, iNumCols);
    if (!pdRV1)
        return Error_t::kMemError;

    CMatrix::setZero(ppfU, iNumRows, iNumCols);
    CMatrix::setZero(ppfW, iNumCols, iNumCols);
    CMatrix::setZero(ppfV, iNumCols, iNumCols);

    CMatrix::copy(ppfU, ppfMat, iNumRows, iNumCols);

    for (i = 0; i < iNumCols; i++)
    {
        l = i + 1;
        pdRV1[i] = dScale * dG;

        dG = 0;
        dS = 0;
        dScale = 0;

        if (i < iNumRows)
        {
            for (k = i; k < iNumRows; k++)
                dScale += fabs(ppfU[k][i]);

            if (dScale)
            {
                for (auto j = i; j < iNumRows; j++)
                    ppfU[j][i] *= 1 / dScale;

                for (k = i; k < iNumRows; k++)
                {
                    dTmp = ppfU[k][i];
                    dS += dTmp * dTmp;
                }

                dF = ppfU[i][i];
                dG = std::sqrt(dS);
                if (dF >= 0)
                    dG *= -1;
                dH = dF * dG - dS;
                ppfU[i][i] = dF - dG;

                if (i != iNumCols)
                {
                    for (auto j = l; j < iNumCols; j++)
                    {
                        dS = 0;
                        for (k = i; k < iNumRows; k++)
                            dS += ppfU[k][i] * ppfU[k][j];
                        dF = dS / dH;
                        for (k = i; k < iNumRows; k++)
                            ppfU[k][j] += dF * ppfU[k][i];
                    }
                }
                for (auto j = i; j < iNumRows; j++)
                    ppfU[j][i] *= dScale;
            }
        }

        ppfW[i][i] = dScale * dG;

        dG = 0;
        dS = 0;
        dScale = 0;

        if ((i <= iNumRows) && (i != iNumCols))
        {
            for (k = l; k < iNumCols; k++)
                dScale += fabs(ppfU[i][k]);

            if (dScale)
            {
                for (auto j = l; j < iNumCols; j++)
                    ppfU[i][j] *= 1 / dScale;
                for (k = l; k < iNumCols; k++)
                {
                    dTmp = ppfU[i][k];
                    dS += dTmp * dTmp;
                }

                dF = ppfU[i][l];
                dG = std::sqrt(dS);
                if (dF >= 0)
                    dG *= -1;
                dH = dF * dG - dS;
                ppfU[i][l] = dF - dG;

                for (k = l; k < iNumCols; k++)
                    pdRV1[k] = ppfU[i][k] / dH;

                if (i != iNumRows)
                {
                    for (auto j = l; j < iNumRows; j++)
                    {
                        dS = 0;
                        for (k = l; k < iNumCols; k++)
                            dS += ppfU[j][k] * ppfU[i][k];
                        for (k = l; k < iNumCols; k++)
                            ppfU[j][k] += dS * pdRV1[k];
                    }
                }
                for (auto j = l; j < iNumCols; j++)
                    ppfU[i][j] *= dScale;
            }
        }
        dTmp = fabs(ppfW[i][i]) + fabs(pdRV1[i]);
        dNorm = std::max(dNorm, dTmp);
    }

    for (i = iNumCols - 1; i >= 0; i--)
    {
        if (i < iNumCols)
        {
            if (dG)
            {
                for (auto j = l; j < iNumCols; j++)
                {
                    dTmp = ppfU[i][j] / ppfU[i][l];
                    ppfV[j][i] = dTmp / dG;
                }
                for (auto j = l; j < iNumCols; j++)
                {
                    dS = 0;
                    for (k = l; k < iNumCols; k++)
                        dS += ppfU[i][k] * ppfV[k][j];
                    for (k = l; k < iNumCols; k++)
                        ppfV[k][j] += dS * ppfV[k][i];
                }
            }
            for (auto j = l; j < iNumCols; j++)
            {
                ppfV[i][j] = 0;
                ppfV[j][i] = 0;
            }
        }
        ppfV[i][i] = 1;
        dG = pdRV1[i];
        l = i;
    }

    for (i = iNumCols - 1; i >= 0; i--)
    {
        l = i + 1;
        dG = ppfW[i][i];

        if (i < iNumCols)
        {   
            for (auto j = l; j < iNumCols; j++)
                ppfU[i][j] = 0;
        }

        if (dG)
        {
            dG = 1.F / dG;
            if (i != iNumCols)
            {
                for (auto j = l; j < iNumCols; j++)
                {
                    dS = 0;
                    for (k = l; k < iNumRows; k++)
                        dS += ppfU[k][i] * ppfU[k][j];
                    dF = (dS / ppfU[i][i]) * dG;
                    for (k = i; k < iNumRows; k++)
                        ppfU[k][j] += dF * ppfU[k][i];
                }
            }
        }

        for (auto j = i; j < iNumRows; j++)
            ppfU[j][i] *= dG;
        ppfU[i][i] += 1;
    }

    for (k = iNumCols - 1; k >= 0; k--)
    {
        for (int iIter = 0; iIter < iMaxIterations; iIter++)
        {
            float    dC, dX, dY, dZ;
            int        iNM = 0,
                iFlag = 1;
            for (l = k; l >= 0; l--)
            {
                iNM = l - 1;
                if (fabs(pdRV1[l]) + dNorm == dNorm)
                {
                    iFlag = 0;
                    break;
                }
                if (fabs(ppfW[iNM][iNM]) + dNorm == dNorm)
                    break;
            }
            if (iFlag)
            {
                dC = 0.0;
                dS = 1.0;
                for (i = l; i <= k; i++)
                {
                    dF = dS * pdRV1[i];
                    if (fabs(dF) + dNorm != dNorm)
                    {
                        dG = ppfW[i][i];
                        dH = matPythag(dF, dG);
                        ppfW[i][i] = dH;
                        dH = 1.F / dH;
                        dC = dG * dH;
                        dS = (-dF * dH);

                        for (auto j = 0; j < iNumRows; j++)
                        {
                            dY = ppfU[j][iNM];
                            dZ = ppfU[j][i];
                            ppfU[j][iNM] = dY * dC + dZ * dS;
                            ppfU[j][i] = dZ * dC - dY * dS;
                        }
                    }
                }
            }

            dZ = ppfW[k][k];

            if (l == k)
            {
                if (dZ < 0)
                {
                    ppfW[k][k] = -dZ;
                    for (auto j = 0; j < iNumCols; j++)
                        ppfV[j][k] *= -1;
                }
                break;
            }

            if (iIter >= iMaxIterations)
                return Error_t::kUnknownError;

            dX = ppfW[l][l];
            iNM = k - 1;
            dY = ppfW[iNM][iNM];
            dG = pdRV1[iNM];
            dH = pdRV1[k];
            dF = ((dY - dZ) * (dY + dZ) + (dG - dH) * (dG + dH)) / (2.F * dH * dY);
            dG = matPythag(dF, 1.);
            dF = ((dX - dZ) * (dX + dZ) + dH * ((dY / (dF + (dF >= 0 ? fabs(dG) : -fabs(dG)))) - dH)) / dX;
            dC = 1;
            dS = 1;

            for (auto j = l; j <= iNM; j++)
            {
                int jj;

                i = j + 1;
                dG = pdRV1[i];
                dY = ppfW[i][i];
                dH = dS * dG;
                dG = dC * dG;
                dZ = matPythag(dF, dH);
                pdRV1[j] = dZ;
                dC = dF / dZ;
                dS = dH / dZ;
                dF = (dX * dC) + (dG * dS);
                dG = (dG * dC) - (dX * dS);
                dH = dY * dS;
                dY = dY * dC;

                for (jj = 0; jj < iNumCols; jj++)
                {
                    dX = ppfV[jj][j];
                    dZ = ppfV[jj][i];

                    ppfV[jj][j] = dX * dC + dZ * dS;
                    ppfV[jj][i] = dZ * dC - dX * dS;
                }

                dZ = matPythag(dF, dH);
                ppfW[j][j] = dZ;

                if (dZ)
                {
                    dZ = 1.F / dZ;
                    dC = dF * dZ;
                    dS = dH * dZ;
                }
                dF = (dC * dG) + (dS * dY);
                dX = (dC * dY) - (dS * dG);

                for (jj = 0; jj < iNumRows; jj++)
                {
                    dY = ppfU[jj][j];
                    dZ = ppfU[jj][i];
                    ppfU[jj][j] = dY * dC + dZ * dS;
                    ppfU[jj][i] = dZ * dC - dY * dS;
                }
            }
            pdRV1[l] = 0;
            pdRV1[k] = dF;
            ppfW[k][k] = dX;
        }
    }

    CVector::free(pdRV1);

    return Error_t::kNoError;
}

float CPca::matPythag(float dA, float dB)
{
    float    dC,
        dMult;

    if (fabs(dA) > fabs(dB))
    {
        dC = dB / dA;
        dMult = dA;
    }
    else
    {
        if (!dB)
            return 0;
        dC = dA / dB;
        dMult = dB;
    }
    return (dMult * std::sqrt(1 + (dC * dC)));
}
