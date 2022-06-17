
#include "Vector.h"
#include "Matrix.h"

#include "ToolViterbi.h"

CViterbi::CViterbi(void)
{
    reset();
}

CViterbi::~CViterbi(void)
{
    reset();
}

Error_t CViterbi::init(const float *const *const ppfPTransition, const float *pfPStart, int iNumStates, int iNumObs)
{
    if (!ppfPTransition || !pfPStart)
        return Error_t::kFunctionInvalidArgsError;

    if (iNumStates <= 0 || iNumObs <= 0)
        return Error_t::kFunctionInvalidArgsError;

    assert(ppfPTransition[0]);

    reset();

    m_iNumStates = iNumStates;
    m_iNumObs = iNumObs;

    // allocate memory
    CVector::alloc(m_pfStart, m_iNumStates);

    CMatrix::alloc(m_ppfProb, m_iNumStates, m_iNumObs);

    CMatrix::alloc(m_ppfTransProb, m_iNumStates, m_iNumStates);
    CMatrix::alloc(m_ppiPathIdx, m_iNumStates, m_iNumObs);

    // set values
    CVector::copy(m_pfStart, pfPStart, iNumStates);
    CMatrix::copy(m_ppfTransProb, ppfPTransition, m_iNumStates, m_iNumStates);

    // all done here
    m_bIsInitialized = true;

    return Error_t::kNoError;
}

Error_t CViterbi::reset()
{
    m_bIsInitialized = false;
    m_bWasProcessed = false;

    CVector::free(m_pfStart);

    CMatrix::free(m_ppfProb, m_iNumStates);

    CMatrix::free(m_ppfTransProb, m_iNumStates);
    CMatrix::free(m_ppiPathIdx, m_iNumStates);

    m_iNumStates = 0;
    m_iNumObs = 0;
    m_fOverallProb = 0;

    return Error_t::kNoError;
}

Error_t CViterbi::compViterbi(const float *const *const ppfPEmission, bool bUseLogLikelihood /*= true*/)
{
    if (!m_bIsInitialized)
        return Error_t::kNotInitializedError;

    if (!ppfPEmission)
        return Error_t::kFunctionInvalidArgsError;

    assert(ppfPEmission[0]);

    // the computation of the probability matrix takes place here
    if (!bUseLogLikelihood)
        compProbability_(ppfPEmission);
    else
        compLogLikelihood_(ppfPEmission);

    // find overall probability
    m_fOverallProb = -std::numeric_limits<float>::max();
    for (auto m = 0; m < m_iNumStates; m++)
    {
        if (m_ppfProb[m][m_iNumObs - 1] > m_fOverallProb)
        {
            m_fOverallProb = m_ppfProb[m][m_iNumObs - 1];
            m_iEndState = m;
        }
    }

    // all done
    m_bWasProcessed = true;

    return Error_t::kNoError;
}

void CViterbi::compProbability_(const float *const *const ppfPEmission)
{
    // initialize
    for (auto m = 0; m < m_iNumStates; m++)
    {
        CVector::setValue(m_ppfProb[m], 1.F, m_iNumObs);
        m_ppfProb[m][0] = ppfPEmission[m][0] * m_pfStart[m];
    }

    // compute probability matrix and store backtracking path
    for (int n = 1; n < m_iNumObs; n++)
    {
        for (int m = 0; m < m_iNumStates; m++)
        {
            // find max of preceding prob times trans prob
            float fMaxProb = 0;
            for (auto s = 0; s < m_iNumStates; s++)
            {
                float fProbsm = m_ppfProb[s][n - 1] * m_ppfTransProb[s][m];
                if (fProbsm > fMaxProb)
                {
                    fMaxProb = fProbsm;
                    m_ppfProb[m][n] = fMaxProb;
                    m_ppiPathIdx[m][n] = s;
                }
            }
            m_ppfProb[m][n] *= ppfPEmission[m][n];
        }
    }
}

void CViterbi::compLogLikelihood_(const float *const *const ppfPEmission)
{
    // convert trans prob to log
    for (auto m = 0; m < m_iNumStates; m++)
        for (auto s = 0; s < m_iNumStates; s++)
            m_ppfTransProb[m][s] = std::log(m_ppfTransProb[m][s] + m_kLogMin);

    // initialize
    for (auto m = 0; m < m_iNumStates; m++)
        m_ppfProb[m][0] = std::log(ppfPEmission[m][0] + m_kLogMin) + std::log(m_pfStart[m] + m_kLogMin);

    // compute probability matrix and store backtracking path
    for (int n = 1; n < m_iNumObs; n++)
    {
        for (int m = 0; m < m_iNumStates; m++)
        {
            // find max of preceding prob times trans prob
            float fMaxProb = -std::numeric_limits<float>::max();
            for (auto s = 0; s < m_iNumStates; s++)
            {
                float fProbsm = m_ppfProb[s][n - 1] + m_ppfTransProb[s][m];
                if (fProbsm > fMaxProb)
                {
                    fMaxProb = fProbsm;
                    m_ppfProb[m][n] = fMaxProb;
                    m_ppiPathIdx[m][n] = s;
                }
            }
            m_ppfProb[m][n] += std::log(ppfPEmission[m][n] + m_kLogMin);
        }
    }
}


float CViterbi::getOverallProbability() const
{
    return m_fOverallProb;
}

Error_t CViterbi::getStateSequence(int *piStateSequence) const
{
    if (!piStateSequence)
        return Error_t::kFunctionInvalidArgsError;

    if (!m_bWasProcessed || m_iEndState < 0)
        return Error_t::kFunctionIllegalCallError;

    int iIdx = m_iNumObs - 1;

    // init
    piStateSequence[iIdx] = m_iEndState;

    // trace back
    while (iIdx > 0)
    {
        piStateSequence[iIdx - 1] = m_ppiPathIdx[piStateSequence[iIdx]][iIdx];
        iIdx--;
    }

    return Error_t::kNoError;
}

