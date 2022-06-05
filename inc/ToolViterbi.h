#if !defined(__Viterbi_HEADER_INCLUDED__)
#define __Viterbi_HEADER_INCLUDED__

#pragma once

#include "ErrorDef.h"

/*! \brief viterbi algorithm
*/
class CViterbi
{
public:
    CViterbi(void);
    virtual ~CViterbi(void);

    /*! initializes the class
    \param ppfPTransition transition probability matrix (dimension iNumStates X iNumStates)
    \param pfPStart start probabilities (length iNumStates)
    \param iNumStates number of states
    \param iNumObservations number of observations
    \return Error_t
    */
    Error_t init (float **ppfPTransition, float *pfPStart, int iNumStates, int iNumObservations);
    
    /*! resets all internal class members
    \return Error_t
    */
    Error_t reset ();

    /*! computes cost and path w/o back-tracking
    \param ppfPEmission (dimensions iNumStates X iNumObservations)
    \param bUseLogLikelihood (use logarithmic likelihood - needed at least for longer sequences)
    \return Error_t
    */
    Error_t compViterbi (float **ppfPEmission, bool bUseLogLikelihood = true);
    
    /*! returns the overall probability
    \return float
    */
    float getOverallProbability () const;
    
    /*! returns the state sequence
    \param piStateSequence pointer to memory the result is written to (user allocated, length iNumObservations)
    \return Error_t
    */
    Error_t getStateSequence ( int *piStateSequence) const;

private:
    CViterbi(const CViterbi& that);
    CViterbi& operator=(const CViterbi& c);

    void compProbability_(float** ppfPEmission);
    void compLogLikelihood_(float** ppfPEmission);

    enum MatrixDimension_t
    {
        kRow,
        kCol,

        kNumMatrixDimensions
    };

    bool m_bIsInitialized = false; //!< true if init has been called
    bool m_bWasProcessed = false; //!< true if compViterbi has been called

    float** m_ppfProb = 0; //!< probability matrix
    float** m_ppfTransProb = 0; //!< transition probability matrix
    float* m_pfStart = 0; //!< start probability vector
    float m_fOverallProb = -1e30F; //!< resulting overall path probability

    unsigned int  **m_ppiPathIdx = 0; //!< matrix with directions for traceback

    int m_iNumStates = 0; //!< number of states
    int m_iNumObs = 0; //!< number of observations
    int m_iEndState = -1; //!< most likely end state

    const float m_kLogMin = 1e-30F; //!< constant to avoid log(0)
};


#endif