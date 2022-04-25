
#if !defined(__Dtw_HEADER_INCLUDED__)
#define __Dtw_HEADER_INCLUDED__

#pragma once

#include "ErrorDef.h"

/*! \brief simple dynamic time warping 
*/
class CDtw
{
public:
    enum MatrixDimension_t
    {
        kRow,
        kCol,

        kNumMatrixDimensions
    };

    CDtw(void);
    virtual ~CDtw(void);

    /*! initializes the class with the size of the distance matrix
    \param iNumRows
    \param iNumCols
    \return Error_t
    */
    Error_t init (int iNumRows, int iNumCols);
    
    /*! resets all internal class members
    \return Error_t
    */
    Error_t reset ();

    /*! computes cost and path w/o back-tracking
    \param ppfDistanceMatrix (dimensions [rows][columns])
    \return Error_t
    */
    Error_t process (float **ppfDistanceMatrix);
 
    /*! returns the length of the path
    \return int
    */
    int getPathLength ();
    
    /*! returns the overall cost
    \return float
    */
    float getPathCost () const;
    
    /*! returns the path \sa getPathLength
    \param ppiPathResult pointer to memory the result is written to (dimensions [2][length_of_path])
    \return Error_t
    */
    Error_t getPath (int **ppiPathResult) const;

private:
    enum Directions_t  
    {
        kHoriz,         //!< move horizontally
        kVert,          //!< move vertically
        kDiag,          //!< move diagonally

        kNumDirections
    };
    enum InternalMemory_t
    {
        kRowCurr,       //!< current row index (cost calculation)
        kRowNext,       //!< next row index (cost calculation)

        kNumVectors
    };

    static inline Directions_t findMinimum (float fHorizCost, float fVertCost, float fDiagCost, float &fResultCost) 
    {
        Directions_t eDirection = kDiag;
        fResultCost = fDiagCost;

        if (fHorizCost < fDiagCost)
        {
            eDirection  = kHoriz;
            fResultCost = fHorizCost;
        }
        if (fVertCost < fHorizCost && fVertCost < fDiagCost)
        {
            eDirection  = kVert;
            fResultCost = fVertCost;
        }

        return eDirection;
    }


    bool m_bIsInitialized;              //!< true if init has been called
    bool m_bWasProcessed;               //!< true if process has been called

    float *m_apfCost[kNumVectors];      //!< only allocate two rows instead of a whole matrix for the cost 
    float m_fOverallCost;               //!< overall cost
    unsigned char  **m_ppePathIdx;      //!< matrix with directions for traceback
    int   m_iLengthOfPath;              //!< overall length of path
    int   m_aiMatrixDimensions[kNumMatrixDimensions]; //!, size of distance matrix

    static const int aiDecrement[kNumDirections][kNumMatrixDimensions];
};


#endif
