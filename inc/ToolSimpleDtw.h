#if !defined(__ACA_Dtw_HEADER_INCLUDED__)
#define __ACA_Dtw_HEADER_INCLUDED__

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
    Error_t init(int iNumRows, int iNumCols);

    /*! resets all internal class members
    \return Error_t
    */
    Error_t reset();

    /*! computes cost and path w/o back-tracking
    \param ppfDistanceMatrix (dimensions [rows][columns])
    \return Error_t
    */
    Error_t compDtw(const float *const *const ppfDistanceMatrix);

    /*! returns the length of the path
    \return int
    */
    int getPathLength();

    /*! returns the overall cost
    \return float
    */
    float getPathCost() const;

    /*! returns the path \sa getPathLength
    \param ppiPathResult pointer to memory the result is written to (dimensions [2][length_of_path])
    \return Error_t
    */
    Error_t getPath(int **ppiPathResult) const;

private:
    CDtw(const CDtw &that);
    CDtw &operator=(const CDtw &c);

    enum Directions_t
    {
        kHoriz, //!< move horizontally
        kVert, //!< move vertically
        kDiag, //!< move diagonally

        kNumDirections
    };
    enum InternalMemory_t
    {
        kRowCurr, //!< current row index (cost calculation)
        kRowNext, //!< next row index (cost calculation)

        kNumVectors
    };

    /*! returns the direction of the minimum
    \param fHorizCost prev cost (horizontal)
    \param fVertCost prev cost (vertical)
    \param fDiagCost prev cost (diag)
    \param fResultCost cost of the result
    \return Directions_t
    */
    static inline Directions_t findMin_(float fHorizCost, float fVertCost, float fDiagCost, float &fResultCost)
    {
        Directions_t eDirection = kDiag;
        fResultCost = fDiagCost;

        if (fHorizCost < fDiagCost)
        {
            eDirection = kHoriz;
            fResultCost = fHorizCost;
        }
        if (fVertCost < fHorizCost && fVertCost < fDiagCost)
        {
            eDirection = kVert;
            fResultCost = fVertCost;
        }

        return eDirection;
    }

    float *m_apfCost[kNumVectors] = { 0,0 }; //!< only allocate two rows instead of a whole matrix for the cost 
    float m_fOverallCost = 0; //!< overall cost

    unsigned char **m_ppePathIdx = 0; //!< matrix with directions for traceback
    int   m_iPathLength = 0; //!< overall length of path

    int   m_aiMatrixDims[kNumMatrixDimensions] = { 0,0 }; //!< size of distance matrix

    static const int aiDecrement[kNumDirections][kNumMatrixDimensions]; //!< index changes for directions

    bool m_bIsInitialized = false; //!< true if init has been called
    bool m_bWasProcessed = false; //!< true if compDtw has been called
};

#endif // __ACA_Dtw_HEADER_INCLUDED__
