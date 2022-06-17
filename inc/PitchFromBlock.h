#if !defined(__ACA_PitchFromBlock_HEADER_INCLUDED__)
#define __ACA_PitchFromBlock_HEADER_INCLUDED__

#include "ErrorDef.h"

#include "Pitch.h"


/*! \brief class for computation of f0 from a block of data (e.g., time or magnitude spectrum)
*/
class CPitchFromBlockIf
{
public:

    /*! initializes a PitchFromBlock instance
    \param pCInstance pointer to instance to be written
    \param ePitchIdx index of Pitch to extract
    \param iDataLength: block length
    \param fSampleRate: sample rate (only used when needed)
    \return Error_t
    */
    static Error_t create(CPitchFromBlockIf*& pCInstance, CPitchIf::PitchExtractors_t ePitchIdx, int iDataLength, float fSampleRate = 1.F);

    /*! destroys a PitchFromBlock instance
    \param pCInstance pointer to instance to be destroyed
    \return Error_t
    */
    static Error_t destroy(CPitchFromBlockIf*& pCInstance);

    /*! returns index of the Pitch to extract
    \return Pitch_t
    */
    CPitchIf::PitchExtractors_t getPitchExtractorIdx() const
    {
        return m_ePitchIdx;
    }

    /*! performs the PitchFromBlock computation
    \param pfIn input data of length iDataLength
    \return float fF0InHz
    */
    virtual float compF0(const float* pfIn) = 0;

protected:
    CPitchFromBlockIf() {};
    CPitchFromBlockIf(CPitchIf::PitchExtractors_t ePitchIdx, int iDataLength, float fSampleRate);;
    virtual ~CPitchFromBlockIf() {};
    CPitchFromBlockIf(const CPitchFromBlockIf& that);
    CPitchFromBlockIf& operator=(const CPitchFromBlockIf& c);

    CPitchIf::PitchExtractors_t m_ePitchIdx = CPitchIf::kNumPitchExtractors; //!< index of Pitch to extract

    int m_iDataLength = 0; //!< block length

    float m_fSampleRate = 0; //!< sample rate
};

#endif // #if !defined(__ACA_PitchFromBlock_HEADER_INCLUDED__)
