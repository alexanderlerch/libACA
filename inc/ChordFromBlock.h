#if !defined(__ACA_ChordFromBlock_HEADER_INCLUDED__)
#define __ACA_ChordFromBlock_HEADER_INCLUDED__
#include <map>

#include "ErrorDef.h"

// forward declarations
class CFeatureFromBlockIf;

/*! \brief class for computation of a chord from a magnitude spectrum
*/
class CChordFromBlockIf
{
public:

    /*! initializes a ChordFromBlock instance
    \param pCInstance pointer to instance to be written
    \param iMagSpecLength: block length
    \param fSampleRate: sample rate
    \return Error_t
    */
    static Error_t create(CChordFromBlockIf *&pCInstance, int iMagSpecLength, float fSampleRate = 1.F);

    /*! destroys a ChordFromBlock instance
    \param pCInstance pointer to instance to be destroyed
    \return Error_t
    */
    static Error_t destroy(CChordFromBlockIf *&pCInstance);

    /*! calculates the chord probabilities
    \param pfChordProb resulting chord probabilities (user allocated, length kNumChords)
    \param pfIn input data of length iMagSpecLength
    \return Error_t
    */
    virtual Error_t compChordProb(float *pfChordProb, const float *pfIn);

protected:
    enum PitchClasses_t
    {
        kC,
        kCs,
        kD,
        kDs,
        kE,
        kF,
        kFs,
        kG,
        kGs,
        kA,
        kAs,
        kB,

        kNumPitchClasses

    };
    explicit CChordFromBlockIf(int iMagSpecLength, float fSampleRate);
    virtual ~CChordFromBlockIf();
    CChordFromBlockIf(const CChordFromBlockIf &that);
    CChordFromBlockIf &operator=(const CChordFromBlockIf &c);

    void genTemplateMatrix_(); //!< chord templates for this similarity based chord detection

    int m_iMagSpecLength = 0;  //!< block length
    float m_fSampleRate = 0; //!< sample rate

    float *m_pfPitchChroma = 0; //!< current pitch chroma

    float **m_ppfTemplateMatrix = 0; //!< matrix holding all chord templates

    CFeatureFromBlockIf *m_pCFeatureExtractor = 0; //!< instance for pitch chroma extraction
};

#endif // #if !defined(__ACA_ChordFromBlock_HEADER_INCLUDED__)
