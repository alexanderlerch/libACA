#if !defined(__ACA_KeyFromChroma_HEADER_INCLUDED__)
#define __ACA_KeyFromChroma_HEADER_INCLUDED__

#pragma once

#include "ErrorDef.h"

// forward declaration
template <class T> class CRingBuffer;

/*! \brief computation of key profile distance
*/
class CKeyFromChroma
{
public:
    CKeyFromChroma(void);;
    virtual ~CKeyFromChroma(void);;

    /*! classifies a new query chroma vector
    \return int index of most likely key
    */
    int getKey(const float *pfQuery);


private:
    enum Modes_t
    {
        kMajor,
        kMinor,

        kNumModes
    };
    enum Pitches_t
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
    CKeyFromChroma(const CKeyFromChroma &that);     //!< disallow copy construction   
    CKeyFromChroma &operator=(const CKeyFromChroma &c);

    const float m_aafKeyProfiles[kNumModes][kNumPitchClasses] = { {6.35F, 2.23F, 3.48F, 2.33F, 4.38F, 4.09F, 2.52F, 5.19F, 2.39F, 3.66F, 2.29F, 2.88F},  //!< krumhansl templates
                                                                  {6.33F, 2.68F, 3.52F, 5.38F, 2.60F, 3.53F, 2.54F, 4.75F, 3.98F, 2.69F, 3.34F, 3.17F} };

    CRingBuffer<float> *m_apCRingBuff[kNumModes] = { 0 }; //!< ringbuffer for cyclic shifting of the templates
};

#endif // __ACA_KeyFromChroma_HEADER_INCLUDED__
