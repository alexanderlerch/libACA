#if !defined(__ToolConversion_hdr__)
#define __ToolConversion_hdr__

#include "ErrorDef.h"

/*! \brief class frequency to mel and mel to frequency conversion functions
*/
class CFreq2Mel2Freq
{
public:
    enum ConversionFunctions_t
    {
        kFant,
        kShaughnessy,
        kUmesh,

        kNumConversionFunctions
    };

    static Error_t create(CFreq2Mel2Freq*& pCInstance, ConversionFunctions_t eConversion = kFant);
    static Error_t destroy(CFreq2Mel2Freq*& pCInstance);
    virtual float convertFreq2Mel(float fFrequency) = 0;
    virtual void convertFreq2Mel(float* pfMel, const float *pfFrequency, int iLengthBuff);
    virtual float convertMel2Freq(float fMel) = 0;
    virtual void convertMel2Freq(float *pfFrequency, const float *pfMel, int iLengthBuff);

protected:
    CFreq2Mel2Freq() {};
    virtual ~CFreq2Mel2Freq() {};
 };


#endif // __ToolConversion_hdr__