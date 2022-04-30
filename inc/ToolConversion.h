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
    virtual void convertFreq2Mel(float* pfMel, const float* pfFrequency, int iLengthBuff);
    virtual float convertMel2Freq(float fMel) = 0;
    virtual void convertMel2Freq(float* pfFrequency, const float* pfMel, int iLengthBuff);

protected:
    CFreq2Mel2Freq() {};
    virtual ~CFreq2Mel2Freq() {};
};

class CFreq2Midi2Freq
{
public:
    static float convertFreq2Midi(float fFrequency, float fA4InHz = 440.F)
    {
        assert(fFrequency > 0);
        assert(fA4InHz > 0);
        return 69.F + 12.F * std::log2f(fFrequency / fA4InHz);
    };
    static void convertFreq2Midi(float* pfMidi, const float* pfFrequency, int iLengthBuff, float fA4InHz = 440.F)
    {
        assert(pfMidi);
        assert(pfFrequency);
        assert(iLengthBuff > 0);
        assert(fA4InHz > 0);

        for (auto n = 0; n < iLengthBuff; n++)
            pfMidi[n] = convertFreq2Midi(pfFrequency[n], fA4InHz);
    };
    static float convertMidi2Freq(float fMidi, float fA4InHz = 440.F)
    {
        assert(fMidi >= 0);
        assert(fA4InHz > 0);
        return fA4InHz * std::exp2f((fMidi - 69.F) / 12.F);
    };
    static void convertMidi2Freq(float* pfFrequency, const float* pfMidi, int iLengthBuff, float fA4InHz = 440.F)
    {
        assert(pfMidi);
        assert(pfFrequency);
        assert(iLengthBuff > 0);
        assert(fA4InHz > 0);

        for (auto n = 0; n < iLengthBuff; n++)
            pfFrequency[n] = convertMidi2Freq(pfMidi[n], fA4InHz);
    };

private:
    CFreq2Midi2Freq() {};
    virtual ~CFreq2Midi2Freq() {};
};
class CFreq2Bin2Freq
{
public:
    static float convertFreq2Bin(float fFrequency, int iFftLength, float fSampleRate = 44100.F)
    {
        assert(fFrequency >= 0);
        assert(iFftLength >= 0);
        assert(fFrequency < fSampleRate);
        assert(fSampleRate > 0);

        return fFrequency / fSampleRate * static_cast<float>(iFftLength);
    };
    static void convertFreq2Bin(float* pfMidi, const float* pfFrequency, int iLengthBuff, int iFftLength, float fSampleRate = 44100.F)
    {
        assert(pfMidi);
        assert(pfFrequency);
        assert(iLengthBuff > 0);
        assert(iFftLength >= 0);
        assert(fSampleRate > 0);

        for (auto n = 0; n < iLengthBuff; n++)
            pfMidi[n] = convertFreq2Bin(pfFrequency[n], iFftLength, fSampleRate);
    };
    static float convertBin2Freq(float fBin, int iFftLength, float fSampleRate = 44100.F)
    {
        assert(fBin >= 0);
        assert(iFftLength >= 0);
        assert(fBin < iFftLength);
        assert(fSampleRate > 0);
        
        return fBin * fSampleRate / static_cast<float>(iFftLength);
    };
    static void convertBin2Freq(float* pfFrequency, const float* pfMidi, int iLengthBuff, int iFftLength, float fSampleRate = 44100.F)
    {
        assert(pfMidi);
        assert(pfFrequency);
        assert(iLengthBuff > 0);
        assert(iFftLength >= 0);
        assert(fSampleRate > 0);

        for (auto n = 0; n < iLengthBuff; n++)
            pfFrequency[n] = convertBin2Freq(pfMidi[n], iFftLength, fSampleRate);
    };

private:
    CFreq2Bin2Freq() {};
    virtual ~CFreq2Bin2Freq() {};
};


#endif // __ToolConversion_hdr__