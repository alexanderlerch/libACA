#if !defined(__ToolConversion_hdr__)
#define __ToolConversion_hdr__

#include "ErrorDef.h"

/*! \brief class frequency to mel and mel to frequency conversion functions (requires instantiation)
*/
class CFreq2Mel2Freq
{
public:
    enum ConversionFunctions_t
    {
        kFant,          //!< conversion acc. to Fant
        kShaughnessy,   //!< conversion acc. to Shaughnessy
        kUmesh,         //!< conversion acc. to Umesh

        kNumConversionFunctions
    };

    /*! creates a new instance for conversion
    \param pCInstance pointer to new instance (to be written)
    \param eConversion flag for the conversion function to use
    \return Error_t
    */
    static Error_t create(CFreq2Mel2Freq*& pCInstance, ConversionFunctions_t eConversion = kFant);

    /*! destroys an instance 
    \param pCInstance pointer to instance to be destroyed (set to zero)
    \return Error_t
    */
    static Error_t destroy(CFreq2Mel2Freq*& pCInstance);

    /*! converts a frequency scalar to a mel value
    \param fInHz frequency in Hz
    \return mel value
    */
    virtual float convertFreq2Mel(float fInHz) = 0;

    /*! converts a frequency array to a mel array
    \param pfMel output mel values (length iLenghBuff, to be written)
    \param pffInHz input frequency values in Hz (length iLenghBuff)
    \param iLengthBuff length of buffers
    \return void
    */
    virtual void convertFreq2Mel(float* pfMel, const float* pffInHz, int iLengthBuff);

    /*! converts a mel scalar to a frequency
    \param fMel mel value
    \return frequency value in Hz
    */
    virtual float convertMel2Freq(float fMel) = 0;

    /*! converts a mel array to a frequency array
    \param pffInHz output frequency values in Hz (length iLenghBuff, to be written)
    \param pfMel input mel values (length iLenghBuff)
    \param iLengthBuff length of buffers
    \return void
    */
    virtual void convertMel2Freq(float* pffInHz, const float* pfMel, int iLengthBuff);

protected:
    CFreq2Mel2Freq() {};
    virtual ~CFreq2Mel2Freq() {};
};

/*! \brief class frequency to midi and midi to frequency conversion functions (does not require instantiation)
*/
class CFreq2Midi2Freq
{
public:

    /*! converts a frequency scalar to midi (float)
    \param fInHz frequency value in Hz
    \param fA4InHz tuning (reference) frequency in Hz
    \return Midi value
    */
    static float convertFreq2Midi(float fInHz, float fA4InHz = 440.F)
    {
        assert(fInHz > 0);
        assert(fA4InHz > 0);
        return 69.F + 12.F * std::log2f(fInHz / fA4InHz);
    };

    /*! converts a frequency vector to midi (float)
    \param pfMidi output buffer (length iLengthBuff, to be written)
    \param pffInHz input buffer with frequency values in Hz (length iLengthBuff)
    \param iLengthBuff length of buffers
    \param fA4InHz tuning (reference) frequency in Hz
    \return void 
    */
    static void convertFreq2Midi(float* pfMidi, const float* pffInHz, int iLengthBuff, float fA4InHz = 440.F)
    {
        assert(pfMidi);
        assert(pffInHz);
        assert(iLengthBuff > 0);
        assert(fA4InHz > 0);

        for (auto n = 0; n < iLengthBuff; n++)
            pfMidi[n] = convertFreq2Midi(pffInHz[n], fA4InHz);
    };

    /*! converts a midi scalar to frequency 
    \param fMidi value 
    \param fA4InHz tuning (reference) frequency in Hz
    \return frequency value in Hz
    */
    static float convertMidi2Freq(float fMidi, float fA4InHz = 440.F)
    {
        assert(fMidi >= 0);
        assert(fA4InHz > 0);
        return fA4InHz * std::exp2f((fMidi - 69.F) / 12.F);
    };

    /*! converts a midi vector to frequency 
    \param pffInHz output buffer with frequency values in Hz (length iLengthBuff)
    \param pfMidi input buffer (length iLengthBuff, to be written)
    \param iLengthBuff length of buffers
    \param fA4InHz tuning (reference) frequency in Hz
    \return void
    */
    static void convertMidi2Freq(float* pffInHz, const float* pfMidi, int iLengthBuff, float fA4InHz = 440.F)
    {
        assert(pfMidi);
        assert(pffInHz);
        assert(iLengthBuff > 0);
        assert(fA4InHz > 0);

        for (auto n = 0; n < iLengthBuff; n++)
            pffInHz[n] = convertMidi2Freq(pfMidi[n], fA4InHz);
    };

private:
    CFreq2Midi2Freq() {};
    virtual ~CFreq2Midi2Freq() {};
};

/*! \brief class frequency to FFT bin index and FFT bin index to frequency conversion functions (does not require instantiation)
*/
class CFreq2Bin2Freq
{
public:

    /*! converts a frequency scalar to an FFT bin index
    \param fInHz frequency value in Hz
    \param iFftLength length of FFT
    \param fSampleRate sample rate frequency in Hz
    \return bin index (float)
    */
    static float convertFreq2Bin(float fInHz, int iFftLength, float fSampleRate = 44100.F)
    {
        assert(fInHz >= 0);
        assert(iFftLength >= 0);
        assert(fInHz < fSampleRate);
        assert(fSampleRate > 0);

        return fInHz / fSampleRate * static_cast<float>(iFftLength);
    };

    /*! converts a frequency vector into a FFT bin vector
    \param pfBin output buffer with FFT bin indices (length iLengthBuff, to be written)
    \param pffInHz frequency in Hz input buffer (length iLengthBuff)
    \param iLengthBuff length of buffers
    \param iFftLength length of FFT
    \param fSampleRate sample rate frequency in Hz
    \return void
    */
    static void convertFreq2Bin(float* pfBin, const float* pffInHz, int iLengthBuff, int iFftLength, float fSampleRate = 44100.F)
    {
        assert(pfBin);
        assert(pffInHz);
        assert(iLengthBuff > 0);
        assert(iFftLength >= 0);
        assert(fSampleRate > 0);

        for (auto n = 0; n < iLengthBuff; n++)
            pfBin[n] = convertFreq2Bin(pffInHz[n], iFftLength, fSampleRate);
    };

    /*! converts a FFT bin index to a frequency scalar
    \param fBin FFT bin index
    \param iFftLength length of FFT
    \param fSampleRate sample rate frequency in Hz
    \return frequency in Hz
    */
    static float convertBin2Freq(float fBin, int iFftLength, float fSampleRate = 44100.F)
    {
        assert(fBin >= 0);
        assert(iFftLength >= 0);
        assert(fBin < iFftLength);
        assert(fSampleRate > 0);
        
        return fBin * fSampleRate / static_cast<float>(iFftLength);
    };

    /*! converts an FFT bin vector to a frequency vector
    \param pffInHz frequency in Hz output buffer (length iLengthBuff, to be written)
    \param pfBin input buffer with FFT bin indices (length iLengthBuff)
    \param iLengthBuff length of buffers
    \param iFftLength length of FFT
    \param fSampleRate sample rate frequency in Hz
    \return void
    */
    static void convertBin2Freq(float* pffInHz, const float* pfBin, int iLengthBuff, int iFftLength, float fSampleRate = 44100.F)
    {
        assert(pfBin);
        assert(pffInHz);
        assert(iLengthBuff > 0);
        assert(iFftLength >= 0);
        assert(fSampleRate > 0);

        for (auto n = 0; n < iLengthBuff; n++)
            pffInHz[n] = convertBin2Freq(pfBin[n], iFftLength, fSampleRate);
    };

private:
    CFreq2Bin2Freq() {};
    virtual ~CFreq2Bin2Freq() {};
};


#endif // __ToolConversion_hdr__