#if !defined(__ACA_ToolConversion_HEADER_INCLUDED__)
#define __ACA_ToolConversion_HEADER_INCLUDED__


#include <cmath>
#include <cstdint>
#include <cassert>
#include <map>
#include <functional>

#include "ErrorDef.h"

/*! \brief class for various frequency conversion functions
*/
class CConversion
{
public:
    /*! \brief different methods for frequency to mel conversion */
    enum MelConversionFunctions_t
    {
        kFant,          //!< conversion acc. to Fant
        kShaughnessy,   //!< conversion acc. to Shaughnessy
        kUmesh,         //!< conversion acc. to Umesh

        kNumMelConversionFunctions
    };

    /*! \brief different methods for frequency to bark conversion */
    enum BarkConversionFunctions_t
    {
        kSchroeder,     //!< conversion acc. to Schroeder
        kTerhardt,      //!< conversion acc. to Terhardt
        kZwicker,       //!< conversion acc. to Zwicker
        kTraunmuller,   //!< conversion acc. to Traunmuller

        kNumBarkConversionFunctions
    };


    /*! converts a frequency scalar to a mel value
    \param fInHz frequency in Hz
    \param eFunc index for conversion function selection
    \return mel value
    */
    static float convertFreq2Mel(float fInHz, MelConversionFunctions_t eFunc = kFant);

    /*! converts a mel scalar to a frequency
    \param fMel mel value
    \param eFunc index for conversion function selection
    \return frequency value in Hz
    */
    static float convertMel2Freq(float fMel, MelConversionFunctions_t eFunc = kFant);

    /*! converts a frequency array to a mel array
    \param pfMel output mel values (length iLenghBuff, to be written)
    \param pffInHz input frequency values in Hz (length iLenghBuff)
    \param iLengthBuff length of buffers
    \param eFunc index for conversion function selection
    \return void
    */
    static void convertFreq2Mel(float *pfMel, const float *pffInHz, int iLengthBuff, MelConversionFunctions_t eFunc = kFant);

    /*! converts a mel array to a frequency array
    \param pffInHz output frequency values in Hz (length iLenghBuff, to be written)
    \param pfMel input mel values (length iLenghBuff)
    \param iLengthBuff length of buffers
    \param eFunc index for conversion function selection
    \return void
    */
    static void convertMel2Freq(float *pffInHz, const float *pfMel, int iLengthBuff, MelConversionFunctions_t eFunc = kFant);


    /*! converts a frequency scalar to a Bark value
    \param fInHz frequency in Hz
    \param eFunc index for conversion function selection
    \return Bark value
    */
    static float convertFreq2Bark(float fInHz, BarkConversionFunctions_t eFunc = kSchroeder);

    /*! converts a Bark scalar to a frequency
    \param fBark Bark value
    \param eFunc index for conversion function selection
    \return frequency value in Hz
    */
    static float convertBark2Freq(float fBark, BarkConversionFunctions_t eFunc = kSchroeder);

    /*! converts a frequency array to a Bark array
    \param pfBark output Bark values (length iLenghBuff, to be written)
    \param pffInHz input frequency values in Hz (length iLenghBuff)
    \param iLengthBuff length of buffers
    \param eFunc index for conversion function selection
    */
    static void convertFreq2Bark(float *pfBark, const float *pffInHz, int iLengthBuff, BarkConversionFunctions_t eFunc = kSchroeder);

    /*! converts a Bark array to a frequency array
    \param pffInHz output frequency values in Hz (length iLenghBuff, to be written)
    \param pfBark input Bark values (length iLenghBuff)
    \param iLengthBuff length of buffers
    \param eFunc index for conversion function selection
    */
    static void convertBark2Freq(float *pffInHz, const float *pfBark, int iLengthBuff, BarkConversionFunctions_t eFunc = kSchroeder);


    /*! converts a frequency scalar to midi (float)
    \param fInHz frequency value in Hz
    \param fA4InHz tuning (reference) frequency in Hz
    \return Midi value
    */
    static float convertFreq2Midi(float fInHz, float fA4InHz = 440.F)
    {
        assert(fInHz > 0);
        assert(fA4InHz > 0);
        return 69.F + 12.F * std::log2(fInHz / fA4InHz);
    };

    /*! converts a frequency vector to midi (float)
    \param pfMidi output buffer (length iLengthBuff, to be written)
    \param pffInHz input buffer with frequency values in Hz (length iLengthBuff)
    \param iLengthBuff length of buffers
    \param fA4InHz tuning (reference) frequency in Hz
    */
    static void convertFreq2Midi(float *pfMidi, const float *pffInHz, int iLengthBuff, float fA4InHz = 440.F)
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
        return fA4InHz * std::exp2((fMidi - 69.F) / 12.F);
    };

    /*! converts a midi vector to frequency
    \param pffInHz output buffer with frequency values in Hz (length iLengthBuff)
    \param pfMidi input buffer (length iLengthBuff, to be written)
    \param iLengthBuff length of buffers
    \param fA4InHz tuning (reference) frequency in Hz
    */
    static void convertMidi2Freq(float *pffInHz, const float *pfMidi, int iLengthBuff, float fA4InHz = 440.F)
    {
        assert(pfMidi);
        assert(pffInHz);
        assert(iLengthBuff > 0);
        assert(fA4InHz > 0);

        for (auto n = 0; n < iLengthBuff; n++)
            pffInHz[n] = convertMidi2Freq(pfMidi[n], fA4InHz);
    };


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
    */
    static void convertFreq2Bin(float *pfBin, const float *pffInHz, int iLengthBuff, int iFftLength, float fSampleRate = 44100.F)
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
    */
    static void convertBin2Freq(float *pffInHz, const float *pfBin, int iLengthBuff, int iFftLength, float fSampleRate = 44100.F);;

    /*! converts a float vector in a 32 bit word based on the sign
    \param pfVec vector of signed float values (length 32)
    \return uint32_t 32 bit word
    */
    static uint32_t convertFloat2Word(const float *pfVec)
    {
        assert(pfVec);

        int iLength = 32;
        uint32_t iRes = 0;

        for (auto b = 0; b < iLength; b++)
        {
            if (pfVec[b] > 0)
                iRes += (1 << (b + 1));
        }

        return iRes;
    }

    /*! converts a 32 bit word into a float vector of -1.F and 1.F
    \param pfDest destination buffer (length 32, to be written)
    \param iWord 32 bit word to be decoded
    */
    static void convertWord2Float(float *pfDest, uint32_t iWord)
    {
        assert(pfDest);

        int iLength = 32;

        for (auto b = 0; b < iLength; b++)
        {
            if (iWord & (1 << (b + 1)))
                pfDest[b] = 1.F;
            else
                pfDest[b] = -1.F;
        }
    }

protected:
    static float convertFreq2MelFant(float fFrequency);
    static float convertMel2FreqFant(float fMel);

    static float convertFreq2MelShaughnessy(float fFrequency);
    static float convertMel2FreqShaughnessy(float fMel);

    static float convertFreq2MelUmesh(float fFrequency);
    static float convertMel2FreqUmesh(float fMel);

    static float convertFreq2BarkSchroeder(float fFrequency);
    static float convertBark2FreqSchroeder(float fBark);

    static float convertFreq2BarkTerhardt(float fFrequency);
    static float convertBark2FreqTerhardt(float fBark);

    static float convertFreq2BarkZwicker(float fFrequency);
    static float convertBark2FreqZwicker(float fBark);

    static float convertFreq2BarkTraunmuller(float fFrequency);
    static float convertBark2FreqTraunmuller(float fBark);

private:
    CConversion() {};
    virtual ~CConversion() {};
    // dispatcher map for static mel functions 
    static const std::map<int, std::function<float(float)>> m_MelDispatchMap;
    // dispatcher map for static Bark functions 
    static const std::map<int, std::function<float(float)>> m_BarkDispatchMap;
};

#endif // __ACA_ToolConversion_HEADER_INCLUDED__
