
#include <cassert>
#include <cmath>

#include "ToolConversion.h"

const std::map<int, std::function<float(float)>> CConversion::m_DispatchMap
{
        {kFant, &convertFreq2MelFant},
        {kShaughnessy, &convertFreq2MelShaughnessy},
        {kUmesh, &convertFreq2MelUmesh},
        {kFant + kNumConversionFunctions, &convertMel2FreqFant},
        {kShaughnessy + kNumConversionFunctions, &convertMel2FreqShaughnessy},
        {kUmesh + kNumConversionFunctions, &convertMel2FreqUmesh}
};

float CConversion::convertFreq2Mel(float fInHz, MelConversionFunctions_t eFunc)
{
    assert(fInHz >= 0);
    return m_DispatchMap.at(eFunc)(fInHz);
}

/*! converts a mel scalar to a frequency
\param fMel mel value
\param eFunc index for conversion function selection
\return frequency value in Hz
*/

float CConversion::convertMel2Freq(float fMel, MelConversionFunctions_t eFunc)
{
    assert(fMel >= 0);
    return m_DispatchMap.at(eFunc + kNumConversionFunctions)(fMel);
}

/*! converts a frequency array to a mel array
\param pfMel output mel values (length iLenghBuff, to be written)
\param pffInHz input frequency values in Hz (length iLenghBuff)
\param iLengthBuff length of buffers
\return void
*/
void CConversion::convertFreq2Mel(float* pfMel, const float* pffInHz, int iLengthBuff, MelConversionFunctions_t eFunc)
{
    assert(pfMel);
    assert(pffInHz);
    assert(iLengthBuff > 0);

    for (auto k = 0; k < iLengthBuff; k++)
        pfMel[k] = m_DispatchMap.at(eFunc)(pffInHz[k]);
}

/*! converts a mel array to a frequency array
\param pffInHz output frequency values in Hz (length iLenghBuff, to be written)
\param pfMel input mel values (length iLenghBuff)
\param iLengthBuff length of buffers
\return void
*/

void CConversion::convertMel2Freq(float* pffInHz, const float* pfMel, int iLengthBuff, MelConversionFunctions_t eFunc)
{
    assert(pfMel);
    assert(pffInHz);
    assert(iLengthBuff > 0);

    for (auto k = 0; k < iLengthBuff; k++)
        pffInHz[k] = m_DispatchMap.at(eFunc + kNumConversionFunctions)(pfMel[k]);
}


/*! converts an FFT bin vector to a frequency vector
\param pffInHz frequency in Hz output buffer (length iLengthBuff, to be written)
\param pfBin input buffer with FFT bin indices (length iLengthBuff)
\param iLengthBuff length of buffers
\param iFftLength length of FFT
\param fSampleRate sample rate frequency in Hz
\return void
*/


void CConversion::convertBin2Freq(float* pffInHz, const float* pfBin, int iLengthBuff, int iFftLength, float fSampleRate)
{
    assert(pfBin);
    assert(pffInHz);
    assert(iLengthBuff > 0);
    assert(iFftLength >= 0);
    assert(fSampleRate > 0);

    for (auto n = 0; n < iLengthBuff; n++)
        pffInHz[n] = convertBin2Freq(pfBin[n], iFftLength, fSampleRate);
}

float CConversion::convertFreq2MelFant(float fFrequency)
{
    return 1000.F * std::log2(1.F + fFrequency / 1000.F);
}

float CConversion::convertMel2FreqFant(float fMel)
{
    return 1000.F * (std::exp2(fMel / 1000.F) - 1.F);
}

float CConversion::convertFreq2MelShaughnessy(float fFrequency)
{
    return 2595.F * std::log10(1.F + fFrequency / 700.F);
}

float CConversion::convertMel2FreqShaughnessy(float fMel)
{
    return 700.F * (std::pow(10.F, fMel / 2595.F) - 1.F);
}

float CConversion::convertFreq2MelUmesh(float fFrequency)
{
    return fFrequency / (2.4e-4F * fFrequency + 0.741F);
}

float CConversion::convertMel2FreqUmesh(float fMel)
{
    return  fMel * 0.741F / (1.F - fMel * 2.4e-4F);
}
