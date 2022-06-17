
#include <cassert>
#include <cmath>

#include "ToolConversion.h"

//////////////////////////////////////////////////////////////////////////////
const std::map<int, std::function<float(float)>> CConversion::m_MelDispatchMap
{
        {kFant, &convertFreq2MelFant},
        {kShaughnessy, &convertFreq2MelShaughnessy},
        {kUmesh, &convertFreq2MelUmesh},
        {kFant + kNumMelConversionFunctions, &convertMel2FreqFant},
        {kShaughnessy + kNumMelConversionFunctions, &convertMel2FreqShaughnessy},
        {kUmesh + kNumMelConversionFunctions, &convertMel2FreqUmesh}
};

float CConversion::convertFreq2Mel(float fInHz, MelConversionFunctions_t eFunc)
{
    assert(fInHz >= 0);
    return m_MelDispatchMap.at(eFunc)(fInHz);
}

float CConversion::convertMel2Freq(float fMel, MelConversionFunctions_t eFunc)
{
    assert(fMel >= 0);
    return m_MelDispatchMap.at(eFunc + kNumMelConversionFunctions)(fMel);
}

void CConversion::convertFreq2Mel(float *pfMel, const float *pffInHz, int iLengthBuff, MelConversionFunctions_t eFunc)
{
    assert(pfMel);
    assert(pffInHz);
    assert(iLengthBuff > 0);

    for (auto k = 0; k < iLengthBuff; k++)
        pfMel[k] = m_MelDispatchMap.at(eFunc)(pffInHz[k]);
}

void CConversion::convertMel2Freq(float *pffInHz, const float *pfMel, int iLengthBuff, MelConversionFunctions_t eFunc)
{
    assert(pfMel);
    assert(pffInHz);
    assert(iLengthBuff > 0);

    for (auto k = 0; k < iLengthBuff; k++)
        pffInHz[k] = m_MelDispatchMap.at(eFunc + kNumMelConversionFunctions)(pfMel[k]);
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

//////////////////////////////////////////////////////////////////////////////
const std::map<int, std::function<float(float)>> CConversion::m_BarkDispatchMap
{
        {kSchroeder, &convertFreq2BarkSchroeder},
        {kTerhardt, &convertFreq2BarkTerhardt},
        {kZwicker, &convertFreq2BarkZwicker},
        {kTraunmuller, &convertFreq2BarkTraunmuller},
        {kSchroeder + kNumBarkConversionFunctions, &convertBark2FreqSchroeder},
        {kTerhardt + kNumBarkConversionFunctions, &convertBark2FreqTerhardt},
        {kZwicker + kNumBarkConversionFunctions, &convertBark2FreqZwicker},
        {kTraunmuller + kNumBarkConversionFunctions,&convertBark2FreqTraunmuller}
};

float CConversion::convertFreq2Bark(float fInHz, BarkConversionFunctions_t eFunc)
{
    assert(fInHz >= 0);
    return m_BarkDispatchMap.at(eFunc)(fInHz);
}

float CConversion::convertBark2Freq(float fBark, BarkConversionFunctions_t eFunc)
{
    assert(fBark >= 0);
    return m_BarkDispatchMap.at(eFunc + kNumBarkConversionFunctions)(fBark);
}

void CConversion::convertFreq2Bark(float *pfBark, const float *pffInHz, int iLengthBuff, BarkConversionFunctions_t eFunc)
{
    assert(pfBark);
    assert(pffInHz);
    assert(iLengthBuff > 0);

    for (auto k = 0; k < iLengthBuff; k++)
        pfBark[k] = m_BarkDispatchMap.at(eFunc)(pffInHz[k]);
}

void CConversion::convertBark2Freq(float *pffInHz, const float *pfBark, int iLengthBuff, BarkConversionFunctions_t eFunc)
{
    assert(pfBark);
    assert(pffInHz);
    assert(iLengthBuff > 0);

    for (auto k = 0; k < iLengthBuff; k++)
        pffInHz[k] = m_BarkDispatchMap.at(eFunc + kNumBarkConversionFunctions)(pfBark[k]);
}


float CConversion::convertFreq2BarkSchroeder(float fFrequency)
{
    assert(fFrequency > 0);
    return 7.F * std::asinh(fFrequency / 650.F);
}
float CConversion::convertBark2FreqSchroeder(float fBark)
{
    assert(fBark > 0);
    return 650.F * std::sinh(fBark / 7.F);
}

float CConversion::convertFreq2BarkTerhardt(float fFrequency)
{
    assert(fFrequency > 0);
    return 13.3F * std::atan(0.75F * fFrequency / 1000.F);
}
float CConversion::convertBark2FreqTerhardt(float fBark)
{
    assert(fBark > 0);
    return 4.F / 3.F * 1000.F * std::tan(fBark / 13.3F);
}

float CConversion::convertFreq2BarkZwicker(float fFrequency)
{
    assert(fFrequency > 0);
    return 13.F * std::atan(0.76F * fFrequency / 1000.F) + 3.5F * std::atan(fFrequency / 7500.F);
}
float CConversion::convertBark2FreqZwicker(float /*fBark*/)
{
    return -1.F;
}

float CConversion::convertFreq2BarkTraunmuller(float fFrequency)
{
    assert(fFrequency > 0);
    return 26.81F / (1.F + 1960.F / fFrequency) - 0.53F;
}
float CConversion::convertBark2FreqTraunmuller(float fBark)
{
    assert(fBark > 0);
    //return 1960.F * (fBark + 0.53F) / (26.81F - fBark);
    return 1960.F / (26.81F / (fBark + 0.53F) - 1.F);
}

//////////////////////////////////////////////////////////////////////////////
void CConversion::convertBin2Freq(float *pffInHz, const float *pfBin, int iLengthBuff, int iFftLength, float fSampleRate)
{
    assert(pfBin);
    assert(pffInHz);
    assert(iLengthBuff > 0);
    assert(iFftLength >= 0);
    assert(fSampleRate > 0);

    for (auto n = 0; n < iLengthBuff; n++)
        pffInHz[n] = convertBin2Freq(pfBin[n], iFftLength, fSampleRate);
}
