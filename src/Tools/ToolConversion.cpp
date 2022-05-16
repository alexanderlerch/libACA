
#include <cassert>
#include <cmath>

#include "ToolConversion.h"

class CFant : public CFreq2Mel2Freq
{
public:
    CFant() {};
    virtual ~CFant() {};

    inline float convertFreq2Mel(float fFrequency) override
    {
        assert(fFrequency >= 0);
        return 1000.F * std::log2(1.F + fFrequency / 1000.F);
    }
    inline float convertMel2Freq(float fMel) override
    {
        assert(fMel >= 0);
        return 1000.F * (std::exp2(fMel / 1000.F) - 1.F);
    }
};

class CShaughnessy : public CFreq2Mel2Freq
{
public:
    CShaughnessy() {};
    virtual ~CShaughnessy() {};

    inline float convertFreq2Mel(float fFrequency) override
    {
        assert(fFrequency >= 0);
        return 2595.F * std::log10(1.F + fFrequency / 700.F);
    }
    inline float convertMel2Freq(float fMel) override
    {
        assert(fMel >= 0);
        return 700.F * (std::pow(10.F, fMel / 2595.F) - 1.F);

    }
};
class CUmesh : public CFreq2Mel2Freq
{
public:
    CUmesh() {};
    virtual ~CUmesh() {};

    inline float convertFreq2Mel(float fFrequency) override
    {
        assert(fFrequency >= 0);
        return fFrequency / (2.4e-4F * fFrequency + 0.741F);
    }
    inline float convertMel2Freq(float fMel) override
    {
        assert(fMel >= 0);
        return  fMel * 0.741F / (1.F - fMel * 2.4e-4F);

    }
};

Error_t CFreq2Mel2Freq::create(CFreq2Mel2Freq*& pCInstance, ConversionFunctions_t eConversion)
{
    switch (eConversion)
    {
    default:
    case kFant:
        pCInstance = new CFant();
        break;
    case kShaughnessy:
        pCInstance = new CShaughnessy();
        break;
    case kUmesh:
        pCInstance = new CUmesh();
        break;
    }
    return Error_t::kNoError;
}

Error_t CFreq2Mel2Freq::destroy(CFreq2Mel2Freq*& pCInstance)
{
    delete pCInstance;
    pCInstance = 0;

    return Error_t::kNoError;
}
void CFreq2Mel2Freq::convertFreq2Mel(float* pfMel, const float* pfFrequency, int iLengthBuff) 
{
    assert(pfMel);
    assert(pfFrequency);
    assert(iLengthBuff > 0);

    for (auto k = 0; k < iLengthBuff; k++)
        pfMel[k] = convertFreq2Mel(pfFrequency[k]);
}
void CFreq2Mel2Freq::convertMel2Freq(float* pfFrequency, const float* pfMel, int iLengthBuff) 
{
    assert(pfMel);
    assert(pfFrequency);
    assert(iLengthBuff > 0);

    for (auto k = 0; k < iLengthBuff; k++)
        pfFrequency[k] = convertMel2Freq(pfMel[k]);
}
