
#include "Vector.h"
#include "Util.h"

#include "ToolConversion.h"
#include "ToolCcf.h"
#include "ToolLowPass.h"

#include "PitchFromBlock.h"

//const float CPitchFromBlockIf::m_kfFloatThresh = 1e-30F;      //!< below this we just assume it's zero

//////////////////////////////////////////////////////////////////////////
//// static member functions
//float CPitchFromBlockIf::compPitchSpectralAcf(const float* pfMagSpec, int iDataLength, float fSampleRate /*= 1.F*/)
//{
//    assert(pfMagSpec);
//    assert(iDataLength > 0);
//    assert(fSampleRate > 0);
//
//}
//
//float CPitchFromBlockIf::compPitchSpectralHps(const float* pfMagSpec, int iDataLength, float /*fSampleRate = 1.F*/)
//{
//    assert(pfMagSpec);
//    assert(iDataLength > 0);
//
//}
//float CPitchFromBlockIf::compPitchTimeAcf(const float* pfSamples, int iDataLength, float /*fSampleRate = 1.F*/)
//{
//    assert(pfSamples);
//    assert(iDataLength > iEta);
//    assert(iEta >= 0);
//
//}
//
//float CPitchFromBlockIf::compPitchTimeAmdf(const float* pfSamples, int iDataLength, float /*fSampleRate = 1.F*/)
//{
//    assert(pfSamples);
//    assert(iDataLength > 0);
//}
//
//float CPitchFromBlockIf::compPitchTimeAuditory(const float* pfSamples, int iDataLength, float /*fSampleRate = 1.F*/)
//{
//    assert(pfSamples);
//    assert(iDataLength > 0);
//
//}
//
//float CPitchFromBlockIf::compPitchTimeZeroCrossings(const float* pfSamples, int iDataLength, float /*fSampleRate = 1.F*/)
//{
//    assert(pfSamples);
//    assert(iDataLength > 0);
//
//}


///////////////////////////////////////////////////////////////////
// Pitchs that need "memory" so can't easily work as static functions
class CPitchSpectralAcf : public CPitchFromBlockIf
{
public:
    CPitchSpectralAcf(PitchExtractors_t ePitchIdx, int iDataLength, float fSampleRate) : CPitchFromBlockIf(ePitchIdx, iDataLength, fSampleRate)
    {
    };

    virtual ~CPitchSpectralAcf()
    {
    };

    Error_t compPitch(float* pfPitch, const float* pfInput) override
    {
        assert(pfPitch);
        assert(pfInput);
        return Error_t::kNoError;
    };

private:
    CPitchSpectralAcf() {};
    CPitchSpectralAcf(const CPitchSpectralAcf& that);     //!< disallow copy construction
    CPitchSpectralAcf& operator=(const CPitchSpectralAcf& c);
};

class CPitchSpectralHps : public CPitchFromBlockIf
{
public:
    CPitchSpectralHps(PitchExtractors_t ePitchIdx, int iDataLength, float fSampleRate) : CPitchFromBlockIf(ePitchIdx, iDataLength, fSampleRate)
    {
    }

    virtual ~CPitchSpectralHps()
    {
    }

    Error_t compPitch(float* pfPitch, const float* pfInput) override
    {
        assert(pfPitch);
        assert(pfInput);


         return Error_t::kNoError;
    };
private:
    CPitchSpectralHps() {};
    CPitchSpectralHps(const CPitchSpectralHps& that);     //!< disallow copy construction  
    CPitchSpectralHps& operator=(const CPitchSpectralHps& c);


};
class CPitchTimeAcf : public CPitchFromBlockIf
{
public:
    CPitchTimeAcf(PitchExtractors_t ePitchIdx, int iDataLength, float fSampleRate) : CPitchFromBlockIf(ePitchIdx, iDataLength, fSampleRate)
    {
    };

    virtual ~CPitchTimeAcf()
    {
    };

    Error_t compPitch(float* pfPitch, const float* pfInput) override
    {
        assert(pfPitch);
        assert(pfInput);

 
        return Error_t::kNoError;
    };


private:
    CPitchTimeAcf() {};
    CPitchTimeAcf(const CPitchTimeAcf& that);     //!< disallow copy construction
    CPitchTimeAcf& operator=(const CPitchTimeAcf& c);

};

class CPitchTimeAmdf : public CPitchFromBlockIf
{
public:
    CPitchTimeAmdf(PitchExtractors_t ePitchIdx, int iDataLength, float fSampleRate) : CPitchFromBlockIf(ePitchIdx, iDataLength, fSampleRate) {};

    virtual ~CPitchTimeAmdf() {};

    Error_t compPitch(float* pfPitch, const float* pfInput) override
    {
        assert(pfPitch);
        assert(pfInput);

        return Error_t::kNoError;
    };

private:
    CPitchTimeAmdf() {};

};

class CPitchTimeAuditory : public CPitchFromBlockIf
{
public:
    CPitchTimeAuditory(PitchExtractors_t ePitchIdx, int iDataLength, float fSampleRate) : CPitchFromBlockIf(ePitchIdx, iDataLength, fSampleRate) {};

    virtual ~CPitchTimeAuditory() {};

    Error_t compPitch(float* pfPitch, const float* pfInput) override
    {
        assert(pfPitch);
        assert(pfInput);

        return Error_t::kNoError;
    };


private:
    CPitchTimeAuditory() {};
};

class CPitchTimeZeroCrossings : public CPitchFromBlockIf
{
public:
    CPitchTimeZeroCrossings(PitchExtractors_t ePitchIdx, int iDataLength, float fSampleRate) : CPitchFromBlockIf(ePitchIdx, iDataLength, fSampleRate) {};

    virtual ~CPitchTimeZeroCrossings() {};

    Error_t compPitch(float* pfPitch, const float* pfInput) override
    {
        assert(pfPitch);
        assert(pfInput);

        return Error_t::kNoError;
    };

private:
    CPitchTimeZeroCrossings() {};
};

///////////////////////////////////////////////////////////////////
// normal member functions
Error_t CPitchFromBlockIf::create(CPitchFromBlockIf*& pCInstance, PitchExtractors_t ePitchIdx, int iDataLength, float fSampleRate)
{
    if (iDataLength <= 0 || fSampleRate <= 0)
        return Error_t::kFunctionInvalidArgsError;


    switch (ePitchIdx)
    {
    default:
    case kPitchTimeAcf:
        pCInstance = new CPitchTimeAcf(ePitchIdx, iDataLength, fSampleRate);
        break;

    case kPitchTimeZeroCrossings:
        pCInstance = new CPitchTimeZeroCrossings(ePitchIdx, iDataLength, fSampleRate);
        break;

    case kPitchTimeAuditory:
        pCInstance = new CPitchTimeAuditory(ePitchIdx, iDataLength, fSampleRate);
        break;

    case kPitchTimeAmdf:
        pCInstance = new CPitchTimeAmdf(ePitchIdx, iDataLength, fSampleRate);
        break;


    case kPitchSpectralHps:
        pCInstance = new CPitchSpectralHps(ePitchIdx, iDataLength, fSampleRate);
        break;

    case kPitchSpectralAcf:
        pCInstance = new CPitchSpectralAcf(ePitchIdx, iDataLength, fSampleRate);
        break;
    }

    return Error_t::kNoError;
}

Error_t CPitchFromBlockIf::destroy(CPitchFromBlockIf*& pCInstance)
{
    delete pCInstance;

    pCInstance = 0;

    return Error_t::kNoError;
}
