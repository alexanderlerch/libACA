#define _USE_MATH_DEFINES
#include <limits>

#include "Filter.h"
#include "Vector.h"
#include "Util.h"

#include "ToolConversion.h"
#include "ToolCcf.h"
#include "ToolGammatone.h"

#include "PitchFromBlock.h"

///////////////////////////////////////////////////////////////////
// Pitchs that need "memory" so can't easily work as static functions
class CPitchSpectralAcf : public CPitchFromBlockIf
{
public:
    CPitchSpectralAcf(CPitchIf::PitchExtractors_t ePitchIdx, int iDataLength, float fSampleRate) : CPitchFromBlockIf(ePitchIdx, iDataLength, fSampleRate)
    {
        m_pCCcf = new CCcf();
        m_pCCcf->init((m_iDataLength-1)*2);
        CVector::alloc(m_pfProcBuff, (static_cast<long long>(m_iDataLength) - 1) * 2);

        CVector::alloc(m_pfAcf, m_pCCcf->getCcfLength(true));
    };

    virtual ~CPitchSpectralAcf()
    {
        CVector::free(m_pfAcf);
        CVector::free(m_pfProcBuff);

        delete m_pCCcf;
        m_pCCcf = 0;
    };

    float compF0(const float* pfInput) override
    {
        assert(pfInput);

        // get minimum bin
        int iMin = CUtil::float2int<int>(CConversion::convertFreq2Bin(m_fMin, (m_iDataLength - 1) * 2, m_fSampleRate));
        float fTmp = 0;
        long long iMaxIdx = -1;

        // flip data so it's more robust and set bin 0 to max
        CVector::copy(m_pfProcBuff, pfInput, m_iDataLength);
        CVectorFloat::flip_I(m_pfProcBuff, m_iDataLength);
        CVector::copy(&m_pfProcBuff[m_iDataLength - 1], pfInput, m_iDataLength - static_cast<long long>(1));
        m_pfProcBuff[m_iDataLength - 1] = CVectorFloat::getMax(pfInput, m_iDataLength);

        // compute acf
        m_pCCcf->compCcf(m_pfProcBuff, m_pfProcBuff, true);
        m_pCCcf->getCcf(m_pfAcf, true);

        // adjust lower search boundary for local maxima
        for (auto k = iMin-1; k < m_iDataLength; k++)
        {
            if (m_pfAcf[k + 1] < m_pfAcf[k])
            {
                iMin--;
                break;
            }
            iMin++;
        }
        
        // sanity check
        if (iMin >= m_iDataLength - 2)
            return 0.F;

        // now find the maximum
        CVectorFloat::findMax(&m_pfAcf[iMin], fTmp, iMaxIdx, m_iDataLength - static_cast<long long>(1) - iMin);

        return CConversion::convertBin2Freq((iMin+iMaxIdx) * 1.F, (m_iDataLength - 1) * 2, m_fSampleRate);
    };

private:
    CPitchSpectralAcf() {};
    CPitchSpectralAcf(const CPitchSpectralAcf& that);     //!< disallow copy construction
    CPitchSpectralAcf& operator=(const CPitchSpectralAcf& c);

    CCcf* m_pCCcf = 0;
    float* m_pfAcf = 0;
    float* m_pfProcBuff = 0;

    float m_fMin = 300.F;
};

class CPitchSpectralHps : public CPitchFromBlockIf
{
public:
    CPitchSpectralHps(CPitchIf::PitchExtractors_t ePitchIdx, int iDataLength, float fSampleRate) : CPitchFromBlockIf(ePitchIdx, iDataLength, fSampleRate)
    {
        CVector::alloc(m_pfProcBuff, m_iDataLength);
    }

    virtual ~CPitchSpectralHps()
    {
        CVector::free(m_pfProcBuff);
    }

    float compF0(const float* pfInput) override
    {
        assert(pfInput);

        float fTmp = 0.F;
        long long iMaxIdx = 0;
        int iMin = CUtil::float2int<int>(CConversion::convertFreq2Bin(m_fMin, (m_iDataLength - 1) * 2, m_fSampleRate));
        CVector::copy(m_pfProcBuff, pfInput, m_iDataLength);
        CVectorFloat::setZero(m_pfProcBuff, iMin);
        CVectorFloat::setZero(&m_pfProcBuff[m_iDataLength/m_iOrder], static_cast<long long>(m_iDataLength) - (m_iDataLength / m_iOrder));

        // do the actual product sum
        for (auto j = 2; j <= m_iOrder; j++)
        {
            for (auto k = 0; k < m_iDataLength / j; k++)
                m_pfProcBuff[k] *= pfInput[j * k];
        }

        // this could be restricted a bit, but is it worth it?
        CVectorFloat::findMax(m_pfProcBuff, fTmp, iMaxIdx, m_iDataLength);

        // sanity check
        if (fTmp <= 1e-30F)
            return 0.F;

        return CConversion::convertBin2Freq(iMaxIdx * 1.F, (m_iDataLength - 1) * 2, m_fSampleRate);
    };
private:
    CPitchSpectralHps() {};
    CPitchSpectralHps(const CPitchSpectralHps& that);     //!< disallow copy construction  
    CPitchSpectralHps& operator=(const CPitchSpectralHps& c);

    float* m_pfProcBuff = 0;
    int m_iOrder = 4;
    float m_fMin = 300.F;
};

class CPitchTimeAcf : public CPitchFromBlockIf
{
public:
    CPitchTimeAcf(CPitchIf::PitchExtractors_t ePitchIdx, int iDataLength, float fSampleRate) : CPitchFromBlockIf(ePitchIdx, iDataLength, fSampleRate)
    {
        m_pCCcf = new CCcf();
        m_pCCcf->init(iDataLength);

        CVector::alloc(m_pfAcf, m_pCCcf->getCcfLength(true));
    };

    virtual ~CPitchTimeAcf()
    {
        CVector::free(m_pfAcf);

        delete m_pCCcf;
        m_pCCcf = 0;
    };

    float compF0(const float* pfInput) override
    {
        assert(pfInput);

        int iEta = 0,
            iEtaMin = static_cast<int>(m_fSampleRate / m_fMax);

        m_pCCcf->compCcf(pfInput, pfInput, true);
        m_pCCcf->getCcf(m_pfAcf, true);

        // avoid main lobe
        while (m_pfAcf[iEta] > m_fMinThresh)
            iEta++;
        if (iEtaMin < iEta)
            iEtaMin = iEta;

        // only look after first minimum
        iEta = 0;
        while (m_pfAcf[iEta] > m_pfAcf[iEta + 1])
        {
            iEta++;
            if (iEta >= m_iDataLength)
                break;
        }

        if (iEta >= m_iDataLength)
            iEtaMin = 0;
        else if (iEtaMin < iEta)
            iEtaMin = iEta;

        // get the maximum given the constraints above
        float fMax = 0;
        long long iMax = -1;
        CVectorFloat::findMax(&m_pfAcf[iEtaMin], fMax, iMax, static_cast<long long>(m_iDataLength) - iEtaMin);

        if (fMax <= 0)
            return 0.F;

        return m_fSampleRate / (iMax + iEtaMin);
    };


private:
    CPitchTimeAcf() {};
    CPitchTimeAcf(const CPitchTimeAcf& that);     //!< disallow copy construction
    CPitchTimeAcf& operator=(const CPitchTimeAcf& c);

    CCcf* m_pCCcf = 0;
    float* m_pfAcf = 0;

    float m_fMax = 2000.F;
    const float m_fMinThresh = 0.35F;

};

class CPitchTimeAuditory : public CPitchFromBlockIf
{
public:
    CPitchTimeAuditory(CPitchIf::PitchExtractors_t ePitchIdx, int iDataLength, float fSampleRate) : CPitchFromBlockIf(ePitchIdx, iDataLength, fSampleRate)
    {
        float afA[3] = { 0 };
        float afB[3] = { 0 };
        CButterLp::calcCoeffs(afB, afA, 2, m_fSmoothLpCutoff);

        m_pCCcf = new CCcf();
        m_pCCcf->init(iDataLength);

        CVector::alloc(m_pfAcf, m_pCCcf->getCcfLength(true));
        CVector::alloc(m_pfSumAcf, m_pCCcf->getCcfLength(true));
        CMatrix::alloc(m_ppfProcBuff, m_iNumBands, iDataLength);

        for (auto c = 0; c < m_iNumBands; c++)
        {
            m_apCFilter[c] = new CFilter<float>();
            m_apCFilter[c]->init(afB, afA, 3);
        }

        CGammaToneFbIf::create(m_pCFilterBank, fSampleRate, m_iNumBands);

    };

    virtual ~CPitchTimeAuditory()
    {
        CVector::free(m_pfAcf);
        CVector::free(m_pfSumAcf);
        CMatrix::free(m_ppfProcBuff, m_iNumBands);

        for (auto c = 0; c < m_iNumBands; c++)
            delete m_apCFilter[c];

        delete m_pCCcf;
        m_pCCcf = 0;

        CGammaToneFbIf::destroy(m_pCFilterBank);
    };

    float compF0(const float* pfInput) override
    {
        assert(pfInput);

        CVectorFloat::setZero(m_pfSumAcf, m_iDataLength);

        m_pCFilterBank->process(m_ppfProcBuff, pfInput, m_iDataLength);
        
        for (auto c = 0; c < m_iNumBands; c++)
        {
            // smooth
            m_apCFilter[c]->process(m_ppfProcBuff[c], m_ppfProcBuff[c], m_iDataLength);

            // compute acf
            m_pCCcf->compCcf(m_ppfProcBuff[c], m_ppfProcBuff[c], true);
            m_pCCcf->getCcf(m_pfAcf, true);

            // compute sum of acfs
            CVectorFloat::add_I(m_pfSumAcf, m_pfAcf, m_iDataLength);
        }

        int iEta = getAcfMax_(m_pfSumAcf);
        return m_fSampleRate / iEta;    
    }


private:
    CPitchTimeAuditory() {};
    CPitchTimeAuditory(const CPitchTimeAuditory& that);     //!< disallow copy construction
    CPitchTimeAuditory& operator=(const CPitchTimeAuditory& c);

    int getAcfMax_(const float* pfInput)
    {
        int iEta = 0,
            iEtaMin = static_cast<int>(m_fSampleRate / m_fMax);

        // avoid main lobe
        while (pfInput[iEta] > m_fMinThresh)
            iEta++;
        if (iEtaMin < iEta)
            iEtaMin = iEta;

        // only look after first minimum
        iEta = 0;
        while (pfInput[iEta] > pfInput[iEta + 1])
        {
            iEta++;
            if (iEta >= m_iDataLength)
                break;
        }

        if (iEta >= m_iDataLength)
            iEtaMin = 0;
        else if (iEtaMin < iEta)
            iEtaMin = iEta;

        // get the maximum given the constraints above
        float fMax = 0;
        long long iMax = -1;
        CVectorFloat::findMax(&pfInput[iEtaMin], fMax, iMax, static_cast<long long>(m_iDataLength) - iEtaMin);

        if (fMax <= 0)
            return 0;

        return static_cast<int>(iMax + iEtaMin);
    };

    CCcf* m_pCCcf = 0;
    float* m_pfAcf = 0;
    float* m_pfSumAcf = 0;
    float** m_ppfProcBuff = 0;

    static const int m_iNumBands = 20;
    CFilter<float>* m_apCFilter[m_iNumBands] = { 0 };
    CGammaToneFbIf* m_pCFilterBank = 0;


    float m_fMax = 2000.F;
    const float m_fSmoothLpCutoff = 0.02F;
    const float m_fMinThresh = 0.35F;

};

class CPitchTimeAmdf : public CPitchFromBlockIf
{
public:
    CPitchTimeAmdf(CPitchIf::PitchExtractors_t ePitchIdx, int iDataLength, float fSampleRate) : CPitchFromBlockIf(ePitchIdx, iDataLength, fSampleRate) {    };

    virtual ~CPitchTimeAmdf() {    };

    float compF0(const float* pfInput) override
    {
        assert(pfInput);

        float fResMin = std::numeric_limits<float>::max();
        int iResEta = -1;
        int iEtaMin = static_cast<int>(m_fSampleRate / m_fMax),
            iEtaMax = static_cast<int>(m_fSampleRate / m_fMin)+1;

        // sanity checks
        if (iEtaMax > m_iDataLength)
            iEtaMax = m_iDataLength;

        if (CVectorFloat::getSum(pfInput, m_iDataLength, true) <= 0)
            return 0.F;

        // compute amdf
        for (auto iEta = iEtaMin; iEta < iEtaMax; iEta++)
        {
            float fAmdf = 0.F;
            for (auto i = 0; i < m_iDataLength - iEta; i++)
                fAmdf += std::abs(pfInput[i] - pfInput[i + iEta]);

            if (fAmdf <= fResMin)
            {
                fResMin = fAmdf;
                iResEta = iEta;
            }
        }

        // sanity check
        if (iResEta < 0)
            return 0.F;

        return m_fSampleRate / iResEta;

    };

private:
    CPitchTimeAmdf() {};
    CPitchTimeAmdf(const CPitchTimeAmdf& that);     //!< disallow copy construction
    CPitchTimeAmdf& operator=(const CPitchTimeAmdf& c);

    float m_fMax = 2000.F;
    float m_fMin = 50.F;

};

class CPitchTimeZeroCrossings : public CPitchFromBlockIf
{
public:
    CPitchTimeZeroCrossings(CPitchIf::PitchExtractors_t ePitchIdx, int iDataLength, float fSampleRate) : CPitchFromBlockIf(ePitchIdx, iDataLength, fSampleRate) {};

    virtual ~CPitchTimeZeroCrossings() {};

    float compF0(const float* pfInput) override
    {
        assert(pfInput);

        int iNumZeroCrossings = 0;
        int iPrevIdx = -1;
        int iDist2Prev = 0;

        for (auto i = 0; i < m_iDataLength-1; i++)
        {
            if (pfInput[i] * pfInput[i + 1] < 0)
            {
                if (iPrevIdx > 0)
                {
                    iNumZeroCrossings++;
                    iDist2Prev += i - iPrevIdx;
                }
                iPrevIdx = i;
            }
        }
        if (iNumZeroCrossings <= 1)
            return 0.F;
        
        return m_fSampleRate / (iDist2Prev * 2.F / iNumZeroCrossings);
    };

private:
    CPitchTimeZeroCrossings() {};
    CPitchTimeZeroCrossings(const CPitchTimeZeroCrossings& that);     //!< disallow copy construction
    CPitchTimeZeroCrossings& operator=(const CPitchTimeZeroCrossings& c);
};

///////////////////////////////////////////////////////////////////
// normal member functions
Error_t CPitchFromBlockIf::create(CPitchFromBlockIf*& pCInstance, CPitchIf::PitchExtractors_t ePitchIdx, int iDataLength, float fSampleRate)
{
    if (iDataLength <= 0 || fSampleRate <= 0)
        return Error_t::kFunctionInvalidArgsError;


    switch (ePitchIdx)
    {
    default:
    case CPitchIf::kPitchTimeAcf:
        pCInstance = new CPitchTimeAcf(ePitchIdx, iDataLength, fSampleRate);
        break;

    case CPitchIf::kPitchTimeZeroCrossings:
        pCInstance = new CPitchTimeZeroCrossings(ePitchIdx, iDataLength, fSampleRate);
        break;

    //case CPitchIf::kPitchTimeAuditory:
    //    pCInstance = new CPitchTimeAuditory(ePitchIdx, iDataLength, fSampleRate);
    //    break;

    case CPitchIf::kPitchTimeAmdf:
        pCInstance = new CPitchTimeAmdf(ePitchIdx, iDataLength, fSampleRate);
        break;


    case CPitchIf::kPitchSpectralHps:
        pCInstance = new CPitchSpectralHps(ePitchIdx, iDataLength, fSampleRate);
        break;

    case CPitchIf::kPitchSpectralAcf:
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
