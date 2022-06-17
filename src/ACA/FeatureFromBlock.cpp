
#include "Vector.h"
#include "Matrix.h"
#include "Util.h"

#include "ToolConversion.h"
#include "ToolCcf.h"
#include "ToolLowPass.h"

#include "FeatureFromBlock.h"

const float CFeatureFromBlockIf::m_kfFloatThresh = 1e-30F;

////////////////////////////////////////////////////////////////////////
// static member functions
float CFeatureFromBlockIf::compFeatureSpectralCentroid(const float *pfMagSpec, int iDataLength, float fSampleRate /*= 1.F*/)
{
    assert(pfMagSpec);
    assert(iDataLength > 0);
    assert(fSampleRate > 0);

    float fNorm = 0;
    float fvsc = 0;

    for (auto k = 0; k < iDataLength; k++)
    {
        fNorm += pfMagSpec[k];
        fvsc += k * pfMagSpec[k];
    }

    // check if zero
    if (fNorm < m_kfFloatThresh)
        return 0;

    // convert from index to Hz
    return fvsc * fSampleRate / (2.F * fNorm * (iDataLength - 1));
}

float CFeatureFromBlockIf::compFeatureSpectralCrestFactor(const float *pfMagSpec, int iDataLength, float /*fSampleRate = 1.F*/)
{
    assert(pfMagSpec);
    assert(iDataLength > 0);

    float fNorm = CVector::getSum(pfMagSpec, iDataLength);

    // check if zero
    if (fNorm < m_kfFloatThresh)
        return 0;

    return CVector::getMax(pfMagSpec, iDataLength) / fNorm;
}

float CFeatureFromBlockIf::compFeatureSpectralDecrease(const float *pfMagSpec, int iDataLength, float /*fSampleRate = 1.F*/)
{
    assert(pfMagSpec);
    assert(iDataLength > 0);

    float fNorm = 0;
    float fvsd = 0;

    for (auto k = 1; k < iDataLength; k++)
    {
        fNorm += pfMagSpec[k];
        fvsd += (pfMagSpec[k] - pfMagSpec[0]) / k;
    }

    // check if zero
    if (fNorm < m_kfFloatThresh)
        return 0;

    return fvsd / fNorm;
}

float CFeatureFromBlockIf::compFeatureSpectralFlatness(const float *pfMagSpec, int iDataLength, float /*fSampleRate = 1.F*/)
{
    assert(pfMagSpec);
    assert(iDataLength > 0);

    float fNorm = CVector::getMean(pfMagSpec, iDataLength);
    float fGeoMean = 0;

    // avoid unnecessary complications
    if (fNorm < m_kfFloatThresh || CVector::getMin(pfMagSpec, iDataLength) < m_kfFloatThresh)
        return 0;

    // compute geometric mean through log
    for (auto k = 0; k < iDataLength; k++)
        fGeoMean += std::log(pfMagSpec[k]);

    return std::exp(fGeoMean / iDataLength) / fNorm;
}

float CFeatureFromBlockIf::compFeatureSpectralFlux(const float *pfMagSpec, const float *pfPrevSpec, int iDataLength, float /*fSampleRate = 1.F*/)
{
    assert(pfMagSpec);
    assert(pfPrevSpec);
    assert(iDataLength > 0);

    float fSum = 0;
    for (auto k = 0; k < iDataLength; k++)
    {
        float fDiff = pfMagSpec[k] - pfPrevSpec[k];
        fSum += fDiff * fDiff;
    }

    return std::sqrt(fSum) / iDataLength;
}

float CFeatureFromBlockIf::compFeatureSpectralSpread(const float *pfMagSpec, int iDataLength, float fSampleRate /*= 1.F*/)
{
    assert(pfMagSpec);
    assert(iDataLength > 0);
    assert(fSampleRate > 0);

    float fNorm = 0;
    float fvss = 0;
    float fvsc = compFeatureSpectralCentroid(pfMagSpec, iDataLength, fSampleRate) * 2 / fSampleRate * (iDataLength - 1);

    // check if zero
    if (fvsc < m_kfFloatThresh)
        return 0;

    for (auto k = 0; k < iDataLength; k++)
    {
        float fAgg = (k - fvsc) * (k - fvsc);
        fNorm += pfMagSpec[k];
        fvss += fAgg * pfMagSpec[k];
    }

    // convert from index to Hz
    return std::sqrt(fvss / fNorm) * fSampleRate / (2.F * (iDataLength - 1));
}

float CFeatureFromBlockIf::compFeatureSpectralKurtosis(const float *pfMagSpec, int iDataLength, float fSampleRate /*= 1.F*/)
{
    assert(pfMagSpec);
    assert(iDataLength > 0);
    assert(fSampleRate > 0);

    float fNorm = 0;
    float fvsk = 0;
    float fvsc = compFeatureSpectralCentroid(pfMagSpec, iDataLength, fSampleRate) * 2 / fSampleRate * (iDataLength - 1);
    float fvss = compFeatureSpectralSpread(pfMagSpec, iDataLength, fSampleRate) * 2 / fSampleRate * (iDataLength - 1);

    // check if zero
    if (fvsc < m_kfFloatThresh || fvss < m_kfFloatThresh)
        return 0;

    for (auto k = 0; k < iDataLength; k++)
    {
        float fAgg = (k - fvsc) * (k - fvsc);
        fAgg *= fAgg;
        fNorm += pfMagSpec[k];
        fvsk += fAgg * pfMagSpec[k];
    }

    // check if zero
    if (fNorm < m_kfFloatThresh)
        return 0;

    return fvsk / (fvss * fvss * fvss * fvss * fNorm) - 3;
}

float CFeatureFromBlockIf::compFeatureSpectralRolloff(const float *pfMagSpec, int iDataLength, float fSampleRate /*= 1.F*/, float fKappa /*= .85F*/)
{
    assert(pfMagSpec);
    assert(iDataLength > 0);
    assert(fSampleRate > 0);
    assert(fKappa > 0 && fKappa <= 1);

    float fNorm = CVector::getSum(pfMagSpec, iDataLength);
    float fSum = 0;
    int k = 0;

    // check if zero
    if (fNorm < m_kfFloatThresh)
        return 0;

    fNorm *= fKappa;
    while (fSum <= fNorm)
    {
        fSum += pfMagSpec[k];
        k++;

        assert(k < iDataLength);
    }

    // convert from index to Hz
    return (k - 1) * fSampleRate / (2.F * (iDataLength - 1));
}

float CFeatureFromBlockIf::compFeatureSpectralSkewness(const float *pfMagSpec, int iDataLength, float fSampleRate /*= 1.F*/)
{
    assert(pfMagSpec);
    assert(iDataLength > 0);
    assert(fSampleRate > 0);

    float fNorm = 0;
    float fvssk = 0;
    float fvsc = compFeatureSpectralCentroid(pfMagSpec, iDataLength, fSampleRate) * 2 / fSampleRate * (iDataLength - 1);
    float fvss = compFeatureSpectralSpread(pfMagSpec, iDataLength, fSampleRate) * 2 / fSampleRate * (iDataLength - 1);

    // check if zero
    if (fvsc < m_kfFloatThresh || fvss < m_kfFloatThresh)
        return 0;

    for (auto k = 0; k < iDataLength; k++)
    {
        float fAgg = (k - fvsc) * (k - fvsc) * (k - fvsc);
        fNorm += pfMagSpec[k];
        fvssk += fAgg * pfMagSpec[k];
    }

    // check if zero
    if (fNorm < m_kfFloatThresh)
        return 0;

    return fvssk / (fvss * fvss * fvss * fNorm);
}

float CFeatureFromBlockIf::compFeatureSpectralSlope(const float *pfMagSpec, int iDataLength, float fSampleRate /*= 1.F*/)
{
    assert(pfMagSpec);
    assert(iDataLength > 0);
    assert(fSampleRate > 0);

    // compute mean
    float fvsc = compFeatureSpectralCentroid(pfMagSpec, iDataLength, fSampleRate) * 2 / fSampleRate * (iDataLength - 1);
    float fvssl = 0;
    float fNorm = 0;

    // check if zero
    if (fvsc < m_kfFloatThresh)
        return 0;

    for (auto k = 0; k < iDataLength; k++)
    {
        float fk = k - (iDataLength + 1) / 2.F;
        fvssl += fk * (pfMagSpec[k] - fvsc);
        fNorm += fk * fk;
    }

    // check if zero
    if (fNorm < m_kfFloatThresh)
        return 0;

    return fvssl / fNorm;
}
float CFeatureFromBlockIf::compFeatureSpectralTonalPowerRatio(const float *pfMagSpec, int iDataLength, float /*fSampleRate = 1.F*/, float fThresh /*= 5e-4F*/)
{
    assert(pfMagSpec);
    assert(iDataLength > 0);

    // initialize
    float fvtpr = 0;
    float fNorm = pfMagSpec[0] * pfMagSpec[0] + pfMagSpec[iDataLength - 1] * pfMagSpec[iDataLength - 1];

    for (auto k = 1; k < iDataLength - 1; k++)
    {
        float fSquare = pfMagSpec[k] * pfMagSpec[k];
        fNorm += fSquare;

        // search for local maxima
        if (pfMagSpec[k] <= pfMagSpec[k - 1] || pfMagSpec[k] <= pfMagSpec[k + 1] || pfMagSpec[k] <= fThresh)
            continue;
        else
        {
            fvtpr += fSquare;

            // increment because the next bin cannot be a local max but don't forget fNorm
            fNorm += pfMagSpec[k + 1] * pfMagSpec[k + 1];
            k++;
        }
    }

    // check if zero
    if (fNorm < m_kfFloatThresh)
        return 0;

    return fvtpr / fNorm;
}

float CFeatureFromBlockIf::compFeatureTimeAcfCoeff(const float *pfSamples, int iDataLength, float /*fSampleRate = 1.F*/, int  iEta /*= 19*/)
{
    assert(pfSamples);
    assert(iDataLength > iEta);
    assert(iEta >= 0);

    return CVector::mulScalar(pfSamples, &pfSamples[iEta], static_cast<long long>(iDataLength) - iEta);
}

float CFeatureFromBlockIf::compFeatureTimePeakEnvelope(const float *pfSamples, int iDataLength, float /*fSampleRate = 1.F*/)
{
    assert(pfSamples);
    assert(iDataLength > 0);

    return CVector::getMax(pfSamples, iDataLength);
}

float CFeatureFromBlockIf::compFeatureTimeStd(const float *pfSamples, int iDataLength, float /*fSampleRate = 1.F*/)
{
    assert(pfSamples);
    assert(iDataLength > 0);

    return CVector::getStd(pfSamples, iDataLength);
}

float CFeatureFromBlockIf::compFeatureTimeRms(const float *pfSamples, int iDataLength, float /*fSampleRate = 1.F*/)
{
    assert(pfSamples);
    assert(iDataLength > 0);

    return CVector::getStd(pfSamples, iDataLength, 0.F);
}

float CFeatureFromBlockIf::compFeatureTimeZeroCrossingRate(const float *pfSamples, int iDataLength, float /*fSampleRate = 1.F*/)
{
    assert(pfSamples);
    assert(iDataLength > 0);

    float fvzc = 0;

    float fPrevSign = 0;
    for (auto i = 0; i < iDataLength; i++)
    {
        float fCurrSign = CUtil::sign(pfSamples[i]);
        fvzc += std::abs(fCurrSign - fPrevSign);
        fPrevSign = fCurrSign;
    }

    // standardize feature 
    return fvzc / (2 * iDataLength);
}

CFeatureFromBlockIf::CFeatureFromBlockIf(CFeatureIf::Feature_t eFeatureIdx, int iDataLength, float fSampleRate) : m_eFeatureIdx(eFeatureIdx), m_iDataLength(iDataLength), m_fSampleRate(fSampleRate) { assert(iDataLength > 0); }


///////////////////////////////////////////////////////////////////
// features that need "memory" so can't easily work as static functions

/*! \brief class for computation of the spectral flux
*/
class CFeatureSpectralFlux : public CFeatureFromBlockIf
{
public:
    CFeatureSpectralFlux(CFeatureIf::Feature_t eFeatureIdx, int iDataLength, float fSampleRate) : CFeatureFromBlockIf(eFeatureIdx, iDataLength, fSampleRate)
    {
        CVector::alloc(m_pfPrevSpec, m_iDataLength);
        CVector::setZero(m_pfPrevSpec, m_iDataLength);
    };

    virtual ~CFeatureSpectralFlux()
    {
        CVector::free(m_pfPrevSpec);
    };

    Error_t compFeature(float *pfFeature, const float *pfIn) override
    {
        *pfFeature = compFeatureSpectralFlux(pfIn, m_pfPrevSpec, m_iDataLength, m_fSampleRate);

        CVector::copy(m_pfPrevSpec, pfIn, m_iDataLength);

        return Error_t::kNoError;
    };

private:
    CFeatureSpectralFlux() {};
    CFeatureSpectralFlux(const CFeatureSpectralFlux &that);     //!< disallow copy construction
    CFeatureSpectralFlux &operator=(const CFeatureSpectralFlux &c);

    float *m_pfPrevSpec = 0; //!< memory of previous spectrum
};

/*! \brief class for computation of the mfccs
*/
class CFeatureSpectralMfccs : public CFeatureFromBlockIf
{
public:
    CFeatureSpectralMfccs(CFeatureIf::Feature_t eFeatureIdx, int iDataLength, float fSampleRate) : CFeatureFromBlockIf(eFeatureIdx, iDataLength, fSampleRate)
    {
        // alloc transfer function memory
        CMatrix::alloc(m_ppfH, m_iNumBands, iDataLength);

        // init MFCC filters
        genMfccFilters_();

        // init dct transform
        genDctMat_(m_iNumMfcCoeffs);

        CVector::alloc(m_pfMelSpec, m_iNumBands);
    }

    virtual ~CFeatureSpectralMfccs()
    {
        CMatrix::free(m_ppfH, m_iNumBands);

        deleteDctMat_();

        CVector::free(m_pfMelSpec);
    }

    Error_t compFeature(float *pfFeature, const float *pfIn) override
    {
        assert(pfFeature);
        assert(pfIn);

        CVector::setZero(pfFeature, m_iNumMfcCoeffs);

        // compute mel spectrum
        for (auto c = 0; c < m_iNumBands; c++)
            m_pfMelSpec[c] = std::log10(CVector::mulScalar(m_ppfH[c], pfIn, m_iDataLength) + 1e-20F);

        // compute dct
        for (auto j = 0; j < m_iNumMfcCoeffs; j++)
            pfFeature[j] = CVector::mulScalar(m_ppfDct[j], m_pfMelSpec, m_iNumBands);

        return Error_t::kNoError;
    };


    int getFeatureDimensions() const override
    {
        return m_iNumMfcCoeffs;
    }

    bool hasAdditionalParam() const override
    {
        return true;
    }

    Error_t setAdditionalParam(float fParamValue) override
    {
        if (fParamValue <= 0)
            return Error_t::kFunctionInvalidArgsError;

        genDctMat_(CUtil::float2int<int>(fParamValue));

        return Error_t::kNoError;
    }
private:
    CFeatureSpectralMfccs() {};
    CFeatureSpectralMfccs(const CFeatureSpectralMfccs &that);     //!< disallow copy construction  
    CFeatureSpectralMfccs &operator=(const CFeatureSpectralMfccs &c);

    void genMfccFilters_()
    {
        const double dFreq = 400. / 3.;
        const int iNumLinFilters = 13;
        const double dLinSpacing = 200. / 3.,
            dLogSpacing = 1.0711703; // note sure where this mel resolution comes from exactly

        assert(m_iNumBands > iNumLinFilters);
        double adBoundFreqs[3] = { dFreq,
            dFreq + dLinSpacing,
            dFreq + 2. * dLinSpacing };
        int aiBoundIdx[3] = { static_cast<int>(CConversion::convertFreq2Bin(static_cast<float>(adBoundFreqs[0]), (m_iDataLength - 1) * 2, m_fSampleRate)),
            static_cast<int>(CConversion::convertFreq2Bin(static_cast<float>(adBoundFreqs[1]), (m_iDataLength - 1) * 2, m_fSampleRate)),
            static_cast<int>(CConversion::convertFreq2Bin(static_cast<float>(adBoundFreqs[2]), (m_iDataLength - 1) * 2, m_fSampleRate)) };

        for (auto c = 0; c < m_iNumBands; c++)
        {
            double dFilterAmp = 2. / (adBoundFreqs[2] - adBoundFreqs[0]);

            // upward slope
            for (auto k = aiBoundIdx[0]; k <= aiBoundIdx[1]; k++)
            {
                float fFreqk = CConversion::convertBin2Freq(1.F * k, (m_iDataLength - 1) * 2, m_fSampleRate);
                if ((fFreqk - adBoundFreqs[0]) <= 0.F)
                    continue;
                m_ppfH[c][k] = static_cast<float>(dFilterAmp * (fFreqk - adBoundFreqs[0]) / (adBoundFreqs[1] - adBoundFreqs[0]));
                assert(m_ppfH[c][k] >= 0);
            }

            // downward slope
            for (auto k = aiBoundIdx[1] + 1; k <= aiBoundIdx[2]; k++)
            {
                float fFreqk = CConversion::convertBin2Freq(1.F * k, (m_iDataLength - 1) * 2, m_fSampleRate);
                m_ppfH[c][k] = static_cast<float>(dFilterAmp * (adBoundFreqs[2] - fFreqk) / (adBoundFreqs[2] - adBoundFreqs[1]));
                assert(m_ppfH[c][k] >= 0);
            }

            // proceed to next band
            adBoundFreqs[0] = adBoundFreqs[1];
            adBoundFreqs[1] = adBoundFreqs[2];
            adBoundFreqs[2] = (c < iNumLinFilters - 3) ? adBoundFreqs[1] + dLinSpacing : adBoundFreqs[2] * dLogSpacing; //!< Check me
            aiBoundIdx[0] = aiBoundIdx[1];
            aiBoundIdx[1] = aiBoundIdx[2];
            aiBoundIdx[2] = static_cast<int>(CConversion::convertFreq2Bin(static_cast<float>(adBoundFreqs[2]), (m_iDataLength - 1) * 2, m_fSampleRate));
        }
    }

    void genDctMat_(int iNumCoeffs)
    {
        allocDctMat_(iNumCoeffs);

        for (auto c = 0; c < iNumCoeffs; c++)
        {
            for (auto b = 0; b < m_iNumBands; b++)
                m_ppfDct[c][b] = static_cast<float>(std::cos(c * (2. * b + 1) * M_PI / 2. / m_iNumBands));

            CVector::mulC_I(m_ppfDct[c], 1 / std::sqrt(m_iNumBands / 2.F), m_iNumBands);
        }
        CVector::mulC_I(m_ppfDct[0], 1.F / std::sqrt(2.f), m_iNumBands);

        m_iNumMfcCoeffs = iNumCoeffs;
    }
    void allocDctMat_(int iNumCoeffs)
    {
        if (m_ppfDct)
            deleteDctMat_();

        CMatrix::alloc(m_ppfDct, iNumCoeffs, m_iNumBands);
    }
    void deleteDctMat_()
    {
        CMatrix::free(m_ppfDct, m_iNumMfcCoeffs);
    }

    const int m_iNumBands = 40; //!< number of mel bands
    int m_iNumMfcCoeffs = 13; //!< number of dct coefficients to extract

    float **m_ppfH = 0, //!< filter bank matrix
        **m_ppfDct = 0; //!< dct matrix

    float *m_pfMelSpec = 0; //!< mel-transformed spectrum
};

/*! \brief class for computation of the pitch chroma
*/
class CFeatureSpectralPitchChroma : public CFeatureFromBlockIf
{
public:
    CFeatureSpectralPitchChroma(CFeatureIf::Feature_t eFeatureIdx, int iDataLength, float fSampleRate) : CFeatureFromBlockIf(eFeatureIdx, iDataLength, fSampleRate)
    {
        // alloc transfer function memory
        CMatrix::alloc(m_ppfH, m_iNumPitchClasses, iDataLength);

        genPcFilters_();
    };

    virtual ~CFeatureSpectralPitchChroma()
    {
        CMatrix::free(m_ppfH, m_iNumPitchClasses);
    };

    Error_t compFeature(float *pfFeature, const float *pfIn) override
    {
        assert(pfFeature);
        assert(pfIn);

        CVector::setZero(pfFeature, m_iNumPitchClasses);

        for (auto p = 0; p < m_iNumPitchClasses; p++)
        {
            // we could do this nicer with CVector::mulScalar if we allocated memory
            for (auto k = 0; k < m_iDataLength; k++)
                pfFeature[p] += m_ppfH[p][k] * (pfIn[k] * pfIn[k]);
        }

        // normalize chroma
        float fSum = CVector::getSum(pfFeature, m_iNumPitchClasses);
        if (fSum > 0)
            CVector::mulC_I(pfFeature, 1.F / fSum, m_iNumPitchClasses);

        return Error_t::kNoError;
    };


    int getFeatureDimensions() const override
    {
        return m_iNumPitchClasses;
    }

    bool hasAdditionalParam() const override
    {
        return true;
    }

    Error_t setAdditionalParam(float fParamValue) override
    {
        if (fParamValue <= 0)
            return Error_t::kFunctionInvalidArgsError;

        m_iNumOctaves = CUtil::float2int<int>(fParamValue);

        // recompute transformation matrix
        genPcFilters_();

        return Error_t::kNoError;
    }
private:
    CFeatureSpectralPitchChroma() {};
    CFeatureSpectralPitchChroma(const CFeatureSpectralPitchChroma &that);     //!< disallow copy construction
    CFeatureSpectralPitchChroma &operator=(const CFeatureSpectralPitchChroma &c);

    void genPcFilters_()
    {
        const float fStartPitch = 60; //!< C4
        float fMid = CConversion::convertMidi2Freq(fStartPitch, m_fA4);

        //sanity check: reduce number of octaves if highest freq is higher than half fs
        while (fMid * std::exp2(m_iNumOctaves) > m_fSampleRate / 2)
            m_iNumOctaves--;
        if (m_iNumOctaves <= 0)
            return;

        for (auto p = 0; p < m_iNumPitchClasses; p++)
        {
            const float fQuarterToneRatio = 1.02930223664349F;
            float afBoundFreqs[2] = { fMid / fQuarterToneRatio,
                fMid * fQuarterToneRatio };

            for (auto o = 0; o < m_iNumOctaves; o++)
            {
                // get indices from freqs
                const int aiBoundIdx[2] = { static_cast<int>(CConversion::convertFreq2Bin(afBoundFreqs[0], (m_iDataLength - 1) * 2, m_fSampleRate)) + 1,
                    static_cast<int>(CConversion::convertFreq2Bin(afBoundFreqs[1], (m_iDataLength - 1) * 2, m_fSampleRate)) };

                // set transfer function
                CVector::setValue(&m_ppfH[p][aiBoundIdx[0]], 1.F / (aiBoundIdx[1] - aiBoundIdx[0] + 1), static_cast<long long>(aiBoundIdx[1]) - aiBoundIdx[0] + 1);

                // proceed to next octave
                for (auto i = 0; i < 2; i++)
                    afBoundFreqs[i] *= 2.F;
            }

            // proceed to next pitch class
            fMid *= fQuarterToneRatio * fQuarterToneRatio;
        }
    }

    const int m_iNumPitchClasses = 12; //!< number of pitch classes

    const float m_fA4 = 440.F; //!< frequency of concert pitch
    int m_iNumOctaves = 4; //!< number of octaves to aggregate

    float **m_ppfH = 0; //!< pitch filter bank matrix
};

/*! \brief class for computation of the spectral rolloff
*/
class CFeatureSpectralRolloff : public CFeatureFromBlockIf
{
public:
    CFeatureSpectralRolloff(CFeatureIf::Feature_t eFeatureIdx, int iDataLength, float fSampleRate) : CFeatureFromBlockIf(eFeatureIdx, iDataLength, fSampleRate) {};

    virtual ~CFeatureSpectralRolloff() {};

    Error_t compFeature(float *pfFeature, const float *pfIn) override
    {
        *pfFeature = compFeatureSpectralRolloff(pfIn, m_iDataLength, m_fSampleRate, m_fKappa);

        return Error_t::kNoError;
    };

    bool hasAdditionalParam() const override
    {
        return true;
    }

    Error_t setAdditionalParam(float fParamValue) override
    {
        if (fParamValue <= 0 || fParamValue > 1)
            return Error_t::kNoError;

        m_fKappa = fParamValue;

        return Error_t::kNoError;
    }

private:
    CFeatureSpectralRolloff() {};

    float m_fKappa = 0.85F; //!< bandwidth parameter
};

/*! \brief class for computation of the tonal power ratio
*/
class CFeatureSpectralTonalPowerRatio : public CFeatureFromBlockIf
{
public:
    CFeatureSpectralTonalPowerRatio(CFeatureIf::Feature_t eFeatureIdx, int iDataLength, float fSampleRate) : CFeatureFromBlockIf(eFeatureIdx, iDataLength, fSampleRate) {};

    virtual ~CFeatureSpectralTonalPowerRatio() {};

    Error_t compFeature(float *pfFeature, const float *pfIn) override
    {
        *pfFeature = compFeatureSpectralTonalPowerRatio(pfIn, m_iDataLength, m_fSampleRate, m_fThresh);

        return Error_t::kNoError;
    };

    bool hasAdditionalParam() const override
    {
        return true;
    }

    Error_t setAdditionalParam(float fParamValue) override
    {
        if (fParamValue <= 0)
            return Error_t::kNoError;

        m_fThresh = fParamValue;

        return Error_t::kNoError;
    }

private:
    CFeatureSpectralTonalPowerRatio() {};

    float m_fThresh = 5e-4F;
};

/*! \brief class for computation of an acf coefficient
*/
class CFeatureTimeAcfCoeff : public CFeatureFromBlockIf
{
public:
    CFeatureTimeAcfCoeff(CFeatureIf::Feature_t eFeatureIdx, int iDataLength, float fSampleRate) : CFeatureFromBlockIf(eFeatureIdx, iDataLength, fSampleRate) {};

    virtual ~CFeatureTimeAcfCoeff() {};

    Error_t compFeature(float *pfFeature, const float *pfIn) override
    {
        *pfFeature = compFeatureTimeAcfCoeff(pfIn, m_iDataLength, m_fSampleRate, m_iEta);

        return Error_t::kNoError;
    };

    bool hasAdditionalParam() const override
    {
        return true;
    }

    Error_t setAdditionalParam(float fParamValue) override
    {
        if (fParamValue <= 0)
            return Error_t::kNoError;

        m_iEta = CUtil::float2int<int>(fParamValue);

        return Error_t::kNoError;
    }

private:
    CFeatureTimeAcfCoeff() {};

    int m_iEta = 19; //!< int this example implementation, we chose the 19th coefficient
};

/*! \brief class for computation of the maximum of the acf
*/
class CFeatureTimeMaxAcf : public CFeatureFromBlockIf
{
public:
    CFeatureTimeMaxAcf(CFeatureIf::Feature_t eFeatureIdx, int iDataLength, float fSampleRate) : CFeatureFromBlockIf(eFeatureIdx, iDataLength, fSampleRate)
    {
        m_pCCcf = new CCcf();
        m_pCCcf->init(iDataLength);

        CVector::alloc(m_pfAcf, m_pCCcf->getCcfLength(true));
    };

    virtual ~CFeatureTimeMaxAcf()
    {
        CVector::free(m_pfAcf);

        delete m_pCCcf;
        m_pCCcf = 0;
    };

    Error_t compFeature(float *pfFeature, const float *pfIn) override
    {
        float fMinThresh = 0.35F;

        int iEta = 0,
            iEtaMin = static_cast<int>(m_fSampleRate / m_fMax);

        // compute ACF
        m_pCCcf->compCcf(pfIn, pfIn, true);
        m_pCCcf->getCcf(m_pfAcf, true);

        // avoid main lobe
        while (m_pfAcf[iEta] > fMinThresh)
            iEta++;
        if (iEtaMin < iEta)
            iEtaMin = iEta;

        // only look after first minimum
        iEta = 0;
        while (m_pfAcf[iEta] > m_pfAcf[iEta + 1])
        {
            iEta++;
            if (iEta >= m_iDataLength-1)
                break;
        }

        if (iEta >= m_iDataLength-1)
            iEtaMin = 0;
        else if (iEtaMin < iEta)
            iEtaMin = iEta;

        // get the maximum given the constraints above
        *pfFeature = CVector::getMax(&m_pfAcf[iEtaMin], static_cast<long long>(m_iDataLength) - iEtaMin);

        return Error_t::kNoError;
    };

    bool hasAdditionalParam() const override
    {
        return true;
    }

    Error_t setAdditionalParam(float fParamValue) override
    {
        if (fParamValue <= 0 || fParamValue > m_fSampleRate / 2)
            return Error_t::kNoError;

        m_fMax = fParamValue;

        return Error_t::kNoError;
    }

private:
    CFeatureTimeMaxAcf() {};
    CFeatureTimeMaxAcf(const CFeatureTimeMaxAcf &that);     //!< disallow copy construction
    CFeatureTimeMaxAcf &operator=(const CFeatureTimeMaxAcf &c);

    CCcf *m_pCCcf = 0; //!< correlation instance
    float *m_pfAcf = 0; //!< acf result

    float m_fMax = 2000.F; //!< upper frequency boundary
};

/*! \brief class for computation of the peak envelope
*/
class CFeatureTimePeakEnvelope : public CFeatureFromBlockIf
{
public:
    CFeatureTimePeakEnvelope(CFeatureIf::Feature_t eFeatureIdx, int iDataLength, float fSampleRate) : CFeatureFromBlockIf(eFeatureIdx, iDataLength, fSampleRate)
    {
        for (auto i = 0; i < kNumPpmFilters; i++)
            m_afAlpha[i] = CSinglePoleLp::calcFilterParam(m_afIntegrationTimeInS[i], fSampleRate);
    };

    virtual ~CFeatureTimePeakEnvelope() {};

    Error_t compFeature(float *pfFeature, const float *pfIn) override
    {
        // extract 1st variant: maximum per block
        pfFeature[kBlockMax] = compFeatureTimePeakEnvelope(pfIn, m_iDataLength, m_fSampleRate);

        // extract 2nd variant: maximum of ppm
        pfFeature[kPpmMax] = 0;
        for (auto i = 0; i < m_iDataLength; i++)
        {
            float fOut = ppm_I(pfIn[i]);
            if (fOut > pfFeature[kPpmMax])
                pfFeature[kPpmMax] = fOut;
        }

        return Error_t::kNoError;
    };

    int getFeatureDimensions() const override
    {
        return kNumPeakTypes;
    }

private:
    CFeatureTimePeakEnvelope() {};

    enum PeakType_t
    {
        kBlockMax,
        kPpmMax,

        kNumPeakTypes
    };

    enum PpmFilters_t
    {
        kAttack,
        kRelease,

        kNumPpmFilters
    };

    float ppm_I(float fIn)
    {
        float fOut = 0;

        fIn = std::abs(fIn);

        // check release vs. attack phase
        if (m_fFilterBuff > fIn)
            fOut = m_afAlpha[kRelease] * m_fFilterBuff;
        else
            fOut = (1.F - m_afAlpha[kAttack]) * fIn + m_afAlpha[kAttack] * m_fFilterBuff;

        m_fFilterBuff = fOut;

        return fOut;
    }

    const float m_afIntegrationTimeInS[kNumPpmFilters] = { .01F, 1.5F }; //!< integration times for filter initialization
    float m_afAlpha[kNumPpmFilters] = { 0,0 }; //!< filter coefficients
    float m_fFilterBuff = 0.F; //!< internal filter memory
};

/*! \brief class for computation of the rms
*/
class CFeatureTimeRms : public CFeatureFromBlockIf
{
public:
    CFeatureTimeRms(CFeatureIf::Feature_t eFeatureIdx, int iDataLength, float fSampleRate) : CFeatureFromBlockIf(eFeatureIdx, iDataLength, fSampleRate)
    {
        CSinglePoleLp::create(m_pCSinglePole);
        m_pCSinglePole->setFilterParam(CSinglePoleLp::calcFilterParam(m_fIntegrationTimeInS, fSampleRate));
    };

    virtual ~CFeatureTimeRms()
    {
        CSinglePoleLp::destroy(m_pCSinglePole);
    };

    Error_t compFeature(float *pfFeature, const float *pfIn) override
    {
        // extract 1st variant: rms per block
        pfFeature[kBlockRms] = compFeatureTimeRms(pfIn, m_iDataLength, m_fSampleRate);

        // extract 2nd variant: rms from single pole
        // do inefficient sample based processing so we don't have to alloc extra memory
        pfFeature[kLowpass] = 0;
        for (auto i = 0; i < m_iDataLength; i++)
        {
            float fIn = pfIn[i] * pfIn[i];
            float fOut = 0;
            m_pCSinglePole->process(&fOut, &fIn, 1);
            if (fOut > pfFeature[kLowpass])
                pfFeature[kLowpass] = fOut;
        }
        pfFeature[kLowpass] = std::sqrt(pfFeature[kLowpass]);

        return Error_t::kNoError;
    };

    int getFeatureDimensions() const override
    {
        return kNumRmsTypes;
    }

    bool hasAdditionalParam() const override
    {
        return true;
    }

    Error_t setAdditionalParam(float fParamValue) override
    {
        if (fParamValue <= 0)
            return Error_t::kNoError;

        m_fIntegrationTimeInS = fParamValue;

        return Error_t::kNoError;
    }

private:
    enum RmsType_t
    {
        kBlockRms,
        kLowpass,

        kNumRmsTypes
    };
    CFeatureTimeRms() {};
    CFeatureTimeRms(const CFeatureTimeRms &that); //!< disallow copy construction   

    CSinglePoleLp *m_pCSinglePole = 0; //!< instance of low pass filter

    float m_fIntegrationTimeInS = .3F; //!< integration time for filter coeff calculation
};

///////////////////////////////////////////////////////////////////
// normal member functions
Error_t CFeatureFromBlockIf::create(CFeatureFromBlockIf *&pCInstance, CFeatureIf::Feature_t eFeatureIdx, int iDataLength, float fSampleRate)
{
    if (iDataLength <= 0 || fSampleRate <= 0)
        return Error_t::kFunctionInvalidArgsError;

    // see if we can compute this feature directly or have a derive class
    switch (eFeatureIdx)
    {
    default:
    case CFeatureIf::kFeatureSpectralCentroid:
    case CFeatureIf::kFeatureSpectralCrestFactor:
    case CFeatureIf::kFeatureSpectralDecrease:
    case CFeatureIf::kFeatureSpectralFlatness:
    case CFeatureIf::kFeatureSpectralKurtosis:
    case CFeatureIf::kFeatureSpectralSkewness:
    case CFeatureIf::kFeatureSpectralSlope:
    case CFeatureIf::kFeatureSpectralSpread:
    case CFeatureIf::kFeatureTimeStd:
    case CFeatureIf::kFeatureTimeZeroCrossingRate:
        pCInstance = new CFeatureFromBlockIf(eFeatureIdx, iDataLength, fSampleRate);
        break;

    case CFeatureIf::kFeatureSpectralFlux:
        pCInstance = new CFeatureSpectralFlux(eFeatureIdx, iDataLength, fSampleRate);
        break;

    case CFeatureIf::kFeatureSpectralMfccs:
        pCInstance = new CFeatureSpectralMfccs(eFeatureIdx, iDataLength, fSampleRate);
        break;

    case CFeatureIf::kFeatureSpectralPitchChroma:
        pCInstance = new CFeatureSpectralPitchChroma(eFeatureIdx, iDataLength, fSampleRate);
        break;

    case CFeatureIf::kFeatureSpectralRolloff:
        pCInstance = new CFeatureSpectralRolloff(eFeatureIdx, iDataLength, fSampleRate);
        break;

    case CFeatureIf::kFeatureSpectralTonalPowerRatio:
        pCInstance = new CFeatureSpectralTonalPowerRatio(eFeatureIdx, iDataLength, fSampleRate);
        break;

    case CFeatureIf::kFeatureTimeAcfCoeff:
        pCInstance = new CFeatureTimeAcfCoeff(eFeatureIdx, iDataLength, fSampleRate);
        break;

    case CFeatureIf::kFeatureTimeMaxAcf:
        pCInstance = new CFeatureTimeMaxAcf(eFeatureIdx, iDataLength, fSampleRate);
        break;

    case CFeatureIf::kFeatureTimePeakEnvelope:
        pCInstance = new CFeatureTimePeakEnvelope(eFeatureIdx, iDataLength, fSampleRate);
        break;

    case CFeatureIf::kFeatureTimeRms:
        pCInstance = new CFeatureTimeRms(eFeatureIdx, iDataLength, fSampleRate);
        break;
    }

    return Error_t::kNoError;
}

Error_t CFeatureFromBlockIf::destroy(CFeatureFromBlockIf *&pCInstance)
{
    delete pCInstance;

    pCInstance = 0;

    return Error_t::kNoError;
}

int CFeatureFromBlockIf::getFeatureDimensions() const
{
    // default: 1 value per block (can be overridden)
    return 1;
}

Error_t CFeatureFromBlockIf::compFeature(float *pfFeature, const float *pfIn)
{
    // default: use one of the static functions (can be overridden)
    *pfFeature = m_DispatchMap.at(m_eFeatureIdx)(pfIn, m_iDataLength, m_fSampleRate);

    return Error_t::kNoError;
}

bool CFeatureFromBlockIf::hasAdditionalParam() const
{
    // default: feature doesn't need any additional parameter (can be overridden)
    return false;
}

Error_t CFeatureFromBlockIf::setAdditionalParam(float /*fParamValue*/)
{
    // default: setting a parameter that doesn't exist (can be overridden)
    return Error_t::kFunctionIllegalCallError;
}
