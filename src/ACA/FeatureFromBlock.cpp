
#include "Vector.h"
#include "Util.h"

#include "ToolConversion.h"
#include "ToolCcf.h"

#include "FeatureFromBlock.h"

const float CFeatureFromBlockIf::m_kfFloatThresh = 1e-30F;      //!< below this we just assume it's zero

////////////////////////////////////////////////////////////////////////
// static member functions
float CFeatureFromBlockIf::compFeatureSpectralCentroid(const float* pfMagSpec, int iDataLength, float fSampleRate /*= 1.F*/)
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

    if (fNorm < m_kfFloatThresh)
        return 0;

    // convert from index to Hz
    return fvsc * fSampleRate / (2.F * fNorm * (iDataLength - 1));
}

float CFeatureFromBlockIf::compFeatureSpectralCrestFactor(const float* pfMagSpec, int iDataLength, float /*fSampleRate = 1.F*/)
{
    assert(pfMagSpec);
    assert(iDataLength > 0);

    float fNorm = CVectorFloat::getSum(pfMagSpec, iDataLength);

    if (fNorm < m_kfFloatThresh)
        return 0;

    return CVectorFloat::getMax(pfMagSpec, iDataLength) / fNorm;
}

float CFeatureFromBlockIf::compFeatureSpectralDecrease(const float* pfMagSpec, int iDataLength, float /*fSampleRate = 1.F*/)
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

    if (fNorm < m_kfFloatThresh)
        return 0;

    return fvsd / fNorm;
}

float CFeatureFromBlockIf::compFeatureSpectralFlatness(const float* pfMagSpec, int iDataLength, float /*fSampleRate = 1.F*/)
{
    assert(pfMagSpec);
    assert(iDataLength > 0);

    float fNorm = CVectorFloat::getMean(pfMagSpec, iDataLength);
    float fGeoMean = 0;

    // avoid unnecessary complications
    if (fNorm < m_kfFloatThresh || CVectorFloat::getMin(pfMagSpec, iDataLength) < m_kfFloatThresh)
        return 0;

    // compute geometric mean through log
    for (auto k = 0; k < iDataLength; k++)
        fGeoMean += std::log(pfMagSpec[k]);

    return std::exp(fGeoMean / iDataLength) / fNorm;
}

float CFeatureFromBlockIf::compFeatureSpectralFlux(const float* pfMagSpec, const float* pfPrevSpec, int iDataLength, float /*fSampleRate = 1.F*/)
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

float CFeatureFromBlockIf::compFeatureSpectralSpread(const float* pfMagSpec, int iDataLength, float fSampleRate /*= 1.F*/)
{
    assert(pfMagSpec);
    assert(iDataLength > 0);
    assert(fSampleRate > 0);

    float fNorm = 0;
    float fvss = 0;
    float fvsc = compFeatureSpectralCentroid(pfMagSpec, iDataLength, fSampleRate) * 2 / fSampleRate * (iDataLength - 1);

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

float CFeatureFromBlockIf::compFeatureSpectralKurtosis(const float* pfMagSpec, int iDataLength, float fSampleRate /*= 1.F*/)
{
    assert(pfMagSpec);
    assert(iDataLength > 0);
    assert(fSampleRate > 0);

    float fNorm = 0;
    float fvsk = 0;
    float fvsc = compFeatureSpectralCentroid(pfMagSpec, iDataLength, fSampleRate) * 2 / fSampleRate * (iDataLength - 1);
    float fvss = compFeatureSpectralSpread(pfMagSpec, iDataLength, fSampleRate) * 2 / fSampleRate * (iDataLength - 1);

    if (fvsc < m_kfFloatThresh || fvss < m_kfFloatThresh)
        return 0;

    for (auto k = 0; k < iDataLength; k++)
    {
        float fAgg = (k - fvsc) * (k - fvsc);
        fAgg *= fAgg;
        fNorm += pfMagSpec[k];
        fvsk += fAgg * pfMagSpec[k];
    }

    if (fNorm < m_kfFloatThresh)
        return 0;

    return fvsk = fvsk / (fvss * fvss * fvss * fvss * fNorm) - 3;
}

float CFeatureFromBlockIf::compFeatureSpectralRolloff(const float* pfMagSpec, int iDataLength, float fSampleRate /*= 1.F*/, float fKappa /*= .85F*/)
{
    assert(pfMagSpec);
    assert(iDataLength > 0);
    assert(fSampleRate > 0);
    assert(fKappa > 0 && fKappa <= 1);

    float fNorm = CVectorFloat::getSum(pfMagSpec, iDataLength);
    float fSum = 0;
    int k = 0;

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

float CFeatureFromBlockIf::compFeatureSpectralSkewness(const float* pfMagSpec, int iDataLength, float fSampleRate /*= 1.F*/)
{
    assert(pfMagSpec);
    assert(iDataLength > 0);
    assert(fSampleRate > 0);

    float fNorm = 0;
    float fvssk = 0;
    float fvsc = compFeatureSpectralCentroid(pfMagSpec, iDataLength, fSampleRate) * 2 / fSampleRate * (iDataLength - 1);
    float fvss = compFeatureSpectralSpread(pfMagSpec, iDataLength, fSampleRate) * 2 / fSampleRate * (iDataLength - 1);

    if (fvsc < m_kfFloatThresh || fvss < m_kfFloatThresh)
        return 0;

    for (auto k = 0; k < iDataLength; k++)
    {
        float fAgg = (k - fvsc) * (k - fvsc) * (k - fvsc);
        fNorm += pfMagSpec[k];
        fvssk += fAgg * pfMagSpec[k];
    }

    if (fNorm < m_kfFloatThresh)
        return 0;

    return fvssk = fvssk / (fvss * fvss * fvss * fNorm);
}

float CFeatureFromBlockIf::compFeatureSpectralSlope(const float* pfMagSpec, int iDataLength, float fSampleRate /*= 1.F*/)
{
    assert(pfMagSpec);
    assert(iDataLength > 0);
    assert(fSampleRate > 0);

    // compute mean
    float fvsc = compFeatureSpectralCentroid(pfMagSpec, iDataLength, fSampleRate) * 2 / fSampleRate * (iDataLength - 1);
    float fvssl = 0;
    float fNorm = 0;

    if (fvsc < m_kfFloatThresh)
        return 0;

    for (auto k = 0; k < iDataLength; k++)
    {
        float fk = k - (iDataLength + 1) / 2.F;
        fvssl += fk * (pfMagSpec[k] - fvsc);
        fNorm += fk * fk;
    }

    if (fNorm < m_kfFloatThresh)
        return 0;

    return fvssl / fNorm;
}
float CFeatureFromBlockIf::compFeatureSpectralTonalPowerRatio(const float* pfMagSpec, int iDataLength, float /*fSampleRate = 1.F*/, float fThresh /*= 5e-4F*/)
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

    if (fNorm < m_kfFloatThresh)
        return 0;

    return fvtpr / fNorm;
}

float CFeatureFromBlockIf::compFeatureTimeAcfCoeff(const float* pfSamples, int iDataLength, float /*fSampleRate = 1.F*/, int  iEta /*= 19*/)
{
    assert(pfSamples);
    assert(iDataLength > iEta);
    assert(iEta >= 0);

    return CVectorFloat::mulScalar(pfSamples, &pfSamples[iEta], iDataLength - iEta);
}

float CFeatureFromBlockIf::compFeatureTimePeakEnvelope(const float* pfSamples, int iDataLength, float /*fSampleRate = 1.F*/)
{
    assert(pfSamples);
    assert(iDataLength > 0);

    return CVectorFloat::getMax(pfSamples, iDataLength);
}

float CFeatureFromBlockIf::compFeatureTimeStd(const float* pfSamples, int iDataLength, float /*fSampleRate = 1.F*/)
{
    assert(pfSamples);
    assert(iDataLength > 0);

    return CVectorFloat::getStd(pfSamples, iDataLength);
}

float CFeatureFromBlockIf::compFeatureTimeRms(const float* pfSamples, int iDataLength, float /*fSampleRate = 1.F*/)
{
    assert(pfSamples);
    assert(iDataLength > 0);

    return CVectorFloat::getStd(pfSamples, iDataLength, 0.F);
}

float CFeatureFromBlockIf::compFeatureTimeZeroCrossingRate(const float* pfSamples, int iDataLength, float /*fSampleRate = 1.F*/)
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


///////////////////////////////////////////////////////////////////
// features that need "memory" so can't easily work as static functions
class CFeatureSpectralFlux : public CFeatureFromBlockIf
{
public:
    CFeatureSpectralFlux(Feature_t eFeatureIdx, int iDataLength, float fSampleRate) : CFeatureFromBlockIf(eFeatureIdx, iDataLength, fSampleRate)
    {
        m_pfPrevSpec = new float[m_iDataLength];
        CVectorFloat::setZero(m_pfPrevSpec, m_iDataLength);
    };

    virtual ~CFeatureSpectralFlux() 
    {
        delete[] m_pfPrevSpec;
        m_pfPrevSpec = 0;
    };

    Error_t calcFeatureFromBlock(float* pfFeature, const float* pfInput) override
    {
        *pfFeature = compFeatureSpectralFlux(pfInput, m_pfPrevSpec, m_iDataLength, m_fSampleRate);

        CVectorFloat::copy(m_pfPrevSpec, pfInput, m_iDataLength);

        return Error_t::kNoError;
    };

private:
    CFeatureSpectralFlux() {};
    CFeatureSpectralFlux(const CFeatureSpectralFlux& that);

    float* m_pfPrevSpec = 0;
};

class CFeatureSpectralMfccs : public CFeatureFromBlockIf
{
public:
    CFeatureSpectralMfccs(Feature_t eFeatureIdx, int iDataLength, float fSampleRate) : CFeatureFromBlockIf(eFeatureIdx, iDataLength, fSampleRate)
    {
        // alloc transfer function memory
        m_ppfH = new float* [m_iNumBands];
        for (auto p = 0; p < m_iNumBands; p++)
        {
            m_ppfH[p] = new float[iDataLength];
            CVectorFloat::setZero(m_ppfH[p], iDataLength);
        }
        genMfccFilters_();
        genDctMat_(m_iNumMfcCoeffs);

        m_pfMelSpec = new float [m_iNumBands];
    }

    virtual ~CFeatureSpectralMfccs()
    {
        for (auto p = 0; p < m_iNumBands; p++)
            delete[] m_ppfH[p];
        delete[] m_ppfH;
        m_ppfH = 0;

        deleteDctMat_();

        delete [] m_pfMelSpec;
        m_pfMelSpec = 0;
    }

    Error_t calcFeatureFromBlock(float* pfFeature, const float* pfInput) override
    {
        assert(pfFeature);
        assert(pfInput);

        CVectorFloat::setZero(pfFeature, m_iNumMfcCoeffs);

        // compute mel spectrum
        for (auto c = 0; c < m_iNumBands; c++)
            m_pfMelSpec[c] = std::log10(CVectorFloat::mulScalar(m_ppfH[c], pfInput, m_iDataLength) + 1e-20F);

        // compute dct
        for (auto j = 0; j < m_iNumMfcCoeffs; j++)
            pfFeature[j] = CVectorFloat::mulScalar(m_ppfDct[j], m_pfMelSpec, m_iNumBands);

         return Error_t::kNoError;
    };


    int getFeatureDimensions() const
    {
        return m_iNumMfcCoeffs;
    }

    bool hasAdditionalParam() const
    {
        return true;
    }

    Error_t setAdditionalParam(float fParamValue)
    {
        if (fParamValue <= 0)
            return Error_t::kFunctionInvalidArgsError;

        genDctMat_(CUtil::float2int<int>(fParamValue));

        return Error_t::kNoError;
    }
private:
    CFeatureSpectralMfccs() {};
    CFeatureSpectralMfccs(const CFeatureSpectralMfccs& that);

    void genMfccFilters_()
    {
        const double dFreq = 400. / 3.; 
        const int iNumLinFilters = 13,
            iNumLogFilters = 27;
        const double dLinSpacing = 200. / 3.,
            dLogSpacing = 1.0711703; // note sure where this mel resolution comes from exactly

        assert(m_iNumBands == iNumLinFilters + iNumLogFilters);
        double adBoundFreqs[3] = { dFreq, 
            dFreq + dLinSpacing,
            dFreq + 2. * dLinSpacing};
        int aiBoundIdx[3] = { static_cast<int>(CConversion::convertFreq2Bin(static_cast<float>(adBoundFreqs[0]), (m_iDataLength - 1) * 2, m_fSampleRate)),
            static_cast<int>(CConversion::convertFreq2Bin(static_cast<float>(adBoundFreqs[1]), (m_iDataLength - 1) * 2, m_fSampleRate)),
            static_cast<int>(CConversion::convertFreq2Bin(static_cast<float>(adBoundFreqs[2]), (m_iDataLength - 1) * 2, m_fSampleRate))};

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
            for (auto k = aiBoundIdx[1]+1; k <= aiBoundIdx[2]; k++)
            {
                float fFreqk = CConversion::convertBin2Freq(1.F * k, (m_iDataLength - 1) * 2, m_fSampleRate);
                //if ((afBoundFreqs[2] - fFreqk) < 0.F)
                //    continue;
                m_ppfH[c][k] = static_cast<float>(dFilterAmp * (adBoundFreqs[2] - fFreqk) / (adBoundFreqs[2] - adBoundFreqs[1]));
                assert(m_ppfH[c][k] >= 0);
            }

            // proceed to next band
            adBoundFreqs[0] = adBoundFreqs[1];
            adBoundFreqs[1] = adBoundFreqs[2];
            adBoundFreqs[2] = (c < iNumLinFilters-3) ? adBoundFreqs[1] + dLinSpacing : adBoundFreqs[2] * dLogSpacing; //!< Check me
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
                m_ppfDct[c][b] = static_cast<float>(std::cos(c * (2 * b + 1) * M_PI / 2. / m_iNumBands));

            CVectorFloat::mulC_I(m_ppfDct[c], 1/std::sqrt(m_iNumBands / 2.F), m_iNumBands);
        }
        CVectorFloat::mulC_I(m_ppfDct[0], 1.F / std::sqrt(2.f), m_iNumBands);

        m_iNumMfcCoeffs = iNumCoeffs;
    }
    void allocDctMat_(int iNumCoeffs)
    {
        if (m_ppfDct)
            deleteDctMat_();

        m_ppfDct = new float* [iNumCoeffs];
        for (auto c = 0; c < iNumCoeffs; c++)
        {
            m_ppfDct[c] = new float[m_iNumBands];
            CVectorFloat::setZero(m_ppfDct[c], m_iNumBands);
        }
    }
    void deleteDctMat_()
    {
        for (auto c = 0; c < m_iNumMfcCoeffs; c++)
            delete[] m_ppfDct[c];
        delete[] m_ppfDct;
        m_ppfDct = 0;
    }

    const int m_iNumBands = 40;
    int m_iNumMfcCoeffs = 13;

    float** m_ppfH = 0,
        **m_ppfDct = 0;

    float *m_pfMelSpec = 0;
};
class CFeatureSpectralPitchChroma : public CFeatureFromBlockIf
{
public:
    CFeatureSpectralPitchChroma(Feature_t eFeatureIdx, int iDataLength, float fSampleRate) : CFeatureFromBlockIf(eFeatureIdx, iDataLength, fSampleRate)
    {
        // alloc transfer function memory
        m_ppfH = new float* [m_iNumPitchClasses];
        for (auto p = 0; p < m_iNumPitchClasses; p++)
        {
            m_ppfH[p] = new float[iDataLength];
            CVectorFloat::setZero(m_ppfH[p], iDataLength);
        }

        genPcFilters_();
    };

    virtual ~CFeatureSpectralPitchChroma()
    {
        for (auto p = 0; p < m_iNumPitchClasses; p++)
            delete[] m_ppfH[p];
        delete[] m_ppfH;
        m_ppfH = 0;
    };

    Error_t calcFeatureFromBlock(float* pfFeature, const float* pfInput) override
    {
        assert(pfFeature);
        assert(pfInput);

        CVectorFloat::setZero(pfFeature, m_iNumPitchClasses);

        for (auto p = 0; p < m_iNumPitchClasses; p++)
        {
            // we could do this nicer with CVectorFloat::mulScalar if we allocated memory
            for (auto k = 0; k < m_iDataLength; k++)
            {
                pfFeature[p] += m_ppfH[p][k] * (pfInput[k] * pfInput[k]);
            }
        }

        float fSum = CVectorFloat::getSum(pfFeature, m_iNumPitchClasses);

        if (fSum > 0)
            CVectorFloat::mulC_I(pfFeature, 1.F/fSum, m_iNumPitchClasses);

        return Error_t::kNoError;
    };


    int getFeatureDimensions() const
    {
        return m_iNumPitchClasses;
    }

    bool hasAdditionalParam() const
    {
        return true;
    }

    Error_t setAdditionalParam(float fParamValue)
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
    CFeatureSpectralPitchChroma(const CFeatureSpectralPitchChroma& that);

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
                int aiBoundIdx[2] = { static_cast<int>(CConversion::convertFreq2Bin(afBoundFreqs[0], (m_iDataLength - 1) * 2, m_fSampleRate)) + 1,
                    static_cast<int>(CConversion::convertFreq2Bin(afBoundFreqs[1], (m_iDataLength - 1) * 2, m_fSampleRate)) };

                // set transfer function
                CVectorFloat::setValue(&m_ppfH[p][aiBoundIdx[0]], 1.F / (aiBoundIdx[1] - aiBoundIdx[0] + 1), aiBoundIdx[1] - aiBoundIdx[0] + 1);

                // proceed to next octave
                for (auto i = 0; i < 2; i++)
                    afBoundFreqs[i] *= 2.F;
            }

            // proceed to next pitch class
            fMid *= fQuarterToneRatio * fQuarterToneRatio;
        }
    }

    const int m_iNumPitchClasses = 12;

    const float m_fA4 = 440.F;
    int m_iNumOctaves = 4;

    float** m_ppfH = 0;
};

class CFeatureSpectralRolloff : public CFeatureFromBlockIf
{
public:
    CFeatureSpectralRolloff(Feature_t eFeatureIdx, int iDataLength, float fSampleRate) : CFeatureFromBlockIf(eFeatureIdx, iDataLength, fSampleRate) {};

    virtual ~CFeatureSpectralRolloff() {};

    Error_t calcFeatureFromBlock(float* pfFeature, const float* pfInput) override
    {
        *pfFeature = compFeatureSpectralRolloff(pfInput, m_iDataLength, m_fSampleRate, m_fKappa);

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
    CFeatureSpectralRolloff(const CFeatureSpectralRolloff& that);

    float m_fKappa = 0.85F;
};

class CFeatureSpectralTonalPowerRatio : public CFeatureFromBlockIf
{
public:
    CFeatureSpectralTonalPowerRatio(Feature_t eFeatureIdx, int iDataLength, float fSampleRate) : CFeatureFromBlockIf(eFeatureIdx, iDataLength, fSampleRate) {};

    virtual ~CFeatureSpectralTonalPowerRatio() {};

    Error_t calcFeatureFromBlock(float* pfFeature, const float* pfInput) override
    {
        *pfFeature = compFeatureSpectralTonalPowerRatio(pfInput, m_iDataLength, m_fSampleRate, m_fThresh);

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
    CFeatureSpectralTonalPowerRatio(const CFeatureSpectralTonalPowerRatio& that);

    float m_fThresh = 5e-4F;
};

class CFeatureTimePeakEnvelope : public CFeatureFromBlockIf
{
public:
    CFeatureTimePeakEnvelope(Feature_t eFeatureIdx, int iDataLength, float fSampleRate) : CFeatureFromBlockIf(eFeatureIdx, iDataLength, fSampleRate) {};

    virtual ~CFeatureTimePeakEnvelope() {};

    Error_t calcFeatureFromBlock(float* pfFeature, const float* pfInput) override
    {
        *pfFeature = compFeatureTimePeakEnvelope(pfInput, m_iDataLength, m_fSampleRate);

        return Error_t::kNoError;
    };

private:
    CFeatureTimePeakEnvelope() {};
    CFeatureTimePeakEnvelope(const CFeatureTimePeakEnvelope& that);
};

class CFeatureTimeAcfCoeff : public CFeatureFromBlockIf
{
public:
    CFeatureTimeAcfCoeff(Feature_t eFeatureIdx, int iDataLength, float fSampleRate) : CFeatureFromBlockIf(eFeatureIdx, iDataLength, fSampleRate) {};

    virtual ~CFeatureTimeAcfCoeff() {};

    Error_t calcFeatureFromBlock(float* pfFeature, const float* pfInput) override
    {
        *pfFeature = compFeatureTimeAcfCoeff(pfInput, m_iDataLength, m_fSampleRate, m_iEta);

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
    CFeatureTimeAcfCoeff(const CFeatureTimeAcfCoeff& that);

    int m_iEta = 19;
};

class CFeatureTimeMaxAcf : public CFeatureFromBlockIf
{
public:
    CFeatureTimeMaxAcf(Feature_t eFeatureIdx, int iDataLength, float fSampleRate) : CFeatureFromBlockIf(eFeatureIdx, iDataLength, fSampleRate)
    {
        m_pCCcf = new CCcf();
        m_pCCcf->init(iDataLength);

        m_pfAcf = new float[m_pCCcf->getCcfLength(true)];
    };

    virtual ~CFeatureTimeMaxAcf() 
    {
        delete[] m_pfAcf;
        m_pfAcf = 0;

        delete m_pCCcf;
        m_pCCcf = 0;
    };

    Error_t calcFeatureFromBlock(float* pfFeature, const float* pfInput) override
    {
        float fMinThresh = 0.35F;

        int iEta = 0,
            iEtaMin = static_cast<int>(m_fSampleRate / m_fMax);

        m_pCCcf->calcCcf(pfInput, pfInput, true);
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
            if (iEta >= m_iDataLength)
                break;
        }
        

        if (iEta >= m_iDataLength)
            iEtaMin = 0;
        else if (iEtaMin < iEta)
            iEtaMin = iEta;

        // get the maximum given the constraints above
        *pfFeature = CVectorFloat::getMax(&m_pfAcf[iEtaMin], m_iDataLength - iEtaMin);

        return Error_t::kNoError;
    };

    bool hasAdditionalParam() const override
    {
        return true;
    }

    Error_t setAdditionalParam(float fParamValue) override
    {
        if (fParamValue <= 0 || fParamValue > m_fSampleRate/2)
            return Error_t::kNoError;

        m_fMax = fParamValue;

        return Error_t::kNoError;
    }

private:
    CFeatureTimeMaxAcf() {};
    CFeatureTimeMaxAcf(const CFeatureTimeMaxAcf& that);

    CCcf *m_pCCcf = 0;
    float* m_pfAcf = 0;

    float m_fMax = 2000.F;
};

class CFeatureTimeRms : public CFeatureFromBlockIf
{
public:
    CFeatureTimeRms(Feature_t eFeatureIdx, int iDataLength, float fSampleRate) : CFeatureFromBlockIf(eFeatureIdx, iDataLength, fSampleRate) {};

    virtual ~CFeatureTimeRms() {};

    Error_t calcFeatureFromBlock(float* pfFeature, const float* pfInput) override
    {
        *pfFeature = compFeatureTimeRms(pfInput, m_iDataLength, m_fSampleRate);

        return Error_t::kNoError;
    };

private:
    CFeatureTimeRms() {};
    CFeatureTimeRms(const CFeatureTimeRms& that);
};

///////////////////////////////////////////////////////////////////
// normal member functions
Error_t CFeatureFromBlockIf::create(CFeatureFromBlockIf*& pCInstance, Feature_t eFeatureIdx, int iDataLength, float fSampleRate)
{
    if (iDataLength <= 0 || fSampleRate <= 0)
        return Error_t::kFunctionInvalidArgsError;


    switch (eFeatureIdx)
    {
    default:
    case kFeatureSpectralCentroid:
    case kFeatureSpectralCrestFactor:
    case kFeatureSpectralDecrease:
    case kFeatureSpectralFlatness:
    case kFeatureSpectralKurtosis:
    case kFeatureSpectralSkewness:
    case kFeatureSpectralSlope:
    case kFeatureSpectralSpread:
    case kFeatureTimeStd:
    case kFeatureTimeZeroCrossingRate:
        pCInstance = new CFeatureFromBlockIf(eFeatureIdx, iDataLength, fSampleRate);
        break;

    case kFeatureSpectralFlux:
        pCInstance = new CFeatureSpectralFlux(eFeatureIdx, iDataLength, fSampleRate);
        break;

    case kFeatureSpectralMfccs:
        pCInstance = new CFeatureSpectralMfccs(eFeatureIdx, iDataLength, fSampleRate);
        break;

    case kFeatureSpectralPitchChroma:
        pCInstance = new CFeatureSpectralPitchChroma(eFeatureIdx, iDataLength, fSampleRate);
        break;

    case kFeatureSpectralRolloff:
        pCInstance = new CFeatureSpectralRolloff(eFeatureIdx, iDataLength, fSampleRate);
        break;

    case kFeatureSpectralTonalPowerRatio:
        pCInstance = new CFeatureSpectralTonalPowerRatio(eFeatureIdx, iDataLength, fSampleRate);
        break;

    case kFeatureTimeAcfCoeff:
        pCInstance = new CFeatureTimeAcfCoeff(eFeatureIdx, iDataLength, fSampleRate);
        break;

    case kFeatureTimeMaxAcf:
        pCInstance = new CFeatureTimeMaxAcf(eFeatureIdx, iDataLength, fSampleRate);
        break;

    case kFeatureTimePeakEnvelope:
        pCInstance = new CFeatureTimePeakEnvelope(eFeatureIdx, iDataLength, fSampleRate);
        break;

    case kFeatureTimeRms:
        pCInstance = new CFeatureTimeRms(eFeatureIdx, iDataLength, fSampleRate);
        break;
    }

    return Error_t::kNoError;
}

Error_t CFeatureFromBlockIf::destroy(CFeatureFromBlockIf*& pCInstance)
{
    delete pCInstance;

    return Error_t::kNoError;
}

int CFeatureFromBlockIf::getFeatureDimensions() const
{
    // default: 1 value per block
    return 1;
}

Error_t CFeatureFromBlockIf::calcFeatureFromBlock(float* pfFeature, const float* pfInput)
{
    // default: use one of the static functions
    *pfFeature = m_DispatchMap.at(m_eFeatureIdx)(pfInput, m_iDataLength, m_fSampleRate);

    return Error_t::kNoError;
}

bool CFeatureFromBlockIf::hasAdditionalParam() const
{
    // default: feature doesn't need any additional parameters
    return false;
}

Error_t CFeatureFromBlockIf::setAdditionalParam(float /*fParamValue*/)
{
    // default: setting a parameter that doesn't exist
    return Error_t::kFunctionIllegalCallError;
}
