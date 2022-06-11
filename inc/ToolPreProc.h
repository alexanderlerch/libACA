#if !defined(__ToolPreProc_hdr__)
#define __ToolPreProc_hdr__

#include <cassert>

#include "AudioFileIf.h"

#include "Vector.h"

/*! \brief class with static utility functions for pre-processing 
*/
class CPreProc
{
public:
    /*! downmixes multichannel audio (can be inplace
    \return void (no error returns b/c not user-facing)
    */
    static void downmix (float *pfOutput, float **ppfInput, int iNumChannels, long long iNumSamples)
    {
        // sanity checks
        assert(pfOutput);
        assert(ppfInput);
        assert(ppfInput[0]);
        assert(iNumChannels > 0);
        assert(iNumSamples > 0);

        // copy in case of not inplace processing
        if (pfOutput != ppfInput[0])
            CVector::copy(pfOutput, ppfInput[0], iNumSamples);

        // downmix
        for (auto c = 1; c < iNumChannels; c++)
            CVector::add_I(pfOutput, ppfInput[c], iNumSamples);
        CVector::mulC_I(pfOutput, 1.F / iNumChannels, iNumSamples);

        return;
    }
 };


/*! \brief class for audio normalization offering three options
* 1. normalize an audio file during blockwise reading with normalizeBlock (requires instance with audio file)
* 2. normalize an audio vector with blockwise reading with normalizeBlock (requires instance with buffer)
* 3. normalize an entire vector at once with normalizeSignal (static function does not require instance)
*/
class CNormalizeAudio
{
public:
    explicit CNormalizeAudio(CAudioFileIf* pCAudioFile) :
        m_fScaleFactor(1.F)
    {
        assert(pCAudioFile);

        CAudioFileIf::FileSpec_t stFileSpec;
        pCAudioFile->getFileSpec(stFileSpec);

        const int iBlockLength = 4096;
        float fGlobalMax = 0.F;
        long long iCurrPos = 0;
        float** ppfAudioData = 0;

        // alloc read buffer
        CVector::alloc(ppfAudioData, stFileSpec.iNumChannels);
        for (auto c = 0; c < stFileSpec.iNumChannels; c++)
            CVector::alloc(ppfAudioData[c], iBlockLength);

        // store current file position for resetting later
        pCAudioFile->getPosition(iCurrPos);

        pCAudioFile->getFileSpec(stFileSpec);

        // 
        pCAudioFile->setPosition(0.);
        while (!pCAudioFile->isEof())
        {
            // set block length variable
            long long iNumSamples = iBlockLength;
            float fMax = 0;

            // read data (iNumOfFrames might be updated!)
            pCAudioFile->readData(ppfAudioData, iNumSamples);

            //downmix if multichannel
            CPreProc::downmix(ppfAudioData[0], ppfAudioData, stFileSpec.iNumChannels, iNumSamples);

            // find max
            fMax = CVector::getMax(ppfAudioData[0], iNumSamples, true);
            if (fMax > fGlobalMax)
                fGlobalMax = fMax;
        }
        if (fGlobalMax > 0)
            m_fScaleFactor = 1.F / fGlobalMax;

        // reset file read position
        pCAudioFile->setPosition(iCurrPos);

        //free internal memory
        for (auto c = 0; c < stFileSpec.iNumChannels; c++)
            CVector::free(ppfAudioData[c]);
        CVector::free(ppfAudioData);
    };
    CNormalizeAudio(const float* pfAudioBuff, long long iAudioLength) :
        m_fScaleFactor(1.F)
    {
        assert(pfAudioBuff);
        assert(iAudioLength > 0);

        float fGlobalMax = CVector::getMax(pfAudioBuff, iAudioLength, true);
        if (fGlobalMax > 0)
            m_fScaleFactor = 1.F / fGlobalMax;
    };
    virtual ~CNormalizeAudio() {};

    /*! performs the normalization on a buffer after previous file parsing to get the maximum
    \param pfAudioBlock audio data, to be over-written
    \param  iNumSamples legnth of pfAudio
    \return Error_t
    */
    void normalizeBlock(float* pfAudioBlock, long long iNumSamples)
    {
        assert(pfAudioBlock);
        assert(iNumSamples > 0);

        CVector::mulC_I(pfAudioBlock, m_fScaleFactor, iNumSamples);
    };

    /*! performs the normalization inplace on a buffer
    \param pfAudio audio data, to be over-written
    \param  iNumSamples legnth of pfAudio
    \return Error_t
    */
    static void normalizeSignal(float* pfAudio, long long iNumSamples)
    {
        assert(pfAudio);
        assert(iNumSamples > 0);

        float fMax = CVector::getMax(pfAudio, iNumSamples, true);

        if (fMax > 0)
            CVector::mulC_I(pfAudio, 1.F / fMax, iNumSamples);

        return;
    }
private:
    CNormalizeAudio();
    CNormalizeAudio(const CNormalizeAudio& that);
    CNormalizeAudio& operator=(const CNormalizeAudio& c);

    float m_fScaleFactor;   //!< factor to normalize
};
#endif // __ToolPreProc_hdr__