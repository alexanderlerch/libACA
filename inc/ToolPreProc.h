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
    static void downmix (float *pfOutput, float **ppfInput, int iNumChannels, long long iNumFrames)
    {
        // sanity checks
        assert(pfOutput);
        assert(ppfInput);
        assert(ppfInput[0]);
        assert(iNumChannels > 0);
        assert(iNumFrames > 0);

        // copy in case of not inplace processing
        if (pfOutput != ppfInput[0])
            CVectorFloat::copy(pfOutput, ppfInput[0], iNumFrames);

        // downmix
        for (auto c = 1; c < iNumChannels; c++)
            CVectorFloat::add_I(pfOutput, ppfInput[c], iNumFrames);
        CVectorFloat::mulC_I(pfOutput, 1.F / iNumChannels, iNumFrames);

        return;
    }
 };


/*! \brief class for audio normalization offering three options
* 1. normalize an audio file during blockwise reading with normalizeBlock (requires instance with audio file)
* 2. normalize an audio vector with blockwise reading with normalizeBlock (requires instance with buffer)
* 3. normalize an entire vector at once with normalizeWithinVec (static function does not require instance)
*/
class CNormalizeAudio
{
public:
    CNormalizeAudio(CAudioFileIf* pCAudioFile) :
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
        ppfAudioData = new float *[stFileSpec.iNumChannels];
        for (auto c = 0; c < stFileSpec.iNumChannels; c++)
            ppfAudioData[c] = new float [iBlockLength];

        // store current file position for resetting later
        pCAudioFile->getPosition(iCurrPos);

        pCAudioFile->getFileSpec(stFileSpec);

        // 
        pCAudioFile->setPosition(0.);
        while (!pCAudioFile->isEof())
        {
            // set block length variable
            long long iNumFrames = iBlockLength;
            float fMax = 0;

            // read data (iNumOfFrames might be updated!)
            pCAudioFile->readData(ppfAudioData, iNumFrames);

            //downmix if multichannel
            CPreProc::downmix(ppfAudioData[0], ppfAudioData, stFileSpec.iNumChannels, iNumFrames);

            // find max
            fMax = CVectorFloat::getMax(ppfAudioData[0], iNumFrames, true);
            if (fMax > fGlobalMax)
                fGlobalMax = fMax;
        }
        if (fGlobalMax > 0)
            m_fScaleFactor = 1.F / fGlobalMax;

        // reset file read position
        pCAudioFile->setPosition(iCurrPos);

        //free internal memory
        for (auto c = 0; c < stFileSpec.iNumChannels; c++)
            delete[] ppfAudioData[c];
        delete[] ppfAudioData;
        ppfAudioData = 0;
    };
    CNormalizeAudio(const float* pfAudioBuff, long long iAudioLength) :
        m_fScaleFactor(1.F)
    {
        assert(pfAudioBuff);
        assert(iAudioLength > 0);

        float fGlobalMax = CVectorFloat::getMax(pfAudioBuff, iAudioLength, true);
        if (fGlobalMax > 0)
            m_fScaleFactor = 1.F / fGlobalMax;
    };
    virtual ~CNormalizeAudio() {};

    /*! performs the normalization on a buffer after previous file parsing to get the maximum
    \param pfAudio audio data, to be over-written
    \param  iNumFrames legnth of pfAudio
    \return Error_t
    */
    void normalizeBlock(float* pfAudio, long long iNumFrames)
    {
        assert(pfAudio);
        assert(iNumFrames > 0);

        CVectorFloat::mulC_I(pfAudio, m_fScaleFactor, iNumFrames);
    };

    /*! performs the normalization inplace on a buffer
    \param pfAudio audio data, to be over-written
    \param  iNumFrames legnth of pfAudio
    \return Error_t
    */
    static void normalizeWithinVec(float* pfAudio, long long iNumFrames)
    {
        assert(pfAudio);
        assert(iNumFrames > 0);

        float fMax = CVectorFloat::getMax(pfAudio, iNumFrames, true);

        if (fMax > 0)
            CVectorFloat::mulC_I(pfAudio, 1.F / fMax, iNumFrames);

        return;
    }
private:
    CNormalizeAudio();
    float m_fScaleFactor;   //!< factor to normalize
};
#endif // __ToolPreProc_hdr__