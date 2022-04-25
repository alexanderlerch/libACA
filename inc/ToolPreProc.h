#if !defined(__ToolPreProc_hdr__)
#define __ToolPreProc_hdr__

#include <cassert>

#include "AudioFileIf.h"

#include "helper/Vector.h"

/*! \brief class with static utility functions 
*/
class CPreProc
{
public:
    /*! downmixes multichannel audio (can be inplace
    \return void (no error returns b/c not user-facing)
    */
    static void downmix (float *pfOutput, float **ppfInput, int iNumChannels, int iNumFrames)
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


class CNormalizeAudio
{
public:
    CNormalizeAudio(CAudioFileIf* pCAudioFile) :
        m_fScaleFactor(1.F)
    {
        assert(pCAudioFile);

        CAudioFileIf::FileSpec_t stFileSpec;

        const int iBlockLength = 4096;
        float fGlobalMax = 0.F;
        long long iCurrPos = 0;
        float** ppfAudioData = 0;

        // alloc read buffer
        ppfAudioData = new float *[stFileSpec.iNumChannels];
        for (auto c = 0; c < stFileSpec.iNumChannels; c++)
            ppfAudioData[c] = new float [iBlockLength];

        pCAudioFile->getPosition(iCurrPos);

        pCAudioFile->getFileSpec(stFileSpec);

        pCAudioFile->setPosition(0.);
        while (!pCAudioFile->isEof())
        {
            // set block length variable
            long long iNumFrames = iBlockLength;
            float fMax = 0;

            // read data (iNumOfFrames might be updated!)
            pCAudioFile->readData(ppfAudioData, iNumFrames);

            CPreProc::downmix(ppfAudioData[0], ppfAudioData, stFileSpec.iNumChannels, iNumFrames);
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
    virtual ~CNormalizeAudio() {};

    void normalizeBlock(float* pfAudio, int iNumFrames)
    {
        assert(pfAudio);
        assert(iNumFrames > 0);

        CVectorFloat::mulC_I(pfAudio, m_fScaleFactor, iNumFrames);
    };
private:
    CNormalizeAudio();
    float m_fScaleFactor;
};
#endif // __ToolPreProc_hdr__