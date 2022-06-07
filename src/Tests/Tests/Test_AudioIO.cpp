#include "AcaAll.h"

#ifdef WITH_TESTS

#include "catch.hpp"

extern std::string cTestDataDir;

namespace AudioIoTest 
{

    void deleteFile(std::string cExt)
    {
        // cleanup: delete file from disk -- permissions problem under win
        remove((cTestDataDir + "/test" + cExt).c_str());
    }
}

TEST_CASE("AudioIo", "[AudioIo]")
{
    float** ppfAudioData = 0;
    float** ppfTmp = 0;
    CAudioFileIf* pCAudioFile = 0;
    CAudioFileIf::FileSpec_t stFileSpec;

    const int iBuffLength = 1027;
    const int iBlockLength = 17;
    const int iNumChannels = 2;

    float fPhase = 0;

    stFileSpec.eBitStreamType = CAudioFileIf::kFileBitStreamInt16;
    stFileSpec.eFormat = CAudioFileIf::kFileFormatRaw;
    stFileSpec.fSampleRateInHz = 44100.F;
    stFileSpec.iNumChannels = iNumChannels;

    CAudioFileIf::create(pCAudioFile);
    ppfAudioData = 0;
    ppfTmp = 0;
    ppfAudioData = new float* [iNumChannels];
    ppfTmp = new float* [iNumChannels];
    for (int i = 0; i < iNumChannels; i++)
        ppfAudioData[i] = new float[iBuffLength];

    for (int i = 0; i < iNumChannels; i++)
    {
        CSynthesis::genSine(ppfAudioData[i], 441.F, stFileSpec.fSampleRateInHz, iBuffLength, .6F, fPhase);
        fPhase += static_cast<float>(M_PI_2);
    }

    SECTION("read raw")
    {
        if (cTestDataDir.empty())
        {
            CHECK(false);
        }
        long long int iReadIdx = 0;
        float** ppfReadData = new float* [iNumChannels];
        for (int i = 0; i < iNumChannels; i++)
            ppfReadData[i] = new float[iBuffLength];

        pCAudioFile->openFile(cTestDataDir + "/ref.pcm", CAudioFileIf::kFileRead, &stFileSpec);

        while (!pCAudioFile->isEof())
        {
            long long int iNumRead = iBlockLength;
            for (int i = 0; i < iNumChannels; i++)
                ppfTmp[i] = &ppfReadData[i][iReadIdx];

            pCAudioFile->readData(ppfTmp, iNumRead);
            iReadIdx += iNumRead;
        }

        // check identity
        for (int c = 0; c < iNumChannels; c++)
        {
            for (auto i = 0; i < iBuffLength; i++)
                CHECK(ppfAudioData[c][i] == Approx(ppfReadData[c][i]).margin(1e-3F).epsilon(1e-3F));
        }

        // free allocated memory
        for (int i = 0; i < iNumChannels; i++)
            delete[] ppfReadData[i];
        delete[] ppfReadData;
    }

    SECTION("read raw offset")
    {
        const long long iOffset = 327;
        long long int iReadIdx = 0;
        float** ppfReadData = new float* [iNumChannels];
        for (int i = 0; i < iNumChannels; i++)
            ppfReadData[i] = new float[iBuffLength];

        pCAudioFile->openFile(cTestDataDir + "/ref.pcm", CAudioFileIf::kFileRead, &stFileSpec);

        pCAudioFile->setPosition(iOffset);

        while (!pCAudioFile->isEof())
        {
            long long int iNumRead = iBlockLength;
            for (int i = 0; i < iNumChannels; i++)
                ppfTmp[i] = &ppfReadData[i][iReadIdx];

            pCAudioFile->readData(ppfTmp, iNumRead);
            iReadIdx += iNumRead;
        }

        // check identity
        for (int c = 0; c < iNumChannels; c++)
        {
            for (auto i = 0; i < iBuffLength - iOffset; i++)
                CHECK(ppfAudioData[c][i+iOffset] == Approx(ppfReadData[c][i]).margin(1e-3F).epsilon(1e-3F));
        }

        // free allocated memory
        for (int i = 0; i < iNumChannels; i++)
            delete[] ppfReadData[i];
        delete[] ppfReadData;
    }

    SECTION("read all at once")
    {
        //note that the file length is longer than the internal read block size
        long long   iFileLength = 0;
        long long   iNumRead = 0;
        float** ppfReadData = new float* [iNumChannels];

        for (int i = 0; i < iNumChannels; i++)
            ppfReadData[i] = new float[iBuffLength];

        pCAudioFile->openFile(cTestDataDir + "/ref.pcm", CAudioFileIf::kFileRead, &stFileSpec);
        pCAudioFile->getLength(iFileLength);
        iNumRead = static_cast<int>(iFileLength);

        pCAudioFile->readData(ppfReadData, iNumRead);

        // check identity
        CHECK(iNumRead == iFileLength);
        for (int c = 0; c < iNumChannels; c++)
        {
            for (auto i = 0; i < iBuffLength; i++)
                CHECK(ppfAudioData[c][i] == Approx(ppfReadData[c][i]).margin(1e-3F).epsilon(1e-3F));
        }

        // free allocated memory
        for (int i = 0; i < iNumChannels; i++)
            delete[] ppfReadData[i];
        delete[] ppfReadData;
    }

    SECTION("read write raw")
    {
        int iNumRemainingFrames = iBuffLength;
        Error_t err = Error_t::kUnknownError;

        err = pCAudioFile->openFile(cTestDataDir + "/test.pcm", CAudioFileIf::kFileWrite, &stFileSpec);
        CHECK(err == Error_t::kNoError);

        // put data
        while (iNumRemainingFrames > 0)
        {
            for (int i = 0; i < iNumChannels; i++)
                ppfTmp[i] = &ppfAudioData[i][iBuffLength - iNumRemainingFrames];
            int iPutFrames = std::min(iBlockLength, iNumRemainingFrames);

            pCAudioFile->writeData(ppfTmp, iPutFrames);

            iNumRemainingFrames -= iPutFrames;
        }
        pCAudioFile->closeFile();
        pCAudioFile->reset();

        // read the file and compare
        long long int iReadIdx = 0;
        float** ppfReadData = new float* [iNumChannels];
        for (int i = 0; i < iNumChannels; i++)
            ppfReadData[i] = new float[iBuffLength];

        err = pCAudioFile->openFile(cTestDataDir + "/test.pcm", CAudioFileIf::kFileRead, &stFileSpec);
        CHECK(err == Error_t::kNoError);

        while (!pCAudioFile->isEof())
        {
            long long int iNumRead = iBlockLength;
            for (int i = 0; i < iNumChannels; i++)
                ppfTmp[i] = &ppfReadData[i][iReadIdx];

            pCAudioFile->readData(ppfTmp, iNumRead);
            iReadIdx += iNumRead;
        }

        // check identity
        for (int c = 0; c < iNumChannels; c++)
        {
            for (auto i = 0; i < iBuffLength; i++)
                CHECK(ppfAudioData[c][i] == Approx(ppfReadData[c][i]).margin(1e-3F).epsilon(1e-3F));
        }

        // free allocated memory
        for (int i = 0; i < iNumChannels; i++)
            delete[] ppfReadData[i];
        delete[] ppfReadData;

        pCAudioFile->closeFile();


        // cleanup
        AudioIoTest::deleteFile(".pcm");
    }

    SECTION("read write wav")
    {
        Error_t err = Error_t::kUnknownError;
        const std::string cExt(".wav");

        // write wave file
        int iNumRemainingFrames = iBuffLength;

        stFileSpec.eFormat = CAudioFileIf::kFileFormatWav;

        err = pCAudioFile->openFile(cTestDataDir + "/test" + cExt, CAudioFileIf::kFileWrite, &stFileSpec);
        CHECK(err == Error_t::kNoError);

        // put data
        while (iNumRemainingFrames > 0)
        {
            for (int i = 0; i < iNumChannels; i++)
                ppfTmp[i] = &ppfAudioData[i][iBuffLength - iNumRemainingFrames];
            int iPutFrames = std::min(iBlockLength, iNumRemainingFrames);

            pCAudioFile->writeData(ppfTmp, iPutFrames);

            iNumRemainingFrames -= iPutFrames;
        }
        pCAudioFile->closeFile();
        pCAudioFile->reset();

        // read the file and compare
        long long iReadIdx = 0;
        float** ppfReadData = new float* [iNumChannels];
        for (int i = 0; i < iNumChannels; i++)
            ppfReadData[i] = new float[iBuffLength];

        err = pCAudioFile->openFile(cTestDataDir + "/test" + cExt, CAudioFileIf::kFileRead, &stFileSpec);
        CHECK(err == Error_t::kNoError);

        while (!pCAudioFile->isEof())
        {
            long long iNumRead = iBlockLength;
            for (int i = 0; i < iNumChannels; i++)
                ppfTmp[i] = &ppfReadData[i][iReadIdx];

            pCAudioFile->readData(ppfTmp, iNumRead);
            iReadIdx += iNumRead;
        }

        // check identity
        for (int c = 0; c < iNumChannels; c++)
        {
            for (auto i = 0; i < iBuffLength; i++)
                CHECK(ppfAudioData[c][i] == Approx(ppfReadData[c][i]).margin(1e-3F).epsilon(1e-3F));
        }

        // free allocated memory
        for (int i = 0; i < iNumChannels; i++)
            delete[] ppfReadData[i];
        delete[] ppfReadData;

        pCAudioFile->closeFile();

        // cleanup
        AudioIoTest::deleteFile(cExt);
    }

    SECTION("file spec")
    {
        const std::string cExt(".wav");
        Error_t err = Error_t::kUnknownError;

        // write wave file
        int iNumRemainingFrames = iBuffLength;

        stFileSpec.eFormat = CAudioFileIf::kFileFormatWav;

        err = pCAudioFile->openFile(cTestDataDir + "/test" + cExt, CAudioFileIf::kFileWrite, &stFileSpec);
        CHECK(err == Error_t::kNoError);

        // put data
        while (iNumRemainingFrames > 0)
        {
            for (int i = 0; i < iNumChannels; i++)
                ppfTmp[i] = &ppfAudioData[i][iBuffLength - iNumRemainingFrames];
            int iPutFrames = std::min(iBlockLength, iNumRemainingFrames);

            pCAudioFile->writeData(ppfTmp, iPutFrames);

            iNumRemainingFrames -= iPutFrames;
        }
        pCAudioFile->closeFile();
        pCAudioFile->reset();

        err = pCAudioFile->openFile(cTestDataDir + "/test" + cExt, CAudioFileIf::kFileRead);
        CHECK(err == Error_t::kNoError);

        pCAudioFile->getFileSpec(stFileSpec);

        CHECK(44100 == stFileSpec.fSampleRateInHz);
        CHECK(CAudioFileIf::kFileFormatWav == stFileSpec.eFormat);

        pCAudioFile->closeFile();

        // cleanup
        AudioIoTest::deleteFile(cExt);
    }


    assert(ppfAudioData != 0);
    for (int i = 0; i < iNumChannels; i++)
        delete[] ppfAudioData[i];
    delete[] ppfAudioData;
    delete[] ppfTmp;

    CAudioFileIf::destroy(pCAudioFile);

}

#endif //WITH_TESTS
