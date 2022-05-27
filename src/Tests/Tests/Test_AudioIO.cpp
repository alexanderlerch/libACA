#include "ACAConfig.h"


#ifdef WITH_TESTS
#include "Synthesis.h"
#include "AudioFileIf.h"

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
    float** m_ppfAudioData = 0;
    float** m_ppfTmp = 0;
    CAudioFileIf* m_pCAudioFile = 0;
    CAudioFileIf::FileSpec_t m_stFileSpec;

    const int m_iBuffLength = 1027;
    const int m_iBlockLength = 17;
    const int m_iNumChannels = 2;

    float fPhase = 0;

    m_stFileSpec.eBitStreamType = CAudioFileIf::kFileBitStreamInt16;
    m_stFileSpec.eFormat = CAudioFileIf::kFileFormatRaw;
    m_stFileSpec.fSampleRateInHz = 44100.F;
    m_stFileSpec.iNumChannels = m_iNumChannels;

    CAudioFileIf::create(m_pCAudioFile);
    m_ppfAudioData = 0;
    m_ppfTmp = 0;
    m_ppfAudioData = new float* [m_iNumChannels];
    m_ppfTmp = new float* [m_iNumChannels];
    for (int i = 0; i < m_iNumChannels; i++)
        m_ppfAudioData[i] = new float[m_iBuffLength];

    for (int i = 0; i < m_iNumChannels; i++)
    {
        CSynthesis::genSine(m_ppfAudioData[i], 441.F, m_stFileSpec.fSampleRateInHz, m_iBuffLength, .6F, fPhase);
        fPhase += static_cast<float>(M_PI_2);
    }

    SECTION("read raw")
    {
        if (cTestDataDir.empty())
        {
            CHECK(false);
        }
        long long int iReadIdx = 0;
        float** ppfReadData = new float* [m_iNumChannels];
        for (int i = 0; i < m_iNumChannels; i++)
            ppfReadData[i] = new float[m_iBuffLength];

        m_pCAudioFile->openFile(cTestDataDir + "/ref.pcm", CAudioFileIf::kFileRead, &m_stFileSpec);

        while (!m_pCAudioFile->isEof())
        {
            long long int iNumRead = m_iBlockLength;
            for (int i = 0; i < m_iNumChannels; i++)
                m_ppfTmp[i] = &ppfReadData[i][iReadIdx];

            m_pCAudioFile->readData(m_ppfTmp, iNumRead);
            iReadIdx += iNumRead;
        }

        // check identity
        for (int c = 0; c < m_iNumChannels; c++)
        {
            for (auto i = 0; i < m_iBuffLength; i++)
                CHECK(m_ppfAudioData[c][i] == Approx(ppfReadData[c][i]).margin(1e-3F).epsilon(1e-3F));
        }

        // free allocated memory
        for (int i = 0; i < m_iNumChannels; i++)
            delete[] ppfReadData[i];
        delete[] ppfReadData;
    }

    SECTION("read raw offset")
    {
        const long long iOffset = 327;
        long long int iReadIdx = 0;
        float** ppfReadData = new float* [m_iNumChannels];
        for (int i = 0; i < m_iNumChannels; i++)
            ppfReadData[i] = new float[m_iBuffLength];

        m_pCAudioFile->openFile(cTestDataDir + "/ref.pcm", CAudioFileIf::kFileRead, &m_stFileSpec);

        m_pCAudioFile->setPosition(iOffset);

        while (!m_pCAudioFile->isEof())
        {
            long long int iNumRead = m_iBlockLength;
            for (int i = 0; i < m_iNumChannels; i++)
                m_ppfTmp[i] = &ppfReadData[i][iReadIdx];

            m_pCAudioFile->readData(m_ppfTmp, iNumRead);
            iReadIdx += iNumRead;
        }

        // check identity
        for (int c = 0; c < m_iNumChannels; c++)
        {
            for (auto i = 0; i < m_iBuffLength - iOffset; i++)
                CHECK(m_ppfAudioData[c][i+iOffset] == Approx(ppfReadData[c][i]).margin(1e-3F).epsilon(1e-3F));
        }

        // free allocated memory
        for (int i = 0; i < m_iNumChannels; i++)
            delete[] ppfReadData[i];
        delete[] ppfReadData;
    }

    SECTION("read all at once")
    {
        //note that the file length is longer than the internal read block size
        long long   iFileLength = 0;
        long long   iNumRead = 0;
        float** ppfReadData = new float* [m_iNumChannels];

        for (int i = 0; i < m_iNumChannels; i++)
            ppfReadData[i] = new float[m_iBuffLength];

        m_pCAudioFile->openFile(cTestDataDir + "/ref.pcm", CAudioFileIf::kFileRead, &m_stFileSpec);
        m_pCAudioFile->getLength(iFileLength);
        iNumRead = static_cast<int>(iFileLength);

        m_pCAudioFile->readData(ppfReadData, iNumRead);

        // check identity
        CHECK(iNumRead == iFileLength);
        for (int c = 0; c < m_iNumChannels; c++)
        {
            for (auto i = 0; i < m_iBuffLength; i++)
                CHECK(m_ppfAudioData[c][i] == Approx(ppfReadData[c][i]).margin(1e-3F).epsilon(1e-3F));
        }

        // free allocated memory
        for (int i = 0; i < m_iNumChannels; i++)
            delete[] ppfReadData[i];
        delete[] ppfReadData;
    }

    SECTION("read write raw")
    {
        int iNumRemainingFrames = m_iBuffLength;
        Error_t err = Error_t::kUnknownError;

        err = m_pCAudioFile->openFile(cTestDataDir + "/test.pcm", CAudioFileIf::kFileWrite, &m_stFileSpec);
        CHECK(err == Error_t::kNoError);

        // put data
        while (iNumRemainingFrames > 0)
        {
            for (int i = 0; i < m_iNumChannels; i++)
                m_ppfTmp[i] = &m_ppfAudioData[i][m_iBuffLength - iNumRemainingFrames];
            int iPutFrames = std::min(m_iBlockLength, iNumRemainingFrames);

            m_pCAudioFile->writeData(m_ppfTmp, iPutFrames);

            iNumRemainingFrames -= iPutFrames;
        }
        m_pCAudioFile->closeFile();
        m_pCAudioFile->reset();

        // read the file and compare
        long long int iReadIdx = 0;
        float** ppfReadData = new float* [m_iNumChannels];
        for (int i = 0; i < m_iNumChannels; i++)
            ppfReadData[i] = new float[m_iBuffLength];

        err = m_pCAudioFile->openFile(cTestDataDir + "/test.pcm", CAudioFileIf::kFileRead, &m_stFileSpec);
        CHECK(err == Error_t::kNoError);

        while (!m_pCAudioFile->isEof())
        {
            long long int iNumRead = m_iBlockLength;
            for (int i = 0; i < m_iNumChannels; i++)
                m_ppfTmp[i] = &ppfReadData[i][iReadIdx];

            m_pCAudioFile->readData(m_ppfTmp, iNumRead);
            iReadIdx += iNumRead;
        }

        // check identity
        for (int c = 0; c < m_iNumChannels; c++)
        {
            for (auto i = 0; i < m_iBuffLength; i++)
                CHECK(m_ppfAudioData[c][i] == Approx(ppfReadData[c][i]).margin(1e-3F).epsilon(1e-3F));
        }

        // free allocated memory
        for (int i = 0; i < m_iNumChannels; i++)
            delete[] ppfReadData[i];
        delete[] ppfReadData;

        m_pCAudioFile->closeFile();


        // cleanup
        AudioIoTest::deleteFile(".pcm");
    }

    SECTION("read write wav")
    {
        Error_t err = Error_t::kUnknownError;
        const std::string cExt(".wav");

        // write wave file
        int iNumRemainingFrames = m_iBuffLength;

        m_stFileSpec.eFormat = CAudioFileIf::kFileFormatWav;

        err = m_pCAudioFile->openFile(cTestDataDir + "/test" + cExt, CAudioFileIf::kFileWrite, &m_stFileSpec);
        CHECK(err == Error_t::kNoError);

        // put data
        while (iNumRemainingFrames > 0)
        {
            for (int i = 0; i < m_iNumChannels; i++)
                m_ppfTmp[i] = &m_ppfAudioData[i][m_iBuffLength - iNumRemainingFrames];
            int iPutFrames = std::min(m_iBlockLength, iNumRemainingFrames);

            m_pCAudioFile->writeData(m_ppfTmp, iPutFrames);

            iNumRemainingFrames -= iPutFrames;
        }
        m_pCAudioFile->closeFile();
        m_pCAudioFile->reset();

        // read the file and compare
        long long iReadIdx = 0;
        float** ppfReadData = new float* [m_iNumChannels];
        for (int i = 0; i < m_iNumChannels; i++)
            ppfReadData[i] = new float[m_iBuffLength];

        err = m_pCAudioFile->openFile(cTestDataDir + "/test" + cExt, CAudioFileIf::kFileRead, &m_stFileSpec);
        CHECK(err == Error_t::kNoError);

        while (!m_pCAudioFile->isEof())
        {
            long long iNumRead = m_iBlockLength;
            for (int i = 0; i < m_iNumChannels; i++)
                m_ppfTmp[i] = &ppfReadData[i][iReadIdx];

            m_pCAudioFile->readData(m_ppfTmp, iNumRead);
            iReadIdx += iNumRead;
        }

        // check identity
        for (int c = 0; c < m_iNumChannels; c++)
        {
            for (auto i = 0; i < m_iBuffLength; i++)
                CHECK(m_ppfAudioData[c][i] == Approx(ppfReadData[c][i]).margin(1e-3F).epsilon(1e-3F));
        }

        // free allocated memory
        for (int i = 0; i < m_iNumChannels; i++)
            delete[] ppfReadData[i];
        delete[] ppfReadData;

        m_pCAudioFile->closeFile();

        // cleanup
        AudioIoTest::deleteFile(cExt);
    }

    SECTION("file spec")
    {
        CAudioFileIf::FileSpec_t stFileSpec;
        const std::string cExt(".wav");
        Error_t err = Error_t::kUnknownError;

        // write wave file
        int iNumRemainingFrames = m_iBuffLength;

        m_stFileSpec.eFormat = CAudioFileIf::kFileFormatWav;

        err = m_pCAudioFile->openFile(cTestDataDir + "/test" + cExt, CAudioFileIf::kFileWrite, &m_stFileSpec);
        CHECK(err == Error_t::kNoError);

        // put data
        while (iNumRemainingFrames > 0)
        {
            for (int i = 0; i < m_iNumChannels; i++)
                m_ppfTmp[i] = &m_ppfAudioData[i][m_iBuffLength - iNumRemainingFrames];
            int iPutFrames = std::min(m_iBlockLength, iNumRemainingFrames);

            m_pCAudioFile->writeData(m_ppfTmp, iPutFrames);

            iNumRemainingFrames -= iPutFrames;
        }
        m_pCAudioFile->closeFile();
        m_pCAudioFile->reset();

        err = m_pCAudioFile->openFile(cTestDataDir + "/test" + cExt, CAudioFileIf::kFileRead);
        CHECK(err == Error_t::kNoError);

        m_pCAudioFile->getFileSpec(stFileSpec);

        CHECK(44100 == stFileSpec.fSampleRateInHz);
        CHECK(CAudioFileIf::kFileFormatWav == stFileSpec.eFormat);

        m_pCAudioFile->closeFile();

        // cleanup
        AudioIoTest::deleteFile(cExt);
    }


    assert(m_ppfAudioData != 0);
    for (int i = 0; i < m_iNumChannels; i++)
        delete[] m_ppfAudioData[i];
    delete[] m_ppfAudioData;
    delete[] m_ppfTmp;

    CAudioFileIf::destroy(m_pCAudioFile);

}

#endif //WITH_TESTS
