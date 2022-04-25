
#include <iostream>
#include <ctime>

#include "MUSI6106Config.h"

#include "AudioFileIf.h"
#include "RingBuffer.h"
#include "ToolPreProc.h"

#include "Fft.h"
#include "Vector.h"

using std::cout;
using std::endl;

// local function declarations
void    showClInfo();

/////////////////////////////////////////////////////////////////////////////////
// main function
int main(int argc, char* argv[])
{
    std::string sInputFilePath,                 //!< file paths
        sOutputFilePath;

    int iBlockLength = 4096,
        iHopLength = 2048;

    clock_t time = 0;

    float** ppfAudioData = 0;

    float* pfSpectrum = 0;

    CAudioFileIf* phAudioFile = 0;
    std::fstream hOutputFile;
    CAudioFileIf::FileSpec_t stFileSpec;

    CRingBuffer<float>* pCRingBuffer = 0;
    CFft* pCFft = 0;

    showClInfo();

    //////////////////////////////////////////////////////////////////////////////
    // parse command line arguments
    if (argc < 2)
    {
        cout << "Missing audio input path!";
        return -1;
    }
    else
    {
        if (argc > 3)
        {
            iHopLength = std::stoi(argv[3]);
        }
        if (argc > 2)
        {
            iBlockLength = std::stoi(argv[2]);
        }
        if (argc > 1)
        {
            sInputFilePath = argv[1];
            sOutputFilePath = sInputFilePath + ".txt";
        }
    }

    //////////////////////////////////////////////////////////////////////////////
    // initialize read buffer
    pCRingBuffer = new CRingBuffer<float>(iBlockLength);
    pCRingBuffer->setWriteIdx(iBlockLength - iHopLength);

    //////////////////////////////////////////////////////////////////////////////
    // initialize FFT and fft output buffer
    pCFft = new CFft();
    pCFft->init(iBlockLength);
    pfSpectrum = new float[iBlockLength];

    //////////////////////////////////////////////////////////////////////////////
    // open the input wave file
    CAudioFileIf::create(phAudioFile);
    phAudioFile->openFile(sInputFilePath, CAudioFileIf::kFileRead);
    if (!phAudioFile->isOpen())
    {
        cout << "Wave file open error!";
        CAudioFileIf::destroy(phAudioFile);
        return -1;
    }
    phAudioFile->getFileSpec(stFileSpec);

    //////////////////////////////////////////////////////////////////////////////
    // open the output text file
    hOutputFile.open(sOutputFilePath.c_str(), std::ios::out);
    if (!hOutputFile.is_open())
    {
        cout << "Text file open error!";
        CAudioFileIf::destroy(phAudioFile);
        return -1;
    }

    //////////////////////////////////////////////////////////////////////////////
    // allocate memory
    ppfAudioData = new float* [stFileSpec.iNumChannels];
    for (int i = 0; i < stFileSpec.iNumChannels; i++)
        ppfAudioData[i] = new float[iBlockLength];

    if (ppfAudioData == 0)
    {
        CAudioFileIf::destroy(phAudioFile);
        hOutputFile.close();
        return -1;
    }
    if (ppfAudioData[0] == 0)
    {
        CAudioFileIf::destroy(phAudioFile);
        hOutputFile.close();
        return -1;
    }

    time = clock();

    //////////////////////////////////////////////////////////////////////////////
    // get audio data and write it to the output text file (one column per channel)
    while (!phAudioFile->isEof())
    {
        // set block length variable
        long long iNumFrames = iHopLength;

        // read data (iNumOfFrames might be updated!)
        phAudioFile->readData(ppfAudioData, iNumFrames);

        // set buffer to zero if not written (EOF)
        if (iNumFrames < iHopLength)
        {
            for (int c = 0; c < stFileSpec.iNumChannels; c++)
                CVectorFloat::setZero(&ppfAudioData[c][iNumFrames], iHopLength - iNumFrames);
        }

        // downmix in case of multichannel
        CPreProc::downmix(ppfAudioData[0], ppfAudioData, stFileSpec.iNumChannels, iNumFrames);

        // write data into ringbuffer
        pCRingBuffer->putPostInc(ppfAudioData[0], iHopLength);

        cout << "\r" << "computing FFT and writing file";

        // get data from ringbuffer and increment read index
        pCRingBuffer->get(ppfAudioData[0], iBlockLength);
        pCRingBuffer->setReadIdx(pCRingBuffer->getReadIdx() + iHopLength);

        // compute magnitude spectrum (hack
        pCFft->doFft(pfSpectrum, ppfAudioData[0]);
        pCFft->getMagnitude(ppfAudioData[0], pfSpectrum);

        // write
        // note that this solution is, compared to the matlab reference, shifted by one block and scaled by 0.5
        for (int i = 0; i < (iBlockLength >> 1) + 1; i++)
        {
            hOutputFile << ppfAudioData[0][i] << "\t";
        }
        hOutputFile << endl;
    }

    cout << "\nreading/writing done in: \t" << (clock() - time) * 1.F / CLOCKS_PER_SEC << " seconds." << endl;

    //////////////////////////////////////////////////////////////////////////////
    // clean-up (close files, delete instances, and free memory)
    CAudioFileIf::destroy(phAudioFile);
    hOutputFile.close();

    delete pCFft;
    pCFft = 0;
    delete pCRingBuffer;
    pCRingBuffer = 0;

    delete[] pfSpectrum;
    pfSpectrum = 0;
    for (int i = 0; i < stFileSpec.iNumChannels; i++)
        delete[] ppfAudioData[i];
    delete[] ppfAudioData;
    ppfAudioData = 0;

    // all done
    return 0;

}


void     showClInfo()
{
    cout << "MUSI6106 Assignment Executable" << endl;
    cout << "(c) 2014-2022 by Alexander Lerch" << endl;
    cout << endl;

    return;
}

