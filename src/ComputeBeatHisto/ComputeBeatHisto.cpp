#include "AcaAll.h"

#include <iostream>
#include <fstream>
#include <ctime>

#include "BeatHisto.h"

using std::cout;
using std::endl;

// local function declarations
void    showClInfo();

/////////////////////////////////////////////////////////////////////////////////
// main function
int main(int argc, char* argv[])
{

    std::string             sInFilePath,                 //!< file paths
        sOutFilePath;

    int iBlockLength = 0, //!< block length in samples 
        iHopLength = 0; //!< hop length in samples
    int iBeatHistoDimension = 0;

    clock_t time = 0;

    CBeatHistoIf* pCInstance = 0;
    CBeatHistoIf::BeatHisto_t eBeatHistoIdx = CBeatHistoIf::kBeatHistoFft;

    float* pfBeatHisto = 0; //!< BeatHisto result

    std::fstream hOutputFile;

    showClInfo();

    //////////////////////////////////////////////////////////////////////////////
    // parse command line arguments
    if (argc < 2)
    {
        cout << "Missing audio input path!" << endl;
        cout << "Expected Synopsis: inputfile [outputtxtfile] [blocksize] [hopsize]" << endl;
        return -1;
    }
    else
    {
        sInFilePath = argv[1];
        sOutFilePath = (argc < 3) ? sInFilePath + ".txt" : argv[2];
        iBlockLength = (argc < 4) ? 1024 : std::stoi(argv[3]);
        iHopLength = (argc < 5) ? 8 : std::stoi(argv[4]);
    }

    //////////////////////////////////////////////////////////////////////////////
    // initialize BeatHisto instance
    CBeatHistoIf::create(pCInstance, sInFilePath, iBlockLength, iHopLength);
    iBeatHistoDimension = pCInstance->getNumBins(eBeatHistoIdx);

    //////////////////////////////////////////////////////////////////////////////
    // open the output text file
    hOutputFile.open(sOutFilePath.c_str(), std::ios::out);
    if (!hOutputFile.is_open())
    {
        cout << "Text file open error!";
        CBeatHistoIf::destroy(pCInstance);
        return -1;
    }

    //////////////////////////////////////////////////////////////////////////////
    // allocate memory
    pfBeatHisto = new float[iBeatHistoDimension];


    if (!pfBeatHisto)
    {
        CBeatHistoIf::destroy(pCInstance);
        hOutputFile.close();
        return -1;
    }

    time = clock();

    //////////////////////////////////////////////////////////////////////////////
    // compute spectrogram
    cout << "\n1. computing BeatHisto..." << endl;
    pCInstance->compBeatHisto(pfBeatHisto);

    cout << "\n BeatHisto computation done in: \t" << (clock() - time) * 1.F / CLOCKS_PER_SEC << " seconds." << endl;

    time = clock();

    //////////////////////////////////////////////////////////////////////////////
    // file writing

    cout << "\n2. writing output file..." << endl;

    for (auto k = 0; k < iBeatHistoDimension; k++)
    {
        hOutputFile << pfBeatHisto[k] << endl;
    }

    cout << "\n writing done in: \t" << (clock() - time) * 1.F / CLOCKS_PER_SEC << " seconds." << endl;

    //////////////////////////////////////////////////////////////////////////////
    // clean-up (close files, delete instances, and free memory)
    CBeatHistoIf::destroy(pCInstance);
    hOutputFile.close();

    delete[] pfBeatHisto;

    // all done
    return 0;

}

void     showClInfo()
{
    cout << "ACA v" << getAcaVersion() << ": Demo Executable for Beat Histogram Extraction" << endl;
    cout << "Build date: " << getAcaBuildDate() << endl;
    cout << "(c) 2022 by Alexander Lerch" << endl;
    cout << "Synopsis: ComputeBeatHistoFunction inputwavfile [outputtxtfile] [blocksize] [hopsize]" << endl;
    cout << endl;

    return;
}
