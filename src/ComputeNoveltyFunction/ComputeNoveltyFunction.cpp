#include "ACAConfig.h"

#include <iostream>
#include <fstream>
#include <ctime>

#include "ACAConfig.h"

#include "Novelty.h"

using std::cout;
using std::endl;

// local function declarations
void    showClInfo();

/////////////////////////////////////////////////////////////////////////////////
// main function
int main(int argc, char* argv[])
{

    std::string             sInputFilePath,                 //!< file paths
        sOutputFilePath;

    std::string sNoveltyString; //!< string of the Novelty to be extracted

    int iBlockLength = 0, //!< block length in samples 
        iHopLength = 0; //!< hop length in samples
    int iNumBlocks = 0; //!< number of blocks

    clock_t time = 0;

    CNoveltyIf* pCInstance = 0;
    CNoveltyIf::Novelty_t eNoveltyIdx = CNoveltyIf::kNumNoveltyFunctions;

    float* pfNovelty = 0; //!< Novelty result

    std::fstream hOutputFile;

    showClInfo();

    //////////////////////////////////////////////////////////////////////////////
    // parse command line arguments
    if (argc < 2)
    {
        cout << "Missing audio input path!" << endl;
        cout << "Expected Synopsis: inputfile Noveltyname [outputtxtfile] [blocksize] [hopsize]" << endl;
        return -1;
    }
    else
    {
        sInputFilePath = argv[1];
        sNoveltyString = (argc < 3) ? "SpectralCentroid" : argv[2];
        sOutputFilePath = (argc < 4) ? sInputFilePath + ".txt" : argv[3];
        iBlockLength = (argc < 5) ? 4096 : std::stoi(argv[4]);
        iHopLength = (argc < 6) ? 512 : std::stoi(argv[5]);
    }

    //////////////////////////////////////////////////////////////////////////////
    // initialize novelty instance
    eNoveltyIdx = CNoveltyIf::getNoveltyIdxFromString(sNoveltyString);
    CNoveltyIf::create(pCInstance, eNoveltyIdx, sInputFilePath, iBlockLength, iHopLength);
    pCInstance->getNumBlocks(iNumBlocks);

    //////////////////////////////////////////////////////////////////////////////
    // open the output text file
    hOutputFile.open(sOutputFilePath.c_str(), std::ios::out);
    if (!hOutputFile.is_open())
    {
        cout << "Text file open error!";
        CNoveltyIf::destroy(pCInstance);
        return -1;
    }

    //////////////////////////////////////////////////////////////////////////////
    // allocate memory
    pfNovelty = new float [iNumBlocks];


    if (!pfNovelty)
    {
        CNoveltyIf::destroy(pCInstance);
        hOutputFile.close();
        return -1;
    }

    time = clock();

    //////////////////////////////////////////////////////////////////////////////
    // compute spectrogram
    cout << "\n1. computing Novelty..." << endl;
    pCInstance->compNovelty(pfNovelty);

    cout << "\n Novelty computation done in: \t" << (clock() - time) * 1.F / CLOCKS_PER_SEC << " seconds." << endl;

    time = clock();

    //////////////////////////////////////////////////////////////////////////////
    // file writing

    cout << "\n2. writing output file..." << endl;

    for (auto n = 0; n < iNumBlocks; n++)
    {
        hOutputFile << pfNovelty[n] << endl;
    }

    cout << "\n writing done in: \t" << (clock() - time) * 1.F / CLOCKS_PER_SEC << " seconds." << endl;

    //////////////////////////////////////////////////////////////////////////////
    // clean-up (close files, delete instances, and free memory)
    CNoveltyIf::destroy(pCInstance);
    hOutputFile.close();

    delete[] pfNovelty;
    pfNovelty = 0;

    // all done
    return 0;

}


void     showClInfo()
{
    cout << "ACA v" << ACA_VERSION_MAJOR << "." << ACA_VERSION_MINOR << "." << ACA_VERSION_PATCH << ": Demo Executable for Novelty Extraction" << endl;
    cout << "(c) 2022 by Alexander Lerch" << endl;
    cout << "Synopsis: ComputeNoveltyFunction inputwavfile Noveltyname [outputtxtfile] [blocksize] [hopsize]" << endl;
    cout << endl;

    return;
}

