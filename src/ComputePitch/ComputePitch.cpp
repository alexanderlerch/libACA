#include "AcaAll.h"

#include <iostream>
#include <fstream>
#include <ctime>

#include "Pitch.h"

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

    std::string sPitchString; //!< string of the Pitch to be extracted

    int iBlockLength = 0, //!< block length in samples 
        iHopLength = 0; //!< hop length in samples
    int iNumBlocks = 0; //!< number of blocks

    clock_t time = 0;

    CPitchIf* pCInstance = 0;
    CPitchIf::PitchExtractors_t ePitchIdx = CPitchIf::kNumPitchExtractors;

    float* pfPitch = 0; //!< F0 result

    std::fstream hOutputFile;

    showClInfo();

    //////////////////////////////////////////////////////////////////////////////
    // parse command line arguments
    if (argc < 2)
    {
        cout << "Missing audio input path!" << endl;
        cout << "Expected Synopsis: inputfile Pitchname [outputtxtfile] [blocksize] [hopsize]" << endl;
        return -1;
    }
    else
    {
        sInputFilePath = argv[1];
        sPitchString = (argc < 3) ? "SpectralAcf" : argv[2];
        sOutputFilePath = (argc < 4) ? sInputFilePath + ".txt" : argv[3];
        iBlockLength = (argc < 5) ? 4096 : std::stoi(argv[4]);
        iHopLength = (argc < 6) ? 512 : std::stoi(argv[5]);
    }

    //////////////////////////////////////////////////////////////////////////////
    // initialize Pitch instance
    ePitchIdx = CPitchIf::getPitchIdxFromString(sPitchString);
    CPitchIf::create(pCInstance, ePitchIdx, sInputFilePath, iBlockLength, iHopLength);
    pCInstance->getNumBlocks(iNumBlocks);

    //////////////////////////////////////////////////////////////////////////////
    // open the output text file
    hOutputFile.open(sOutputFilePath.c_str(), std::ios::out);
    if (!hOutputFile.is_open())
    {
        cout << "Text file open error!";
        CPitchIf::destroy(pCInstance);
        return -1;
    }

    //////////////////////////////////////////////////////////////////////////////
    // allocate memory
    pfPitch = new float[iNumBlocks];


    if (!pfPitch)
    {
        CPitchIf::destroy(pCInstance);
        hOutputFile.close();
        return -1;
    }

    time = clock();

    //////////////////////////////////////////////////////////////////////////////
    // compute f0
    cout << "\n1. computing f0..." << endl;
    pCInstance->compF0(pfPitch);

    cout << "\n F0 computation done in: \t" << (clock() - time) * 1.F / CLOCKS_PER_SEC << " seconds." << endl;

    time = clock();

    //////////////////////////////////////////////////////////////////////////////
    // file writing

    cout << "\n2. writing output file..." << endl;

    for (auto n = 0; n < iNumBlocks; n++)
    {
        hOutputFile << pCInstance->getTimeStamp(n) << "\t" << pfPitch[n] << endl;
    }

    cout << "\n writing done in: \t" << (clock() - time) * 1.F / CLOCKS_PER_SEC << " seconds." << endl;

    //////////////////////////////////////////////////////////////////////////////
    // clean-up (close files, delete instances, and free memory)
    CPitchIf::destroy(pCInstance);
    hOutputFile.close();

    delete[] pfPitch;

    // all done
    return 0;

}


void     showClInfo()
{
    cout << "ACA v" << getAcaVersion() << ": Demo Executable for Pitch (F0) Extraction" << endl;
    cout << "Build date: " << getAcaBuildDate() << endl;
    cout << "(c) 2022 by Alexander Lerch" << endl;
    cout << "Synopsis: ComputePitch inputwavfile Pitchname [outputtxtfile] [blocksize] [hopsize]" << endl;
    cout << endl;

    return;
}
