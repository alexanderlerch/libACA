#include "AcaAll.h"

#include <iostream>
#include <fstream>
#include <ctime>


#include "Chord.h"

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
    int iNumBlocks = 0; //!< number of blocks

    clock_t time = 0;

    CChordIf* pCInstance = 0;

    CChordIf::Chords_t* peChord = 0; //!< chord result

    std::fstream hOutFile;

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
        iBlockLength = (argc < 4) ? 8192 : std::stoi(argv[3]);
        iHopLength = (argc < 5) ? 2048 : std::stoi(argv[4]);
    }

    //////////////////////////////////////////////////////////////////////////////
    // initialize Chord instance
    CChordIf::create(pCInstance, sInFilePath, iBlockLength, iHopLength);
    pCInstance->getNumBlocks(iNumBlocks);

    //////////////////////////////////////////////////////////////////////////////
    // open the output text file
    hOutFile.open(sOutFilePath.c_str(), std::ios::out);
    if (!hOutFile.is_open())
    {
        cout << "Text file open error!";
        CChordIf::destroy(pCInstance);
        return -1;
    }

    //////////////////////////////////////////////////////////////////////////////
    // allocate memory
    peChord = new CChordIf::Chords_t[iNumBlocks];


    if (!peChord)
    {
        CChordIf::destroy(pCInstance);
        hOutFile.close();
        return -1;
    }

    time = clock();

    //////////////////////////////////////////////////////////////////////////////
    // compute spectrogram
    cout << "\n1. computing chord progression..." << endl;
    pCInstance->compChords(peChord);

    cout << "\n F0 computation done in: \t" << (clock() - time) * 1.F / CLOCKS_PER_SEC << " seconds." << endl;

    time = clock();

    //////////////////////////////////////////////////////////////////////////////
    // file writing

    cout << "\n2. writing output file..." << endl;

    for (auto n = 0; n < iNumBlocks; n++)
    {
        hOutFile  << pCInstance->getTimeStamp(n) << "\t" << pCInstance->getChordString(peChord[n]) << endl;
    }

    cout << "\n writing done in: \t" << (clock() - time) * 1.F / CLOCKS_PER_SEC << " seconds." << endl;

    //////////////////////////////////////////////////////////////////////////////
    // clean-up (close files, delete instances, and free memory)
    CChordIf::destroy(pCInstance);
    hOutFile.close();

    delete[] peChord;

    // all done
    return 0;

}


void     showClInfo()
{
    cout << "ACA v" << getAcaVersion() << ": Demo Executable for Chord Extraction" << endl;
    cout << "Build date: " << getAcaBuildDate() << endl;
    cout << "(c) 2022 by Alexander Lerch" << endl;
    cout << "Synopsis: ComputeChord inputwavfile [outputtxtfile] [blocksize] [hopsize]" << endl;
    cout << endl;

    return;
}
