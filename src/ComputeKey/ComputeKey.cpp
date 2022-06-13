#include "AcaAll.h"

#include <iostream>
#include <fstream>
#include <ctime>

#include "Key.h"

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

    int iBlockLength = 0, //!< block length in samples 
        iHopLength = 0; //!< hop length in samples

    int iKeyRes = -1;
    clock_t time = 0;

    CKey* pCInstance = 0;

    std::fstream hOutputFile;

    showClInfo();

    //////////////////////////////////////////////////////////////////////////////
    // parse command line arguments
    if (argc < 2)
    {
        cout << "Missing audio input path!" << endl;
        cout << "Expected Synopsis: inputfile Keyname [outputtxtfile] [blocksize] [hopsize]" << endl;
        return -1;
    }
    else
    {
        sInputFilePath = argv[1];
        sOutputFilePath = (argc < 3) ? "" : argv[2];
        iBlockLength = (argc < 4) ? 4096 : std::stoi(argv[3]);
        iHopLength = (argc < 5) ? 2048 : std::stoi(argv[4]);
    }

    //////////////////////////////////////////////////////////////////////////////
    // initialize Key instance
    pCInstance = new CKey();
    if (!pCInstance)
        return -1;
    pCInstance->init(sInputFilePath, iBlockLength, iHopLength);

    //////////////////////////////////////////////////////////////////////////////
    // open the output text file
    if (!sOutputFilePath.empty())
    {
        hOutputFile.open(sOutputFilePath.c_str(), std::ios::out);
        if (!hOutputFile.is_open())
        {
            cout << "Text file open error!";
            delete pCInstance;
            return -1;
        }
    }

    time = clock();

    //////////////////////////////////////////////////////////////////////////////
    // compute key
    cout << "\n1. computing key..." << endl;
    iKeyRes = pCInstance->compKey();

    cout << "\n Key computation done in: \t" << (clock() - time) * 1.F / CLOCKS_PER_SEC << " seconds." << endl;
    cout << "Result: " << pCInstance->getKeyString(static_cast<CKey::Keys_t>(iKeyRes)) << endl;


    //////////////////////////////////////////////////////////////////////////////
    // file writing
    if (!sOutputFilePath.empty())
    {
        time = clock();
        cout << "\n2. writing output file..." << endl;

        hOutputFile << pCInstance->getKeyString(static_cast<CKey::Keys_t>(iKeyRes)) << endl;

        cout << "\n writing done in: \t" << (clock() - time) * 1.F / CLOCKS_PER_SEC << " seconds." << endl;
    }

    //////////////////////////////////////////////////////////////////////////////
    // clean-up (close files, delete instances, and free memory)
    hOutputFile.close();

    delete pCInstance;

    // all done
    return 0;
}


void     showClInfo()
{
    cout << "ACA v" << getAcaVersion() << ": Demo Executable for Key Extraction" << endl;
    cout << "Build date: " << getAcaBuildDate() << endl;
    cout << "(c) 2022 by Alexander Lerch" << endl;
    cout << "Synopsis: ComputeKey inputwavfile [outputtxtfile] [blocksize] [hopsize]" << endl;
    cout << endl;

    return;
}
