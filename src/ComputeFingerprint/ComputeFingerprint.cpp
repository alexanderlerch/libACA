#include "AcaAll.h"

#include <iostream>
#include <fstream>
#include <ctime>

#include "ToolFingerprint.h"

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

    long long iNumBlocks = 0; //!< number of blocks

    uint32_t *piFingerprintRes = 0;
    clock_t time = 0;

    CFingerprint* pCInstance = 0;

    std::fstream hOutputFile;

    showClInfo();

    //////////////////////////////////////////////////////////////////////////////
    // parse command line arguments
    if (argc < 2)
    {
        cout << "Missing audio input path!" << endl;
        cout << "Expected Synopsis: inputwavfile [outputtxtfile]" << endl;
        return -1;
    }
    else
    {
        sInputFilePath = argv[1];
        sOutputFilePath = (argc < 3) ? "" : argv[2];
    }

    //////////////////////////////////////////////////////////////////////////////
    // initialize Fingerprint instance
    pCInstance = new CFingerprint();
    if (!pCInstance)
        return -1;
    pCInstance->init(sInputFilePath);
    iNumBlocks = pCInstance->getFingerprintLength();

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

    piFingerprintRes = new uint32_t[iNumBlocks];

    time = clock();

    //////////////////////////////////////////////////////////////////////////////
    // compute Fingerprint
    cout << "\n1. computing Fingerprint..." << endl;
    pCInstance->compFingerprint(piFingerprintRes);

    cout << "\n Fingerprint computation done in: \t" << (clock() - time) * 1.F / CLOCKS_PER_SEC << " seconds." << endl;


    //////////////////////////////////////////////////////////////////////////////
    // file writing
    if (!sOutputFilePath.empty())
    {
        time = clock(); 
        cout << "\n2. writing output file..." << endl;

        for (auto n = 0; n < iNumBlocks; n++)
            hOutputFile << std::hex << piFingerprintRes[n] << endl;

        cout << "\n writing done in: \t" << (clock() - time) * 1.F / CLOCKS_PER_SEC << " seconds." << endl;
    }

    //////////////////////////////////////////////////////////////////////////////
    // clean-up (close files, delete instances, and free memory)
    hOutputFile.close();

    delete pCInstance;

    delete[] piFingerprintRes;

    // all done
    return 0;
}


void     showClInfo()
{
    cout << "ACA v" << getAcaVersion() << ": Demo Executable for Fingerprint Extraction" << endl;
    cout << "Build date: " << getAcaBuildDate() << endl;
    cout << "(c) 2022 by Alexander Lerch" << endl;
    cout << "Synopsis: ComputeFingerprint inputwavfile [outputtxtfile] [blocksize] [hopsize]" << endl;
    cout << endl;

    return;
}
