
#include <iostream>
#include <fstream>
#include <ctime>

#include "Spectrogram.h"

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

    int iBlockLength = 0,
        iHopLength = 0;
    int aiSpecGramDimensions[2] = { 0,0 };

    clock_t time = 0;

    CSpectrogramIf* pCSpectrogram = 0;

    float** ppfSpectrogram = 0;

    std::fstream hOutputFile;

    showClInfo();

    //////////////////////////////////////////////////////////////////////////////
    // parse command line arguments
    if (argc < 2)
    {
        cout << "Missing audio input path!" << endl;
        cout << "Expected Synopsis: inputfile [outputfile] [blocksize] [hopsize]" << endl;
        return -1;
    }
    else
    {
        sInputFilePath = argv[1];
        sOutputFilePath = (argc < 3) ? sInputFilePath + ".txt" : argv[2];
        iBlockLength = (argc < 4) ? 4096 : std::stoi(argv[3]);
        iHopLength = (argc < 5) ? 2048 : std::stoi(argv[4]);
    }

    //////////////////////////////////////////////////////////////////////////////
    // initialize spectrogram instance
    CSpectrogramIf::create(pCSpectrogram, sInputFilePath, iBlockLength, iHopLength);
    pCSpectrogram->getSpectrogramDimensions(aiSpecGramDimensions[0], aiSpecGramDimensions[1]);

    //////////////////////////////////////////////////////////////////////////////
    // open the output text file
    hOutputFile.open(sOutputFilePath.c_str(), std::ios::out);
    if (!hOutputFile.is_open())
    {
        cout << "Text file open error!";
        CSpectrogramIf::destroy(pCSpectrogram);
        return -1;
    }

    //////////////////////////////////////////////////////////////////////////////
    // allocate memory
    ppfSpectrogram = new float* [aiSpecGramDimensions[0]];
    for (auto k = 0; k < aiSpecGramDimensions[0]; k++)
        ppfSpectrogram[k] = new float[aiSpecGramDimensions[1]];


    if (!ppfSpectrogram )
    {
        CSpectrogramIf::destroy(pCSpectrogram);
        hOutputFile.close();
        return -1;
    }

    time = clock();

    //////////////////////////////////////////////////////////////////////////////
    // compute spectrogram
    cout << "\n1. computing spectrogram..." << endl;
    pCSpectrogram->getSpectrogram(ppfSpectrogram);

    cout << "\n spectrogram computation done in: \t" << (clock() - time) * 1.F / CLOCKS_PER_SEC << " seconds." << endl;

    time = clock();

    //////////////////////////////////////////////////////////////////////////////
    // file writing

    cout << "\n2. writing output file..." << endl;

    for (auto k = 0; k < aiSpecGramDimensions[0]; k++)
    {
        // write
        for (int n = 0; n < aiSpecGramDimensions[1]; n++)
        {
            hOutputFile << ppfSpectrogram[k][n] << "\t";
        }
        hOutputFile << endl;
    }

    cout << "\n writing done in: \t" << (clock() - time) * 1.F / CLOCKS_PER_SEC << " seconds." << endl;

    //////////////////////////////////////////////////////////////////////////////
    // clean-up (close files, delete instances, and free memory)
    CSpectrogramIf::destroy(pCSpectrogram);
    hOutputFile.close();

    for (int k = 0; k < aiSpecGramDimensions[0]; k++)
        delete[] ppfSpectrogram[k];
    delete[] ppfSpectrogram;
    ppfSpectrogram = 0;

    // all done
    return 0;

}


void     showClInfo()
{
    cout << "Demo Executable for Spectrogram Extraction" << endl;
    cout << "(c) 2022 by Alexander Lerch" << endl;
    cout << "Synopsis: ComputeSpectrogram inputfile [outputfile] [blocksize] [hopsize]" << endl;
    cout << endl;

    return;
}

