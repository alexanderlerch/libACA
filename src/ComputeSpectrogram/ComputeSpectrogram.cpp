#include "AcaAll.h"

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
    std::string sInFilePath, //!< file paths
        sOutFilePath;

    int iBlockLength = 0, //!< block length in samples 
        iHopLength = 0; //!< hop length in samples
    int aiSpecGramDims[2] = { 0,0 }; //!< spectrogram number of rows and cols

    clock_t time = 0;

    CSpectrogramIf* pCSpectrogram = 0; //!< instance to extract the spectrogram

    float** ppfSpectrogram = 0; //!< resulting spectrogram

    std::fstream hOutFile; //!< output text file

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
        iBlockLength = (argc < 4) ? 4096 : std::stoi(argv[3]);
        iHopLength = (argc < 5) ? 2048 : std::stoi(argv[4]);
    }

    //////////////////////////////////////////////////////////////////////////////
    // initialize spectrogram instance
    CSpectrogramIf::create(pCSpectrogram, sInFilePath, iBlockLength, iHopLength);
    pCSpectrogram->getSpectrogramDimensions(aiSpecGramDims[0], aiSpecGramDims[1]);

    //////////////////////////////////////////////////////////////////////////////
    // open the output text file
    hOutFile.open(sOutFilePath.c_str(), std::ios::out);
    if (!hOutFile.is_open())
    {
        cout << "Text file open error!";
        CSpectrogramIf::destroy(pCSpectrogram);
        return -1;
    }

    //////////////////////////////////////////////////////////////////////////////
    // allocate memory
    ppfSpectrogram = new float* [aiSpecGramDims[0]];
    for (auto k = 0; k < aiSpecGramDims[0]; k++)
        ppfSpectrogram[k] = new float[aiSpecGramDims[1]];


    if (!ppfSpectrogram )
    {
        CSpectrogramIf::destroy(pCSpectrogram);
        hOutFile.close();
        return -1;
    }

    time = clock();

    //////////////////////////////////////////////////////////////////////////////
    // compute spectrogram
    cout << "\n1. computing spectrogram..." << endl;
    pCSpectrogram->compSpectrogram(ppfSpectrogram);

    cout << "\n spectrogram computation done in: \t" << (clock() - time) * 1.F / CLOCKS_PER_SEC << " seconds." << endl;

    time = clock();

    //////////////////////////////////////////////////////////////////////////////
    // file writing

    cout << "\n2. writing output file..." << endl;

    for (auto k = 0; k < aiSpecGramDims[0]; k++)
    {
        // write
        for (int n = 0; n < aiSpecGramDims[1]; n++)
        {
            hOutFile << ppfSpectrogram[k][n] << "\t";
        }
        hOutFile << endl;
    }

    cout << "\n writing done in: \t" << (clock() - time) * 1.F / CLOCKS_PER_SEC << " seconds." << endl;

    //////////////////////////////////////////////////////////////////////////////
    // clean-up (close files, delete instances, and free memory)
    CSpectrogramIf::destroy(pCSpectrogram);
    hOutFile.close();

    for (int k = 0; k < aiSpecGramDims[0]; k++)
        delete[] ppfSpectrogram[k];
    delete[] ppfSpectrogram;

    // all done
    return 0;

}


void     showClInfo()
{
    cout << "ACA v" << getAcaVersion() << ": Demo Executable for Spectrogram Extraction" << endl;
    cout << "Build date: " << getAcaBuildDate() << endl;
    cout << "(c) 2022 by Alexander Lerch" << endl;
    cout << "Synopsis: ComputeSpectrogram inputwavfile [outputtxtfile] [blocksize] [hopsize]" << endl;
    cout << endl;

    return;
}

