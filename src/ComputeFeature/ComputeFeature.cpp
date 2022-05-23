#include "ACAConfig.h"

#include <iostream>
#include <fstream>
#include <ctime>

#include "ACAConfig.h"

#include "Feature.h"

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

    std::string sFeatureString; //!< string of the feature to be extracted

    int iBlockLength = 0, //!< block length in samples 
        iHopLength = 0; //!< hop length in samples
    int aiFeatureDimensions[2] = { 0,0 };

    clock_t time = 0;

    CFeatureIf* pCInstance = 0;
    CFeatureIf::Feature_t eFeatureIdx = CFeatureIf::kNumFeatures;

    float** ppfFeature = 0; //!< feature result

    std::fstream hOutputFile;

    showClInfo();

    //////////////////////////////////////////////////////////////////////////////
    // parse command line arguments
    if (argc < 2)
    {
        cout << "Missing audio input path!" << endl;
        cout << "Expected Synopsis: inputfile featurename [outputtxtfile] [blocksize] [hopsize]" << endl;
        return -1;
    }
    else
    {
        sInputFilePath = argv[1];
        sFeatureString = (argc < 3) ? "SpectralCentroid" : argv[2];
        sOutputFilePath = (argc < 4) ? sInputFilePath + ".txt" : argv[3];
        iBlockLength = (argc < 5) ? 4096 : std::stoi(argv[4]);
        iHopLength = (argc < 6) ? 2048 : std::stoi(argv[5]);
    }

    //////////////////////////////////////////////////////////////////////////////
    // initialize spectrogram instance
    eFeatureIdx = CFeatureIf::getFeatureIdxFromString(sFeatureString);
    CFeatureIf::create(pCInstance, eFeatureIdx, sInputFilePath, iBlockLength, iHopLength);
    pCInstance->getFeatureDimensions(aiFeatureDimensions[0], aiFeatureDimensions[1]);

    //////////////////////////////////////////////////////////////////////////////
    // open the output text file
    hOutputFile.open(sOutputFilePath.c_str(), std::ios::out);
    if (!hOutputFile.is_open())
    {
        cout << "Text file open error!";
        CFeatureIf::destroy(pCInstance);
        return -1;
    }

    //////////////////////////////////////////////////////////////////////////////
    // allocate memory
    ppfFeature = new float* [aiFeatureDimensions[0]];
    for (auto k = 0; k < aiFeatureDimensions[0]; k++)
        ppfFeature[k] = new float[aiFeatureDimensions[1]];


    if (!ppfFeature)
    {
        CFeatureIf::destroy(pCInstance);
        hOutputFile.close();
        return -1;
    }

    time = clock();

    //////////////////////////////////////////////////////////////////////////////
    // compute spectrogram
    cout << "\n1. computing feature..." << endl;
    if (aiFeatureDimensions[0] == 1)
        pCInstance->getFeature1Dim(ppfFeature[0]);
    else
        pCInstance->getFeatureNDim(ppfFeature);

    cout << "\n feature computation done in: \t" << (clock() - time) * 1.F / CLOCKS_PER_SEC << " seconds." << endl;

    time = clock();

    //////////////////////////////////////////////////////////////////////////////
    // file writing

    cout << "\n2. writing output file..." << endl;

    for (auto k = 0; k < aiFeatureDimensions[0]; k++)
    {
        // write
        for (int n = 0; n < aiFeatureDimensions[1]; n++)
        {
            hOutputFile << ppfFeature[k][n] << "\t";
        }
        hOutputFile << endl;
    }

    cout << "\n writing done in: \t" << (clock() - time) * 1.F / CLOCKS_PER_SEC << " seconds." << endl;

    //////////////////////////////////////////////////////////////////////////////
    // clean-up (close files, delete instances, and free memory)
    CFeatureIf::destroy(pCInstance);
    hOutputFile.close();

    for (int k = 0; k < aiFeatureDimensions[0]; k++)
        delete[] ppfFeature[k];
    delete[] ppfFeature;
    ppfFeature = 0;

    // all done
    return 0;

}


void     showClInfo()
{
    cout << "ACA v" << ACA_VERSION_MAJOR << "." << ACA_VERSION_MINOR << "." << ACA_VERSION_PATCH << ": Demo Executable for Feature Extraction" << endl;
    cout << "(c) 2022 by Alexander Lerch" << endl;
    cout << "Synopsis: ComputeSpectrogram inputwavfile featurename [outputtxtfile] [blocksize] [hopsize]" << endl;
    cout << endl;

    return;
}

