#include "AcaAll.h"

#include <iostream>
#include <fstream>
#include <ctime>

#include "Feature.h"

using std::cout;
using std::endl;

// local function declarations
void    showClInfo();

/////////////////////////////////////////////////////////////////////////////////
// main function
int main(int argc, char* argv[])
{

    std::string sInFilePath,//!< file paths
        sOutFilePath;

    std::string sFeatureString; //!< string of the feature to be extracted

    int iBlockLength = 0, //!< block length in samples 
        iHopLength = 0; //!< hop length in samples
    int aiFeatureDims[2] = { 0,0 }; //!< feature (matrix) dimension

    clock_t time = 0;

    CFeatureIf* pCInstance = 0;
    CFeatureIf::Feature_t eFeatureIdx = CFeatureIf::kNumFeatures;

    float** ppfFeature = 0; //!< feature result

    std::fstream hOutFile;

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
        sInFilePath = argv[1];
        sFeatureString = (argc < 3) ? "SpectralCentroid" : argv[2];
        sOutFilePath = (argc < 4) ? sInFilePath + ".txt" : argv[3];
        iBlockLength = (argc < 5) ? 4096 : std::stoi(argv[4]);
        iHopLength = (argc < 6) ? 2048 : std::stoi(argv[5]);
    }

    //////////////////////////////////////////////////////////////////////////////
    // initialize feature instance
    eFeatureIdx = CFeatureIf::getFeatureIdxFromString(sFeatureString);
    CFeatureIf::create(pCInstance, eFeatureIdx, sInFilePath, iBlockLength, iHopLength);
    pCInstance->getFeatureDimensions(aiFeatureDims[0], aiFeatureDims[1]);

    //////////////////////////////////////////////////////////////////////////////
    // open the output text file
    hOutFile.open(sOutFilePath.c_str(), std::ios::out);
    if (!hOutFile.is_open())
    {
        cout << "Text file open error!";
        CFeatureIf::destroy(pCInstance);
        return -1;
    }

    //////////////////////////////////////////////////////////////////////////////
    // allocate memory
    ppfFeature = new float* [aiFeatureDims[0]];
    for (auto k = 0; k < aiFeatureDims[0]; k++)
        ppfFeature[k] = new float[aiFeatureDims[1]];


    if (!ppfFeature)
    {
        CFeatureIf::destroy(pCInstance);
        hOutFile.close();
        return -1;
    }

    time = clock();

    //////////////////////////////////////////////////////////////////////////////
    // compute feature
    cout << "\n1. computing feature..." << endl;
    if (aiFeatureDims[0] == 1)
        pCInstance->compFeature1Dim(ppfFeature[0]);
    else
        pCInstance->compFeatureNDim(ppfFeature);

    cout << "\n feature computation done in: \t" << (clock() - time) * 1.F / CLOCKS_PER_SEC << " seconds." << endl;

    time = clock();

    //////////////////////////////////////////////////////////////////////////////
    // file writing

    cout << "\n2. writing output file..." << endl;

    for (auto k = 0; k < aiFeatureDims[0]; k++)
    {
        // write
        for (int n = 0; n < aiFeatureDims[1]; n++)
        {
            hOutFile << ppfFeature[k][n] << "\t";
        }
        hOutFile << endl;
    }

    cout << "\n writing done in: \t" << (clock() - time) * 1.F / CLOCKS_PER_SEC << " seconds." << endl;

    //////////////////////////////////////////////////////////////////////////////
    // clean-up (close files, delete instances, and free memory)
    CFeatureIf::destroy(pCInstance);
    hOutFile.close();

    for (int k = 0; k < aiFeatureDims[0]; k++)
        delete[] ppfFeature[k];
    delete[] ppfFeature;

    // all done
    return 0;

}


void     showClInfo()
{
    cout << "ACA v" << getAcaVersion() << ": Demo Executable for Feature Extraction" << endl;
    cout << "Build date: " << getAcaBuildDate() << endl;
    cout << "(c) 2022 by Alexander Lerch" << endl;
    cout << "Synopsis: ComputeFeature inputwavfile featurename [outputtxtfile] [blocksize] [hopsize]" << endl;
    cout << endl;

    return;
}

