![GitHub top language](https://img.shields.io/github/languages/top/alexanderlerch/libACA)
![release](https://img.shields.io/github/v/release/alexanderlerch/libACA)
![GitHub issues](https://img.shields.io/github/issues-raw/alexanderlerch/libACA)
[![Tests](https://github.com/alexanderlerch/libACA/actions/workflows/cmake.yml/badge.svg)](https://github.com/alexanderlerch/libACA/actions/workflows/cmake.yml)
![GitHub last commit](https://img.shields.io/github/last-commit/alexanderlerch/libACA)
![GitHub](https://img.shields.io/github/license/alexanderlerch/libACA)

# libACA
C++ code accompanying the book ["An Introduction to Audio Content Analysis"](https://www.AudioContentAnalysis.org). The source code shows example implementations of basic approaches, features, and algorithms for music audio content analysis.

All implementations are also available in:
* [Matlab: ACA-Code](https://github.com/alexanderlerch/ACA-Code)
* [Python: pyACA](https://github.com/alexanderlerch/pyACA)

# overview

## directory structure
```console

|_ 3rdparty: 3rd party dependencies
  |_ Fft: simple Fft source code (Toth Laszlo)
  |_ sndlib: sndfile library (c library for file IO: https://ccrma.stanford.edu/software/snd/sndlib/)
  |_ chat2: catch2 testing header (https://github.com/catchorg/Catch2)
|_ cmake.modules: cmake scripts
|_ inc: headers
  |_ helpers: more headers for internal use
|_ src: source code
  |_ ACA: core library 
  |_ AudioFileIO: library wrapping sndfile (3rdparty)
  |_ ComputeBeatHisto: demo executable for beat histogram extraction 
  |_ ComputeChords: demo executable for chord extraction 
  |_ ComputeFeature: demo executable for feature extraction 
  |_ ComputeFingerprint: demo executable for audio fingerprint extraction 
  |_ ComputeKey: demo executable for key detection 
  |_ ComputeMelSpectrogram: demo executable for mel spectrogram extraction 
  |_ ComputeNoveltyFunction: demo executable for novelty function and onset extraction 
  |_ ComputePitch: demo executable for monophonic f0 extraction 
  |_ ComputeSpectrogram: demo executable for spectrogram extraction 
  |_ Tests: all code related to tests
	  |_ TestData: data for specific tests possibly requiring data
	  |_ TestExec: test executable
	  |_ Tests: individual test implementations 
	  
```
## functionality
The library libACA offers simple implementations of basic audio content analysis algorithms, including low level audio features, different f0 extractors, as well as simple approaches to chord, musical key detection, and onset detection. It also offers implementations of multiple generic algorithms useful in audio content analysis such as , Dynamic Time Warping (DTW), K Nearest Neighbor classification, Gaussian Mixture Model, Sequential Forward Feature Selection, Viterbi algorithm, Principal Component Analysis, Non negative Matrix Factorization, various conversion functions.

As the implementation aims at providing an accessible code base to foster understanding of algorithms presented in the text book, there are necessarily compromises to be made between readability, performance, memory efficiency, and structure.

# getting started
If this is your first glance at this project, it might be helpful to visit the [Matlab: ACA-Code](https://github.com/alexanderlerch/ACA-Code) or [Python: pyACA](https://github.com/alexanderlerch/pyACA) repositories of the same algorithms first. Projects and source code in these languages are often more compact and thus more easily accessible. 

Each folder in the src directory is a build target.
All algorithmic implementations which compile into the actualy libACA library can be found in the source directory [./src/ACA](https://github.com/alexanderlerch/libACA/tree/main/src/ACA) with the corresponding header files in [./inc](https://github.com/alexanderlerch/libACA/tree/main/inc). 
All folders starting with 'Compute' are executable targets that can be run from the command line. 
The 'Test' folder contains the test executable.

## creating the project files with CMake
The project files are generated through [CMake](https://www.cmake.org). Using the latest CMake GUI, 
* point the source code directory to the top-level project directly, then 
* set the build directory to some directory you like (suggestion: sourcedir/bld), 
* hit 'Configure' button until nothing is red, then
* 'Generate' the project and open it with your IDE.
In case there are any problems, try clearing the cache first.

On the command line, try from the sourcedir

```console

cmake -B ./bld/ 
cmake --build ./bld/ 

```
Enable ```WITH_TESTS``` to build with Catch2 support and ```WITH_DOXYGENTARGET``` to add a target for creating a doxygen documentation for your project.

If new files are added, clear the cache and rerun configuration and generation.

## running the tests with CTest
Enable ```WITH_TESTS``` (enabled by default) to build with Catch2 support.

On the command line, try from the sourcedir
```console

cd bld/ 
ctest

```
In the IDE, you can either build the ```RUN_TESTS``` or execute ```TestExec```.

## documentation
The latest automatically generated [Doxygen](https://www.doxygen.nl) documentation of this package can be found at [https://alexanderlerch.github.io/libACA](https://alexanderlerch.github.io/libACA).

## code examples
While the code is intended for reading or as easy-to-understand reference implementation, the executables showcase some simple entry points for the core functionality offered.

### feature extraction: spectral centroid
```cpp
#include "Feature.h"

// declarations
CFeatureIf* pCInstance = 0; //!< new instance of the Feature Interface class
CFeatureIf::Feature_t eFeatureIdx = CFeatureIf::kFeatureSpectralCentroid; //!< specifies feature to extract
std::string sInFilePath = "testname.wav"; //!< input file path
int aiFeatureDims[2] = { 0,0 }; //!< resulting feature matrix dimension
float* pfFeature = 0; //!< feature result

// create instance
CFeatureIf::create(pCInstance, eFeatureIdx, sInFilePath);
pCInstance->getFeatureDimensions(aiFeatureDims[0], aiFeatureDims[1]);

// allocate memory
ppfFeature = new float [aiFeatureDims[1]];
	
// extract feature
pCInstance->compFeature1Dim(ppfFeature[0]);

// clean-up 
CFeatureIf::destroy(pCInstance);
delete[] pfFeature;

```
### feature extraction: pitch chroma
```cpp
#include "Feature.h"

// declarations
CFeatureIf* pCInstance = 0; //!< new instance of the Feature Interface class
CFeatureIf::Feature_t eFeatureIdx = CFeatureIf::kFeatureSpectralPitchChroma; //!< specifies feature to extract
std::string sInFilePath = "testname.wav"; //!< input file path
int aiFeatureDims[2] = { 0,0 }; //!< resulting feature matrix dimension
float** ppfFeature = 0; //!< feature result

// create instance
CFeatureIf::create(pCInstance, eFeatureIdx, sInFilePath);
pCInstance->getFeatureDimensions(aiFeatureDims[0], aiFeatureDims[1]);

// allocate memory
ppfFeature = new float* [aiFeatureDims[0]];
for (auto k = 0; k < aiFeatureDims[0]; k++)
	ppfFeature[k] = new float[aiFeatureDims[1]];
	
// extract feature
pCInstance->compFeatureNDim(ppfFeature);

// clean-up 
CFeatureIf::destroy(pCInstance);
for (int k = 0; k < aiFeatureDims[0]; k++)
	delete[] ppfFeature[k];
delete[] ppfFeature;

```
### key detection
```cpp
#include "Key.h"

// declarations
CKey* pCInstance = new CKey(); //!< new instance of the Feature Interface class
std::string sInFilePath = "testname.wav"; //!< input file path
int iKeyRes = -1; //!< index of the resulting key

// init instance
pCInstance->init(sInFilePath);

// compute key
iKeyRes = pCInstance->compKey();

// print result
cout << "detected key: " << pCInstance->getKeyString(static_cast<CKey::Keys_t>(iKeyRes))<< endl;
	
// clean-up 
CKey::destroy(pCInstance);

```
# design principles
C++ is not the most convenient language for explaining algorithmic concepts. Other languages such as Python or Matlab are better suited for this taks. Audio applications and especially applications capable of real-time processing, however, often require implementation in a language such as C++, as do many embedded applications.
The source code of a C++ implementation can give the reader a better estimate of algorithmic complexity and required memory than code written in a high-level language, as long as too many 3rd Party dependencies are avoided.

## accessibility

readability, flat class hierarchies, descriptive variable names

## C++11 compatibility

## optimization vs. memory allocation vs. readability

## api memory ownership

# other information

## text book
The textbook "An Introduction to Audio Content Analysis" might be available free of charge to you if your library has subscribed to the [IEEE](https://ieeexplore.ieee.org/book/6266785) library. It introduces the field and applications of audio content analysis, explains basic principles, describes a superset of the algorithms in this repository, and provides numerous references that allow the reader to quickly dig deeper into each topic if required.

## slides
Lecture slides linked to the text book content, accompanied by the latex source files can be found in (this repository}[https:://www.github.com/alexanderlerch/ACA-Slides].

## video lectures
Video lectures, albeit a somewhat outdated and accompanying a previous edition, can be found on [audiocontentanalysis.org](https://www.audiocontentanalysis.org/class).

## datasets
A list of useful datasets for audio content analysis focused on music and music information retrieval in general can be found on [audiocontentanalysis.org](https://www.audiocontentanalysis.org/data-sets).

## license
libACA is licensed under an [MIT license](https://github.com/alexanderlerch/libACA/blob/main/LICENSE).