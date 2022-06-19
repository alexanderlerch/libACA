![GitHub top language](https://img.shields.io/github/languages/top/alexanderlerch/libACA)
![GitHub issues](https://img.shields.io/github/issues-raw/alexanderlerch/libACA)
[![DOI](https://zenodo.org/badge/485183471.svg)](https://zenodo.org/badge/latestdoi/485183471)
[![Tests](https://github.com/alexanderlerch/libACA/actions/workflows/cmake.yml/badge.svg)](https://github.com/alexanderlerch/libACA/actions/workflows/cmake.yml)
![GitHub last commit](https://img.shields.io/github/last-commit/alexanderlerch/libACA)
![GitHub](https://img.shields.io/github/license/alexanderlerch/libACA)
![release](https://img.shields.io/github/v/release/alexanderlerch/libACA)

# libACA
C++ code accompanying the book ["An Introduction to Audio Content Analysis"](https://www.AudioContentAnalysis.org). The source code shows example implementations of basic approaches, features, and algorithms for music audio content analysis.

All implementations are also available in:
* [Matlab: ACA-Code](https://github.com/alexanderlerch/ACA-Code)
* [Python: pyACA](https://github.com/alexanderlerch/pyACA)

# overview
The library libACA offers simple implementations of basic audio content analysis algorithms, including low level audio features, different f0 extractors, as well as simple approaches to chord, musical key detection, and onset detection. It also offers implementations of multiple generic algorithms useful in audio content analysis. 

As the implementation aims at providing an accessible code base to foster understanding of algorithms presented in the text book, there are necessarily compromises to be made between readability, performance, memory efficiency, and structure.

# getting started
If this is your first glance at this project, it might be helpful to visit the [Matlab: ACA-Code](https://github.com/alexanderlerch/ACA-Code) or [Python: pyACA](https://github.com/alexanderlerch/pyACA) repositories of the same algorithms first. Projects and source code in these languages are often more compact and thus more easily accessible. 

Each folder in the src directory is a build target.
All algorithmic implementations which compile into the actual libACA library can be found in the source directory [./src/ACA](https://github.com/alexanderlerch/libACA/tree/main/src/ACA) with the corresponding header files in [./inc](https://github.com/alexanderlerch/libACA/tree/main/inc). 
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
In addition to the executables easily identifyable in the directory structure above, different functionality is available through different header files
>- [BeatHisto.h](https://github.com/alexanderlerch/libACA/blob/main/inc/BeatHisto.h): beat histogram computation
>- [Chord.h](https://github.com/alexanderlerch/libACA/blob/main/inc/Chord.h): chord detection
>- [Feature.h](https://github.com/alexanderlerch/libACA/blob/main/inc/Feature.h): extraction of instantaneous features
>- [Key.h](https://github.com/alexanderlerch/libACA/blob/main/inc/Key.h): key detection
>- [Novelty.h](https://github.com/alexanderlerch/libACA/blob/main/inc/Novelty.h): extraction of the novelty function and onsets
>- [Pitch.h](https://github.com/alexanderlerch/libACA/blob/main/inc/Pitch.h): f0 extraction from monophonic audio
>- [Spectrogram.h](https://github.com/alexanderlerch/libACA/blob/main/inc/Spectrogram.h): computation of (Mel) spectrogram
>- [ToolBlockAudio.h](https://github.com/alexanderlerch/libACA/blob/main/inc/ToolBlockAudio.h): blocking the audio
>- [ToolCcf.h](https://github.com/alexanderlerch/libACA/blob/main/inc/ToolCcf.h): auto- and cross-correlation
>- [ToolConversion.h](https://github.com/alexanderlerch/libACA/blob/main/inc/ToolConversion.h): various conversion functions (freq/mel/midi/bin, etc.)
>- [ToolFingerprint.h](https://github.com/alexanderlerch/libACA/blob/main/inc/ToolFingerprint.h): audio fingerprint extraction
>- [ToolGammatone.h](https://github.com/alexanderlerch/libACA/blob/main/inc/ToolGammatone.h): gammatone filterbank
>- [ToolGmm.h](https://github.com/alexanderlerch/libACA/blob/main/inc/ToolGmm.h): gaussian mixture model
>- [ToolGmmClassifier.h](https://github.com/alexanderlerch/libACA/blob/main/inc/ToolGmmClassifier.h): classifier based on a gaussian mixture model
>- [ToolInstFreq.h](https://github.com/alexanderlerch/libACA/blob/main/inc/ToolInstFreq.h): computation of the instantaneous frequency
>- [ToolLeaveOneOutCrossVal.h](https://github.com/alexanderlerch/libACA/blob/main/inc/ToolLeaveOneOutCrossVal.h): leave one out cross validation
>- [ToolLowPass.h](https://github.com/alexanderlerch/libACA/blob/main/inc/ToolLowPass.h): smoothing filters (moving average and single pole)
>- [ToolNmf.h](https://github.com/alexanderlerch/libACA/blob/main/inc/ToolNmf.h): non-negative matrix factorization
>- [ToolPca.h](https://github.com/alexanderlerch/libACA/blob/main/inc/ToolPca.h): principal component analysis
>- [ToolPreProc.h](https://github.com/alexanderlerch/libACA/blob/main/inc/ToolPreProc.h): pre-processing (downmixing and normalization)
>- [ToolResample.h](https://github.com/alexanderlerch/libACA/blob/main/inc/ToolResample.h): very simple sample rate conversion
>- [ToolSeqFeatureSel.h](https://github.com/alexanderlerch/libACA/blob/main/inc/ToolSeqFeatureSel.h): sequential forward feature selection
>- [ToolSimpleDtw.h](https://github.com/alexanderlerch/libACA/blob/main/inc/ToolSimpleDtw.h): dynamic time warping
>- [ToolSimpleKmean.h](https://github.com/alexanderlerch/libACA/blob/main/inc/ToolSimpleKmean.h): kmeans clustering algorithm
>- [ToolSimpleKnn.h](https://github.com/alexanderlerch/libACA/blob/main/inc/ToolSimpleKnn.h): k-nearest neighbor classification
>- [ToolViterbi.h](https://github.com/alexanderlerch/libACA/blob/main/inc/ToolViterbi.h): Viterbi algorithm

In addition, there are the following generic helper functions available
>- [helper/Fft.h](https://github.com/alexanderlerch/libACA/blob/main/inc/helper/Fft.h): fast fourier transform
>- [helper/Filter.h](https://github.com/alexanderlerch/libACA/blob/main/inc/helper/Filter.h): generic filter implementation
>- [helper/Matrix.h](https://github.com/alexanderlerch/libACA/blob/main/inc/helper/Matrix.h): matrix operations on double pointers
>- [helper/RingBuffer.h](https://github.com/alexanderlerch/libACA/blob/main/inc/helper/RingBuffer.h): circular buffer implementation
>- [helper/Synthesis.h](https://github.com/alexanderlerch/libACA/blob/main/inc/helper/Synthesis.h): simple signal generators
>- [helper/Util.h](https://github.com/alexanderlerch/libACA/blob/main/inc/helper/Util.h): utility functions
>- [helper/Vector.h](https://github.com/alexanderlerch/libACA/blob/main/inc/helper/Vector.h): vector operations

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
The source code of a sample C++ implementation such as libACA provides can give the reader a better estimate of algorithmic complexity and required memory than code written in a high-level language (at least as long as too many 3rd Party dependencies are avoided).


## minimal dependencies
Third party code and even dependency on the STL are reduced to a minimum, more specifically to the simple audio file reader and writer library [sndlib](https://ccrma.stanford.edu/software/snd/sndlib/) and a standard FFT implementation. In addition, [Catch2](https://github.com/catchorg/Catch2) is used as testing framework.

The main reasons for avoiding dependencies are
* accessibility, i.e., algorithmic implementations from scratch without obfuscation by using 3rd party implementations,
* maintainability through independence from 3rd party code

This design choice comes, however, at the cost of both significantly more lines of code and the focus on simple algorithms and models (e.g., no sophisticated machine learning models).

## readability
To increase readability of the sources, the project uses
* consistent and descriptive variable naming that also includes the type (I know, sue me),
* flat class hierarchies if inheritance is used at all.
* C++11 compatible code and some C-Style approaches

At many points a compromise had to be found between readability and functionality. It is, for example, not a big deal to ask a user to read an audio file into a matrix in Matlab or Python, but the same operation might require considerable code in C++. Therefore, some interface classes also offer audio file reading as an option.

## memory allocation and ownership
Any memory allocation is shifted from process functions to constructors and init functions except when needed. This accounts for real-time adaptibility of most of the code base; memory operations in real-time contexts are generally a bad idea.
All allocated memory is always freed by the class which allocated it; for example, it will never happen that libACA frees memory that was allocated by the user or allocates memory for the user. That means that for some results, the user has to allocate buffers to retrieve the processing results.


## optimization 
To increase readability, the code is not optimized except on an algorithmic level (e.g., the correlation function is computed in the frequency domain instead of the time domain). However, it was implemented in a way that allows for optimization. That means that typical number crunching functions (FFT, Vector and Matrix operations, ...) are wrapped so they can be easily replaced without modifying the rest of the library.

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
