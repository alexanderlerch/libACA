![GitHub top language](https://img.shields.io/github/languages/top/alexanderlerch/libACA)
![GitHub issues](https://img.shields.io/github/issues-raw/alexanderlerch/libACA)
![GitHub last commit](https://img.shields.io/github/last-commit/alexanderlerch/libACA)
![GitHub](https://img.shields.io/github/license/alexanderlerch/libACA)

# libACA
work in progress - ACA scripts in C++

compare:
* [Matlab: ACA-Code](https://github.com/alexanderlerch/ACA-Code)
* [Python: pyACA](https://github.com/alexanderlerch/pyACA)

## Project Structure
```console
|_ 3rdparty: (3rd party dependencies)
  |_ Fft: simple Fft library
  |_ sndlib: sndfile library (3rdparty with ugly code and lots of warnings)
  |_ chat2: catch2 testing header
|_ cmake.modules: (cmake scripts)
|_ inc: headers
  |_ helpers: more headers for internal use
|_ src: source code
  |_ ACA: core library 
  |_ AudioFileIO: library wrapping sndfile (3rdparty)
  |_ ComputeBeatHisto: demo executable for beat histogram extraction 
  |_ ComputeFeature: demo executable for feature extraction 
  |_ ComputeMelSpectrogram: demo executable for mel spectrogram extraction 
  |_ ComputeNoveltyFunction: demo executable for novelty function and onset extraction 
  |_ ComputeSpectrogram: demo executable for spectrogram extraction 
  |_ Tests: all code related to tests
	  |_ TestData: data for specific tests possibly requiring data
	  |_ TestExec: test executable
	  |_ Tests: individual test implementations 
```

## Creating the Project Files with CMake
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

## Running the Tests with CTest
Enable ```WITH_TESTS``` to build with Catch2 support.

On the command line, try from the sourcedir
```console
cd bld/ 
ctest
```
In the IDE, you can either build the ```RUN_TESTS``` or execute ```TestExec```.
