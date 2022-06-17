cmake_minimum_required (VERSION 3.6)
project (ACA
        VERSION 0.3.1)

#######################################################################################################
## includes
# where to look for the default stuff...
set (CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake.modules")

# platform detection
include (SetPlatform)

# output directories
include (SetOutputDir)

# oslibs
include (SetOsLibs)

# helper functions
include(AddSrcDirectories)
include(AddIncDirectories)

# with tests or not
option (WITH_TESTS "Use catch2 for testing" ON)

#######################################################################################################
## project specific stuff
set_property(GLOBAL PROPERTY USE_FOLDERS ON)

# set directories
set (PROJECT_DIRECTORIES)
list (APPEND PROJECT_DIRECTORIES ${CMAKE_SOURCE_DIR}/src/ACA) 

list (APPEND PROJECT_DIRECTORIES ${CMAKE_SOURCE_DIR}/src/ComputeBeatHisto)       
list (APPEND PROJECT_DIRECTORIES ${CMAKE_SOURCE_DIR}/src/ComputeChords)      
list (APPEND PROJECT_DIRECTORIES ${CMAKE_SOURCE_DIR}/src/ComputeFeature)       
list (APPEND PROJECT_DIRECTORIES ${CMAKE_SOURCE_DIR}/src/ComputeFingerprint)       
list (APPEND PROJECT_DIRECTORIES ${CMAKE_SOURCE_DIR}/src/ComputeKey)       
list (APPEND PROJECT_DIRECTORIES ${CMAKE_SOURCE_DIR}/src/ComputeMelSpectrogram)   
list (APPEND PROJECT_DIRECTORIES ${CMAKE_SOURCE_DIR}/src/ComputeNoveltyFunction)       
list (APPEND PROJECT_DIRECTORIES ${CMAKE_SOURCE_DIR}/src/ComputePitch)       
list (APPEND PROJECT_DIRECTORIES ${CMAKE_SOURCE_DIR}/src/ComputeSpectrogram)   

list (APPEND PROJECT_DIRECTORIES ${CMAKE_SOURCE_DIR}/src/AudioFileIO) 
    
# set includes
set (PROJECT_INCLUDES)
list (APPEND PROJECT_INCLUDES ${CMAKE_SOURCE_DIR}/inc)       
list (APPEND PROJECT_INCLUDES ${CMAKE_SOURCE_DIR}/inc/helper)       
list (APPEND PROJECT_INCLUDES ${CMAKE_BINARY_DIR}/inc)       

if (WITH_TESTS)
    list (APPEND PROJECT_INCLUDES ${CMAKE_BINARY_DIR}/3rdparty/catch2)       
    list (APPEND PROJECT_DIRECTORIES ${CMAKE_SOURCE_DIR}/src/Tests/TestExec)
endif (WITH_TESTS)

# add directories
add_src_directories ()


# output version number variable
message(STATUS ${PROJECT_NAME} Version: ${PROJECT_VERSION})
message(STATUS "")

# generate platform config file
configure_file(${CMAKE_SOURCE_DIR}/src/${PROJECT_NAME}Config.h.in ${CMAKE_BINARY_DIR}/inc/${PROJECT_NAME}Config.h)

#######################################################################################################
# ## tests
if (WITH_TESTS)
    enable_testing()
    
    # add tests here
    add_test(AudioIo ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/TestExec AudioIo* "${CMAKE_SOURCE_DIR}/src/Tests/TestData/")
    add_test(BeatHisto ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/TestExec BeatHisto*)
    add_test(Chord ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/TestExec Chord*)
    add_test(Dtw ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/TestExec Dtw*)
    add_test(Features ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/TestExec Feature*)
    add_test(Filter ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/TestExec Filter*)
    add_test(Fft ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/TestExec Fft*)
    add_test(Key ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/TestExec Key*)
    add_test(Matrix ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/TestExec Matrix*)
    add_test(Novelty ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/TestExec Novelty*)
    add_test(Pitch ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/TestExec Pitch*)
    add_test(RingBuff ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/TestExec RingBuff*)
    add_test(Spectrogram ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/TestExec Spectrogram*)
    add_test(Tools ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/TestExec Tools*)

    include(AddCppcheckTest)
    add_cppcheck_test ()

endif (WITH_TESTS)

#######################################################################################################
## add documentation
#-- Add an Option to toggle the generation of the API documentation
option(WITH_DOXYGENTARGET "Use Doxygen to create the documentation" OFF)
#-- Configure the Template doxy.config for our specific project
configure_file(${CMAKE_SOURCE_DIR}/src/doxy.config.in ${PROJECT_BINARY_DIR}/docs/doxy.config)
if(WITH_DOXYGENTARGET)
    FIND_PACKAGE(Doxygen)
    if (NOT DOXYGEN_FOUND)
    message(FATAL_ERROR 
      "Doxygen is needed to build the documentation. Please install it correctly")
    endif(NOT DOXYGEN_FOUND)

    #-- Add a custom target to run Doxygen when ever the project is built
    add_custom_target (Doxy_documentation  
            COMMAND ${DOXYGEN_EXECUTABLE} ${PROJECT_BINARY_DIR}/docs/doxy.config
            SOURCES ${PROJECT_BINARY_DIR}/docs/doxy.config)
endif(WITH_DOXYGENTARGET)

