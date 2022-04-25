set(CurrLib AudioFileIO)

## set version number
set (${CurrLib}_VERSION_MAJOR "0" CACHE INTERNAL "")
set (${CurrLib}_VERSION_MINOR "0" CACHE INTERNAL "")
set (${CurrLib}_VERSION_PATCH "1" CACHE INTERNAL "")

# get source files
file(GLOB ${CurrLib}_SOURCES RELATIVE ${CMAKE_SOURCE_DIR}/src/${CurrLib} *.c;*.cpp;*.txt;../../3rdparty/sndlib/*.c;../../3rdparty/sndlib/*.cpp;../../3rdparty/sndlib/*.txt)
file(GLOB ${CurrLib}_HEADERS RELATIVE ${CMAKE_SOURCE_DIR}/src/${CurrLib} *.h;../inc/*.h;../../3rdparty/sndlib/*.h)
include_directories(${CMAKE_SOURCE_DIR}/3rdparty/sndlib)

## add include directories
add_inc_directories ()

# build a static library
add_library(${CurrLib} STATIC ${${CurrLib}_SOURCES} ${${CurrLib}_HEADERS})
set_target_properties(${CurrLib} PROPERTIES CLEAN_DIRECT_OUTPUT 1)

# set output name
set_target_properties(${CurrLib} PROPERTIES OUTPUT_NAME "${CurrLib}")
set_target_properties(${CurrLib} PROPERTIES PREFIX "lib")

if(MSVC)
    set_target_properties(${CurrLib} PROPERTIES COMPILE_FLAGS "/W1 /DMUS_WINDOZE")
endif(MSVC)

if (APPLE)
    set_target_properties(${CurrLib} PROPERTIES COMPILE_FLAGS "-w")
endif(APPLE)

