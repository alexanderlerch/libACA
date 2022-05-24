set(CurrLib AudioFileIO)

## set version number
set (${CurrLib}_VERSION_MAJOR "0" CACHE INTERNAL "")
set (${CurrLib}_VERSION_MINOR "0" CACHE INTERNAL "")
set (${CurrLib}_VERSION_PATCH "1" CACHE INTERNAL "")

# get source files
file(GLOB ${CurrLib}_SOURCES RELATIVE ${CMAKE_SOURCE_DIR}/src/${CurrLib} *.c;*.cpp;*.txt)
file(GLOB SNDLIB_SOURCES RELATIVE ${CMAKE_SOURCE_DIR}/src/${CurrLib} ../../3rdparty/sndlib/*.c;../../3rdparty/sndlib/*.cpp)
file(GLOB ${CurrLib}_HEADERS RELATIVE ${CMAKE_SOURCE_DIR}/src/${CurrLib} *.h;../inc/*.h)
file(GLOB SNDLIB_HEADERS RELATIVE ${CMAKE_SOURCE_DIR}/src/${CurrLib} ../../3rdparty/sndlib/*.h)

#remove mains
list(REMOVE_ITEM SNDLIB_SOURCES RELATIVE ${CMAKE_SOURCE_DIR}/src/${CurrLib} ../../3rdparty/sndlib/sndplay.c)
list(REMOVE_ITEM SNDLIB_SOURCES RELATIVE ${CMAKE_SOURCE_DIR}/src/${CurrLib} ../../3rdparty/sndlib/sndinfo.c)


## add include directories
include_directories(${CMAKE_SOURCE_DIR}/3rdparty/sndlib)
add_inc_directories ()

# build a static library
add_library(${CurrLib} STATIC ${${CurrLib}_SOURCES} ${${CurrLib}_HEADERS} ${SNDLIB_SOURCES} ${SNDLIB_HEADERS})
set_target_properties(${CurrLib} PROPERTIES CLEAN_DIRECT_OUTPUT 1)

# set output name
set_target_properties(${CurrLib} PROPERTIES OUTPUT_NAME "${CurrLib}")
set_target_properties(${CurrLib} PROPERTIES PREFIX "lib")

source_group("3rd Party" FILES ${SNDLIB_SOURCES} ${SNDLIB_HEADERS})

if(MSVC)
    #set_target_properties(${CurrLib} PROPERTIES COMPILE_FLAGS "/W1 /DMUS_WINDOZE")
    target_compile_options(${CurrLib} PRIVATE /W1 /DMUS_WINDOZE /wd4273 /wd4090 /wd4047)
endif(MSVC)

if (APPLE)
    set_target_properties(${CurrLib} PROPERTIES COMPILE_FLAGS "-w")
endif(APPLE)

set_target_properties(${CurrLib} PROPERTIES FOLDER AudioIO)

