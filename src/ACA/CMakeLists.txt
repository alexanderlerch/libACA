set(CurrLib Aca)

## get all source files
file(GLOB ${CurrLib}_SOURCES RELATIVE ${CMAKE_SOURCE_DIR}/src/${CurrLib} *.cpp;*.txt)
file(GLOB 3RDPARTY_SOURCES RELATIVE ${CMAKE_SOURCE_DIR}/src/${CurrLib} ../../3rdparty/Fft/*.cpp)
file(GLOB ${CurrLib}_HEADERS RELATIVE ${CMAKE_SOURCE_DIR}/src/${CurrLib} *.h;../../inc/*.h;../../3rdparty/Fft/*.h)
file(GLOB HELPER_HEADERS RELATIVE ${CMAKE_SOURCE_DIR}/src/${CurrLib} ../../inc/helper/*.h)

## add include directories
include_directories(${CMAKE_SOURCE_DIR}/3rdparty/Fft ${CMAKE_SOURCE_DIR}/inc/helper)
add_inc_directories ()

## build a static library
add_library(${CurrLib} STATIC ${${CurrLib}_SOURCES} ${TOOLS_SOURCES} ${3RDPARTY_SOURCES} ${${CurrLib}_HEADERS} ${HELPER_HEADERS})
set_target_properties(${CurrLib} PROPERTIES CLEAN_DIRECT_OUTPUT 1)

## set output name
set_target_properties(${CurrLib} PROPERTIES OUTPUT_NAME "${CurrLib}")
set_target_properties(${CurrLib} PROPERTIES PREFIX "lib")

source_group("Tools" FILES ${TOOLS_SOURCES})
source_group("Header Files\\Helpers" FILES ${HELPER_HEADERS})
source_group("3rd Party" FILES ${3RDPARTY_SOURCES})

if(MSVC)
    target_compile_options(${CurrLib} PRIVATE /W4 /WX)
else (MSVC)
    target_compile_options(${CurrLib} PRIVATE -Wall)
endif(MSVC)

## set group in IDE
target_link_libraries(${CurrLib} AudioFileIO)


