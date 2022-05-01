set(CurrLib Aca)

set (ToolsLIB Tools)

# get all source files
file(GLOB ${CurrLib}_SOURCES RELATIVE ${CMAKE_SOURCE_DIR}/src/${CurrLib} *.cpp;*.txt;../../3rdparty/Fft/*.cpp)
file(GLOB ${CurrLib}_HEADERS RELATIVE ${CMAKE_SOURCE_DIR}/src/${CurrLib} *.h;../../inc/*.h;../../inc/helper/*.h;../../3rdparty/Fft/*.h)

## add include directories
include_directories(${CMAKE_SOURCE_DIR}/3rdparty/Fft)
add_inc_directories ()

# build a static library
add_library(${CurrLib} STATIC ${${CurrLib}_SOURCES} ${${CurrLib}_HEADERS})
set_target_properties(${CurrLib} PROPERTIES CLEAN_DIRECT_OUTPUT 1)

# set output name
set_target_properties(${CurrLib} PROPERTIES OUTPUT_NAME "${CurrLib}")
set_target_properties(${CurrLib} PROPERTIES PREFIX "lib")

target_link_libraries(${CurrLib} ${ToolsLIB})

