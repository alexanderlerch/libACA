set (APP TestExec)

set (TestLIB gtest)

set (AudioLIB AudioFileIO)
set (AcaLIB Aca)


list (APPEND PROJECT_INCLUDES ${CMAKE_SOURCE_DIR}/3rdparty/googletest/${TestLIB}/include)
list (APPEND PROJECT_INCLUDES ${CMAKE_SOURCE_DIR}/3rdparty/googletest/${TestLIB})

##############################################################
## TestExec
file(GLOB ${APP}_SOURCES RELATIVE ${CMAKE_SOURCE_DIR}/src/Tests/${APP} Test.cpp)
file(GLOB ${APP}_HEADERS RELATIVE ${CMAKE_SOURCE_DIR}/src/Tests/${APP} *.h;../../../inc/*.h;)
file(GLOB HELPER_HEADERS RELATIVE ${CMAKE_SOURCE_DIR}/src/Tests/${APP} ../../../inc/helper/*.h)

# these are the individual tests
file(GLOB ${APP}_SOURCES_TEST RELATIVE ${CMAKE_SOURCE_DIR}/src/Tests/${APP} ../Tests/*.cpp;)
file(GLOB ${APP}_HEADERS_TEST RELATIVE ${CMAKE_SOURCE_DIR}/src/Tests/${APP} ../Tests/*.h)

## add include directories
include_directories(${CMAKE_SOURCE_DIR}/inc/helper)
add_inc_directories ()

add_executable(${APP} ${${APP}_SOURCES} ${${APP}_HEADERS} ${${APP}_SOURCES_TEST} ${${APP}_HEADERS_TEST} ${HELPER_HEADERS})

# link the libraries
target_link_libraries(${APP} ${LIB} ${TestLIB} ${AcaLIB} ${AudioLIB} ${OS_LIBS})

set_target_properties(${APP} PROPERTIES FOLDER Testing)
source_group("Header Files\\Helpers" FILES ${HELPER_HEADERS})

set_target_properties(gtest PROPERTIES FOLDER Testing)
set_target_properties(gtest_main PROPERTIES FOLDER Testing)

