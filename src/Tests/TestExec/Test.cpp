#define _CRT_SECURE_NO_WARNINGS

#include "ACAConfig.h"

#include <gtest/gtest.h>

#if defined(ACA_WIN64) && defined(_DEBUG)
    #include "windows.h"
    #define _CRTDBG_MAP_ALLOC //to get more details
    #include <crtdbg.h>   //for malloc and free
#endif

std::string cTestDataDir;

GTEST_API_ int main(int argc, char** argv) 
{
    printf("Running main() from %s\n", __FILE__);
    testing::InitGoogleTest(&argc, argv);
 
#if defined(ACA_WIN64) && defined(_DEBUG)
     _CrtSetDbgFlag(_CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF);
    //_CrtSetBreakAlloc(2677);
#endif

    // quick hack for FILE IO test
    if (argc > 1)
        cTestDataDir.assign(argv[1]);
    else
    {
        cTestDataDir.assign(CMAKE_SOURCE_DIR);
        cTestDataDir.append("/src/Tests/TestData/");
    }

    return  RUN_ALL_TESTS();
}
