#define _CRT_SECURE_NO_WARNINGS

#include "ACAConfig.h"

#include <gtest/gtest.h>

std::string cTestDataDir;

GTEST_API_ int main(int argc, char** argv) {
    printf("Running main() from %s\n", __FILE__);
    testing::InitGoogleTest(&argc, argv);
 
    // quick hack for FILE IO test
    if (argc > 1)
        cTestDataDir.assign(argv[1]);
    else
    {
        cTestDataDir.assign(CMAKE_SOURCE_DIR);
        cTestDataDir.append("/src/Tests/TestData/");
    }

    return RUN_ALL_TESTS();
}
