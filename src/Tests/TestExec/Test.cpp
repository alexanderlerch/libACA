#define _CRT_SECURE_NO_WARNINGS

#include "ACAConfig.h"


#define CATCH_CONFIG_RUNNER
#include "../3rdparty/catch2/catch.hpp"

#if defined(ACA_WIN64) && defined(_DEBUG)
#include "windows.h"
#define _CRTDBG_MAP_ALLOC //to get more details
#include <crtdbg.h>   //for malloc and free
#endif

using namespace Catch::clara;

std::string cTestDataDir;

int main(int argc, char* argv[]) {
    Catch::Session session;

    auto cli = session.cli()
        | Opt(cTestDataDir, "test data directory")
        ["-x"]["--testdatadir"]
        ("test data directory");

    session.cli(cli);

    auto ret = session.applyCommandLine(argc, argv);
    if (ret) {
        return ret;
    }

    // quick hack for FILE IO test
    if (cTestDataDir.empty())
    {
        cTestDataDir.assign(CMAKE_SOURCE_DIR);
        cTestDataDir.append("/src/Tests/TestData/");
    }

#if defined(ACA_WIN64) && defined(_DEBUG)
    _CrtSetDbgFlag(_CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF | _CRTDBG_CHECK_ALWAYS_DF);
    //_CrtSetBreakAlloc(18111);
#endif

    return session.run();
}
