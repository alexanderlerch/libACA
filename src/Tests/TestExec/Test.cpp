#define _CRT_SECURE_NO_WARNINGS

#include "MUSI6106Config.h"

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





#ifdef UNITTESTPLUSPLUS
#include <UnitTest++.h>
#include "TestReporterStdout.h"

#include "MUSI6106Config.h"

// include project headers
#include "AudioFileIf.h"

#define WITH_FLOATEXCEPTIONS
#define WITH_MEMORYCHECK

#if (!defined(NDEBUG) && defined (GTCMT_WIN32))
    // include exception header
    #if defined(WITH_FLOATEXCEPTIONS) 
        #include <float.h>
    #endif // #ifndef WITHOUT_EXCEPTIONS

    // include memory leak header
    #if defined(WITH_MEMORYCHECK)
        #define CRTDBG_MAP_ALLOC
        #include <stdlib.h>
        #include <crtdbg.h>
    #else
        #include <stdlib.h>
    #endif
#endif
std::string cTestDataDir;

/////////////////////////////////////////////////////////////////////////////////
// main function
int main(int argc, char* argv[])
{

    // detect memory leaks in win32
#if (defined(WITH_MEMORYCHECK) && !defined(NDEBUG) && defined (GTCMT_WIN32))
    // set memory checking flags
    int iDbgFlag = _CrtSetDbgFlag(_CRTDBG_REPORT_FLAG);
    iDbgFlag |= _CRTDBG_CHECK_ALWAYS_DF;
    iDbgFlag |= _CRTDBG_LEAK_CHECK_DF;
    _CrtSetDbgFlag( iDbgFlag );

    //_CrtSetBreakAlloc(245);
#endif

    // enable floating point exceptions in win32
#if (defined(WITH_FLOATEXCEPTIONS) && !defined(NDEBUG) && defined (GTCMT_WIN32))
    // enable check for exceptions (don't forget to enable stop in MSVC!)
    _controlfp(~(_EM_INVALID | _EM_ZERODIVIDE | _EM_OVERFLOW | _EM_UNDERFLOW | _EM_DENORMAL), _MCW_EM) ;
#endif // #ifndef WITHOUT_EXCEPTIONS


    // argument 2 contains the working dir
    if (argc > 2)
        cTestDataDir.assign(argv[2]);
    
    // see http://stackoverflow.com/questions/3546054/how-do-i-run-a-single-test-with-unittest
    if( argc > 1 )
    {
        //walk list of all tests, add those with a name that
        //matches one of the arguments  to a new TestList
        const UnitTest::TestList& allTests( UnitTest::Test::GetTestList() );
        UnitTest::TestList selectedTests;
        UnitTest::Test* p = allTests.GetHead();
        while( p )
        {
            if( strcmp( p->m_details.suiteName, argv[ 1 ] ) == 0 )
            {
                selectedTests.Add( p );
            }
            p = p->m_nextTest;
        }
        selectedTests.Add(0);

        if (!selectedTests.GetHead())
            return -1;

        //run selected test(s) only
        UnitTest::TestReporterStdout reporter;
        UnitTest::TestRunner runner( reporter );
        return runner.RunTestsIf( selectedTests, 0, UnitTest::True(), 0 );
    }
    else
    {
        return UnitTest::RunAllTests();
    }
}

#endif // UNITTESTPLUSPLUS
