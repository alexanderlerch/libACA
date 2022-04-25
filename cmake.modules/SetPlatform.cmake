## set defines for operating system and platform

if (APPLE)
	set(GTCMT_MACOSX 1)
endif (APPLE)

if (UNIX AND NOT APPLE)
	## get bit depth
	if ( CMAKE_SIZEOF_VOID_P EQUAL 4)
		set(GTCMT_LINUX32 1)
	else ( CMAKE_SIZEOF_VOID_P EQUAL 4 )
		set(GTCMT_LINUX64 1)
	endif ( CMAKE_SIZEOF_VOID_P EQUAL 4 )
endif (UNIX AND NOT APPLE)

if(MINGW)
	if ( CMAKE_SIZEOF_VOID_P EQUAL 4)
		set(GTCMT_WIN32 1)
	else ( CMAKE_SIZEOF_VOID_P EQUAL 4 )
		set(GTCMT_WIN64 1)
	endif ( CMAKE_SIZEOF_VOID_P EQUAL 4 )
endif(MINGW)

if (MSVC)
	if ( CMAKE_CL_64 EQUAL 0 )
		set(GTCMT_WIN32 1)
	else ( CMAKE_CL_64 EQUAL 0 )
		set(GTCMT_WIN64 1)
	endif ( CMAKE_CL_64 EQUAL 0 )
    foreach (flag_var
             CMAKE_C_FLAGS CMAKE_C_FLAGS_DEBUG CMAKE_C_FLAGS_RELEASE
             CMAKE_C_FLAGS_MINSIZEREL CMAKE_C_FLAGS_RELWITHDEBINFO
             CMAKE_CXX_FLAGS CMAKE_CXX_FLAGS_DEBUG CMAKE_CXX_FLAGS_RELEASE
             CMAKE_CXX_FLAGS_MINSIZEREL CMAKE_CXX_FLAGS_RELWITHDEBINFO)
      if (NOT BUILD_SHARED_LIBS AND NOT gtest_force_shared_crt)
		set (CMAKE_MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")    
        # When Google Test is built as a shared library, it should also use
        # shared runtime libraries.  Otherwise, it may end up with multiple
        # copies of runtime library data in different modules, resulting in
        # hard-to-find crashes. When it is built as a static library, it is
        # preferable to use CRT as static libraries, as we don't have to rely
        # on CRT DLLs being available. CMake always defaults to using shared
        # CRT libraries, so we override that default here.
        string(REPLACE "/MD" "-MT" ${flag_var} "${${flag_var}}")
      endif()

      # We prefer more strict warning checking for building Google Test.
      # Replaces /W3 with /W4 in defaults.
      string(REPLACE "/W3" "/W4" ${flag_var} "${${flag_var}}")

      # Prevent D9025 warning for targets that have exception handling
      # turned off (/EHs-c- flag). Where required, exceptions are explicitly
      # re-enabled using the cxx_exception_flags variable.
      string(REPLACE "/EHsc" "/fp:precise" ${flag_var} "${${flag_var}}")
	set (CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG}" CACHE STRING "" FORCE)    
	set (CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE}" CACHE STRING "" FORCE)    
	  
    endforeach()
endif (MSVC)