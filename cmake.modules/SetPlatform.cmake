## set defines for operating system and platform

if (APPLE)
	set(ACA_MACOSX 1)
endif (APPLE)

if (UNIX AND NOT APPLE)
	## get bit depth
	if ( CMAKE_SIZEOF_VOID_P EQUAL 4)
		set(ACA_LINUX32 1)
	else ( CMAKE_SIZEOF_VOID_P EQUAL 4 )
		set(ACA_LINUX64 1)
	endif ( CMAKE_SIZEOF_VOID_P EQUAL 4 )
endif (UNIX AND NOT APPLE)

if(MINGW)
	if ( CMAKE_SIZEOF_VOID_P EQUAL 4)
		set(ACA_WIN32 1)
	else ( CMAKE_SIZEOF_VOID_P EQUAL 4 )
		set(ACA_WIN64 1)
	endif ( CMAKE_SIZEOF_VOID_P EQUAL 4 )
endif(MINGW)

if (MSVC)
	if ( CMAKE_CL_64 EQUAL 0 )
		set(ACA_WIN32 1)
	else ( CMAKE_CL_64 EQUAL 0 )
		set(ACA_WIN64 1)
	endif ( CMAKE_CL_64 EQUAL 0 )

	add_compile_options("-D_USE_MATH_DEFINES" "/wd26812")
	  
endif (MSVC)
set (CMAKE_CXX_STANDARD 11)