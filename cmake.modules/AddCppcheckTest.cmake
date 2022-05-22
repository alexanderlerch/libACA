function (add_cppcheck_test)
	find_package(cppcheck)
	if (CPPCHECK_FOUND)
		# copy list just to be sure
		list(APPEND CPPCHK_DEPLIST ${PROJECT_DIRECTORIES})
		list(FILTER CPPCHK_DEPLIST EXCLUDE REGEX ".*test*")

		# generate include list with "-I" prefix
		foreach(arg ${PROJECT_INCLUDES})
		   set(CPPCHK_INCL "${CPPCHK_INCL} -I ${arg}")
		endforeach(arg ${PROJECT_INCLUDES})

		# add the test itself!
		add_test(CppCheck ${CPPCHECK_EXECUTABLE} "--error-exitcode=1" ${CPPCHK_DEPLIST} ${CPPCHK_INCL})
	endif (CPPCHECK_FOUND)
endfunction (add_cppcheck_test)
