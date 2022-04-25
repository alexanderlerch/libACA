function (add_src_directories)
    ## header of dependencies printoutmessage(STATUS "")
    message(STATUS "******************")
    message(STATUS "Targets added:")
     
    ## add dependencies from list and print out the name
    foreach(PATH ${PROJECT_DIRECTORIES})
        get_filename_component( FILENAME ${PATH}
                                NAME_WE)
        message(STATUS ${FILENAME})
        add_subdirectory(${PATH} "${CMAKE_CURRENT_BINARY_DIR}/${FILENAME}")
    endforeach(PATH) 
     
    ## footer of dependencies printout
    message(STATUS "******************")
    message(STATUS "")
endfunction(add_src_directories)