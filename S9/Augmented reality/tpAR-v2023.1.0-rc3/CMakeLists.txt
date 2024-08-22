cmake_minimum_required(VERSION 3.1)
include(ExternalProject)

project( TP_Interface_AR VERSION 2020.1.0 LANGUAGES CXX)

# guard against in-source builds
if(${CMAKE_SOURCE_DIR} STREQUAL ${CMAKE_BINARY_DIR})
    message(FATAL_ERROR "In-source builds not allowed.")
endif()

# set the path where we can find the findXXX.cmake
set(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/.cmake)

# add a search path for the glm libraries
# it will work if they are already compiled in /3rdparty
if(NOT DEFINED GLM_DIR)
    set(GLM_DIR ${PROJECT_SOURCE_DIR}/3rdparty/glm/build)
endif() 
list(APPEND CMAKE_LIBRARY_PATH "${GLM_DIR}/lib")
list(APPEND CMAKE_INCLUDE_PATH "${GLM_DIR}/include")


# set the output path for the generated files
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/bin)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/lib )

#########################################################
# SET COMPILATION FLAGS FOR C++11
#########################################################

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

#########################################################
#
# EXTERNAL LIBRARIES
#
#########################################################

#########################################################
# LIB GLM
#########################################################

#try to find the libraries
find_package(GLM )

if(NOT GLM_FOUND)

    # Fallback is only for *nix systems
    if(MSVC)
        message(FATAL_ERROR "You need to provide the location of the GLM library through the GLM_DIR 
            variable, e.g. -DGLM_DIR:PATH=<install path>")
    endif()

    #just to be sure
    set(GLM_DIR ${PROJECT_SOURCE_DIR}/3rdparty/glm)

    # set(EP_PREFIX ${CMAKE_CURRENT_BINARY_DIR}/3rdpary)
    # set_property(DIRECTORY PROPERTY EP_BASE ${PROJECT_SOURCE_DIR}/3rdparty)
    ExternalProject_Add( glm
                        URL http://devernay.free.fr/hacks/glm/glm-0.3.2.tar.gz
                        PREFIX ${GLM_DIR}
                        BUILD_IN_SOURCE 1
                        INSTALL_DIR ${GLM_DIR}/build
                        CONFIGURE_COMMAND ./configure --prefix=${GLM_DIR}/build --enable-shared
                        BUILD_COMMAND make -j 4
                        INSTALL_COMMAND make install)

    ExternalProject_Get_Property(glm install_dir)
    message( "lib glm has not be found, it will be installed in: ${install_dir}")
    set(LIBGLM_INCLUDE_DIRS ${install_dir}/include)   
    # message( "lib glm has not be found, it will be installed in: ${LIBGLM_INCLUDE_DIRS}")

    add_library(glm_lib SHARED IMPORTED)
    set_property(TARGET glm_lib PROPERTY IMPORTED_LOCATION ${install_dir}/lib/libglm${CMAKE_SHARED_LIBRARY_SUFFIX})
    add_dependencies(glm_lib glm)

    set(LIBGLM_LIBRARIES glm_lib)

    set(GLM_FOUND ON)

else()
    message( "-- Found glm" )
    message( STATUS "LIBGLM_INCLUDE_DIRS ${LIBGLM_INCLUDE_DIRS}" )
    message( STATUS "LIBGLM_LIBRARIES ${LIBGLM_LIBRARIES}" )
endif(NOT GLM_FOUND)



#########################################################
# FIND OPENCV
#########################################################
if(MSVC)
    set(OpenCV_STATIC ON)
endif()
find_package(OpenCV 2.4 REQUIRED)
include_directories(${OpenCV_INCLUDE_DIRS})
link_directories(${OpenCV_LIB_DIR})
add_definitions(${OpenCV_DEFINITIONS})
# message( STATUS "OpenCV_LIB_DIR ${OpenCV_LIB_DIR}" )
# message( STATUS "OpenCV_VERSION ${OpenCV_INCLUDE_DIRS}" )
# message( STATUS "OpenCV_VERSION ${OpenCV_VERSION}" )
# message( STATUS "OpenCV_FOUND ${OpenCV_FOUND}" )

if(NOT OpenCV_FOUND)
    message(ERROR " OpenCV not found!")
else()
    message( "-- Found OpenCV version: ${OpenCV_VERSION}" )
endif(NOT OpenCV_FOUND)

#########################################################
# FIND OPENGL
#########################################################
find_package(OpenGL REQUIRED)
include_directories(${OpenGL_INCLUDE_DIRS})
link_directories(${OpenGL_LIBRARY_DIRS})
add_definitions(${OpenGL_DEFINITIONS})
# MESSAGE( "${OPENGL_LIBRARIES}" )
# MESSAGE( "${OPENGL_LIBRARIES_DIRS}" )
# MESSAGE( "${OPENGL_DEFINITIONS}" )
if(NOT OPENGL_FOUND)
    message(ERROR " OPENGL not found!")
endif(NOT OPENGL_FOUND)

#########################################################
# FIND GLUT
#########################################################
if(MSVC)
    message(STATUS "GLUT_ROOT_PATH: ${GLUT_ROOT_PATH}")
endif()
find_package(GLUT REQUIRED)
message(STATUS "GLUT_FOUND: ${GLUT_FOUND}")
message(STATUS "GLUT_INCLUDE_DIR: ${GLUT_INCLUDE_DIR}")
message(STATUS "GLUT_LIBRARIES: ${GLUT_LIBRARIES}")
include_directories(${GLUT_INCLUDE_DIR})
link_directories(${GLUT_LIBRARY_DIRS})
add_definitions(${GLUT_DEFINITIONS})
# message( "glut ${GLUT_INCLUDE_DIR}" )
if(NOT GLUT_FOUND)
    message(ERROR " GLUT not found!")
else()
    message( "-- Found GLUT" )
endif(NOT GLUT_FOUND)


## FreeGLUT ?
# find_package(FreeGLUT REQUIRED)
# include_directories(${FreeGLUT_INCLUDE_DIRS})
# link_directories(${FreeGLUT_LIBRARY_DIRS})
# add_definitions(${FreeGLUT_DEFINITIONS})
# MESSAGE( "${FREEGLUT_INCLUDE_PATH}" )
# MESSAGE( "${FREEGLUT_LIBRARY}" )
# MESSAGE( "${FreeGLUT_DEFINITIONS}" )
# if(NOT FREEGLUT_FOUND)
#     message(ERROR " FreeGLUT not found!")
# endif(NOT FREEGLUT_FOUND)

#########################################################
# Doxygen
#########################################################

# add a target to generate API documentation with Doxygen
find_package(Doxygen QUIET)
# message( "${DOXYGEN_EXECUTABLE}" )
if(DOXYGEN_FOUND)
    set(CMAKE_DOC_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/doc)
    configure_file(${CMAKE_CURRENT_SOURCE_DIR}/doc/Doxyfile.in ${CMAKE_DOC_OUTPUT_DIRECTORY}/Doxyfile @ONLY)
    add_custom_target(doc
            ${DOXYGEN_EXECUTABLE} ${CMAKE_DOC_OUTPUT_DIRECTORY}/Doxyfile
            WORKING_DIRECTORY ${CMAKE_DOC_OUTPUT_DIRECTORY}
            COMMENT "Generating API documentation with Doxygen" VERBATIM)
endif(DOXYGEN_FOUND)



#########################################################
#
# PROJECT LIBRARIES
#
#########################################################

#########################################################
# TRACKER LIBRARY
#########################################################

add_subdirectory(src)


include_directories(${CMAKE_CURRENT_SOURCE_DIR}/src/tracker)

