cmake_minimum_required(VERSION 3.7)

# Set the type/configuration of build to perform
set ( CMAKE_CONFIGURATION_TYPES "Debug" "Release" "MinSizeRel" "RelWithDebInfo" "CodeCoverage" )
set ( CMAKE_BUILD_TYPE "Release"
  CACHE STRING "Select which configuration to build." )
set_property ( CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS ${CMAKE_CONFIGURATION_TYPES} )

#Name project and specify source languages
project(coarray_icar VERSION 0.1.0 LANGUAGES Fortran)

#Print an error message on an attempt to build inside the source directory tree:
if ("${CMAKE_CURRENT_SOURCE_DIR}" STREQUAL "${CMAKE_CURRENT_BINARY_DIR}")
  message(FATAL_ERROR "ERROR! "
    "CMAKE_CURRENT_SOURCE_DIR=${CMAKE_CURRENT_SOURCE_DIR}"
    " == CMAKE_CURRENT_BINARY_DIR=${CMAKE_CURRENT_BINARY_DIR}"
    "\nThis archive does not support in-source builds:\n"
    "You must now delete the CMakeCache.txt file and the CMakeFiles/ directory under"
    "the 'src' source directory or you will not be able to configure correctly!"
    "\nYou must now run something like:\n"
    "  $ rm -r CMakeCache.txt CMakeFiles/"
    "\n"
    "Please create a directory outside the opencoarrays source tree and build under that outside directory "
    "in a manner such as\n"
    "  $ mkdir build-opencarrays\n"
    "  $ cd build-opencoarrays\n"
    "  $ CC=gcc FC=gfortran cmake <path-to-opencoarrays-source-directory> -DCMAKE_INSTALL_PREFIX=<path-to-install-directory>\n"
    "\nsubstituting the appropriate syntax for your shell (the above line assumes the bash shell)."
    )
endif()

#Report untested Fortran compiler unless explicitly directed to build all examples.
if ("${CMAKE_Fortran_COMPILER_ID}" MATCHES "GNU" )
  set(gfortran_compiler true)
  # add_definitions(-DPREFIX_NAME=_gfortran_caf_)
  set ( CMAKE_Fortran_FLAGS_CODECOVERAGE "-fprofile-arcs -ftest-coverage -O0"
    CACHE STRING "Code coverage C compiler flags")
else()
  message(WARNING
    "\n"
    "Attempting to build with untested Fortran compiler: ${CMAKE_Fortran_COMPILER_ID}. "
    "Please report any failures to opencoarrays@googlegroups.com\n\n"
  )
endif()

if(gfortran_compiler)
  set(CMAKE_REQUIRED_FLAGS "-fcoarray=single -ffree-form")
endif()
include(CheckFortranSourceCompiles)
CHECK_Fortran_SOURCE_COMPILES("
  program main
    implicit none
    integer :: i
    i = this_image()
  end program
" Check_Simple_Coarray_Fortran_Source_Compiles)
if(gfortran_compiler)
  unset(CMAKE_REQUIRED_FLAGS)
endif()

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/src)

add_subdirectory(src)

enable_testing()

# Determine if we're using Open MPI
execute_process(COMMAND ${MPIEXEC} --version
  OUTPUT_VARIABLE mpi_version_out)
if (mpi_version_out MATCHES "[Oo]pen[ -][Mm][Pp][Ii]")
  message( STATUS "OpenMPI detected")
  set ( openmpi true )
endif ()

include( ProcessorCount )
ProcessorCount(N)
function(add_mpi_test name num_mpi_proc path)
   if ( ((N LESS num_mpi_proc) OR (N EQUAL 0)) )
     message(STATUS "Test ${name} is oversubscribed: ${num_mpi_proc} ranks requested with ${N} system processor available.")
     if ( openmpi )
       if ( N LESS 2 )
	 set( num_mpi_proc 2 )
	 set (test_parameters --oversubscribed)
       else()
	 set ( num_mpi_proc ${N} )
       endif()
       message( STATUS "Open-MPI detected, over-riding oversubscribed test, ${name}, with ${num_mpi_proc} ranks." )
     endif()
   endif()
   set(test_parameters ${test_parameters} ${MPIEXEC_NUMPROC_FLAG} ${num_mpi_proc} )
   add_test(NAME ${name} COMMAND ${MPIEXEC} ${test_parameters} "${path}")
   set_property(TEST ${name} PROPERTY PASS_REGULAR_EXPRESSION "Test passed.")
endfunction(add_mpi_test)

#set(tests_root ${CMAKE_CURRENT_BINARY_DIR}/src/tests)

#if(gfortran)
#  add_mpi_test(initialize_mpi 2 ${tests_root}/unit/init_register/initialize_mpi)
#else()
#  add_test(co_sum_extension ${tests_root}/unit/extensions/test-co_sum-extension.sh)
#  set_property(TEST co_sum_extension PROPERTY PASS_REGULAR_EXPRESSION "Test passed.")
#endif()
