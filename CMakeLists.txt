cmake_minimum_required(VERSION 3.10)
# Set the type/configuration of build to perform
set ( CMAKE_CONFIGURATION_TYPES "Debug" "Release" "MinSizeRel" "RelWithDebInfo" )
set ( CMAKE_BUILD_TYPE "Release"
  CACHE STRING "Select which configuration to build." )
set_property ( CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS ${CMAKE_CONFIGURATION_TYPES} )

# Name project and specify source languages
project(morfeus VERSION "0.1" LANGUAGES Fortran)

#Print an error message on an attempt to build inside the source directory tree:
if ("${CMAKE_CURRENT_SOURCE_DIR}" STREQUAL "${CMAKE_CURRENT_BINARY_DIR}")
  message(FATAL_ERROR "ERROR! "
    "CMAKE_CURRENT_SOURCE_DIR=${CMAKE_CURRENT_SOURCE_DIR}"
    " == CMAKE_CURRENT_BINARY_DIR=${CMAKE_CURRENT_BINARY_DIR}"
    "\nThis archive does not support in-source builds:\n"
    "You must now delete the CMakeCache.txt file and the CMakeFiles/ directory under "
    "the 'src' source directory or you will not be able to configure correctly!"
    "\nYou must now run something like:\n"
    "  $ rm -r CMakeCache.txt CMakeFiles/"
    "\n"
    "Please create a directory outside the opencoarrays source tree and build under that outside directory "
    "in a manner such as\n"
    "  $ mkdir -p coarray_icar/build\n"
    "  $ cd coarray_icar/build\n"
    "  $ FC=caf cmake ..\n"
    "\nsubstituting the appropriate syntax for your shell (the above line assumes the bash shell)."
    )
endif()

set (CMAKE_Fortran_FLAGS_DEBUG "-g -DUSE_ASSERTIONS=.true." CACHE
  STRING "Flags used by the compiler during debug builds."
  FORCE)
set (CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-g -O2 -DNDEBUG -DUSE_ASSERTIONS=.true." CACHE
  STRING "Flags used by the compiler during release builds with debug info."
  FORCE)

if (NOT "${CMAKE_Fortran_COMPILER_ID}" MATCHES "GNU|Intel|Cray" )
  message(WARNING
    "\n"
    "Attempting to build with untested Fortran compiler: ${CMAKE_Fortran_COMPILER_ID}. "
    "Please report any failures at https://github.com/gutmann/coarray_icar/issues\n\n"
  )
endif()

if ("${CMAKE_Fortran_COMPILER_ID}" MATCHES "Intel" )
  if (DEFINED DISTRIBUTED)
    set(coarray_flags "-coarray=distributed")
  else()
    set(coarray_flags "-coarray=shared")
  endif()
endif()

set(CMAKE_Fortran_FLAGS_DEBUG "${coarray_flags} ${CMAKE_Fortran_FLAGS_DEBUG}")
set(CMAKE_Fortran_FLAGS_RELEASE "${coarray_flags} ${CMAKE_Fortran_FLAGS_RELEASE}")
set(CMAKE_Fortran_FLAGS_MINSIZEREL "${coarray_flags} ${CMAKE_Fortran_FLAGS_MINSIZEREL}")
set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "${coarray_flags} ${CMAKE_Fortran_FLAGS_RELWITHDEBINFO}")

set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod)

include(CheckFortranSourceCompiles)
check_fortran_source_compiles("
  program main
    implicit none
    integer :: i[*]
    i = this_image()
  end program
" Check_Simple_Coarray_Fortran_Source_Compiles
  SRC_EXT ".f90")

check_fortran_source_compiles("
  program main
  contains
  pure function foo() result(res)
    error stop 'Error stop is supported in pure functions (F2018)'
  end function
  end program
"
  HAVE_ERROR_STOP_IN_PURE
  SRC_EXT ".f90"
  )
if(HAVE_ERROR_STOP_IN_PURE)
  add_definitions(-DHAVE_ERROR_STOP_IN_PURE)
endif()

check_fortran_source_compiles("
  program main
  integer :: i
  i = 0
  error stop i
  end program
"
  HAVE_NON_CONSTANT_ERROR_STOP
  SRC_EXT ".f90")
if(HAVE_NON_CONSTANT_ERROR_STOP)
  add_definitions(-DHAVE_NON_CONSTANT_ERROR_STOP)
endif()

add_subdirectory(src)

enable_testing()

set(test_dir "${CMAKE_BINARY_DIR}/src/tests")
include( ProcessorCount )
ProcessorCount(NUM_IMAGES)
function(add_caf_test name num_caf_img test_target)
  # Function to add MPI tests.
  if(TARGET ${test_target})
    get_target_property(min_test_imgs ${test_target} MIN_IMAGES)
  elseif(TARGET build_${test_target})
    get_target_property(min_test_imgs build_${test_target} MIN_IMAGES)
  endif()
  if(min_test_imgs)
    if(num_caf_img LESS min_test_imgs)
      message( FATAL_ERROR "Test ${name} requires ${min_test_imgs} but was only given ${num_caf_images}" )
    endif()
  endif()
  if ( ((NUM_IMAGES LESS num_caf_img) OR (NUM_IMAGES EQUAL 0)) )
    message(STATUS "Test ${name} is oversubscribed: ${num_caf_img} CAF images requested with ${NUM_IMAGES} system processor available.")
    if ( openmpi )
      if (min_test_imgs)
        set( num_caf_img ${min_test_imgs} )
      elseif ( NUM_IMAGES LESS 2 )
        set( num_caf_img 2 )
      endif()
      set (test_parameters --oversubscribe)
      message( STATUS "Open-MPI back end detected, passing --oversubscribe for oversubscribed test, ${name}, with ${num_caf_img} ranks/images." )
    endif()
  endif()
  set(test_parameters -np ${num_caf_img} ${test_parameters})
  if ("${CMAKE_Fortran_COMPILER_ID}" MATCHES "GNU" )
    add_test(NAME ${name} COMMAND "bash" cafrun ${test_parameters} "${test_dir}/${test_target}")
  elseif ("${CMAKE_Fortran_COMPILER_ID}" MATCHES "Intel" )
    add_test(NAME ${name} COMMAND "${test_dir}/${test_target}")
  elseif ("${CMAKE_Fortran_COMPILER_ID}" MATCHES "Cray" )
    add_test(NAME ${name} COMMAND aprun ${test_parameters} "${test_dir}/${test_target}")
  elseif()
    message(WARNING "Unsupported compiler: tests might not launch correctly.")
    add_test(NAME ${name} COMMAND "${test_dir}/${test_target}")
  endif()
  set_property(TEST ${name} PROPERTY PASS_REGULAR_EXPRESSION "Test passed.")
endfunction(add_caf_test)

add_caf_test(test-ideal 4 test-ideal)
