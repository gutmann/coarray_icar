set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod)

#Toggle C preprocessor macro for turning assertions on or off
if(NO_ASSERTIONS)
  set_source_files_properties(assertions_interface.f90 PROPERTIES COMPILE_FLAGS "-cpp -DUSE_ASSERTIONS=.false.")
else()
  set_source_files_properties(assertions_interface.f90 PROPERTIES COMPILE_FLAGS "-cpp -DUSE_ASSERTIONS=.true.")
endif()

add_library( icar_utils
  assertions_implementation.f90
  assertions_interface.f90
  configuration_implementation.f90
  configuration_interface.f90
)

install(TARGETS icar_utils DESTINATION lib)
