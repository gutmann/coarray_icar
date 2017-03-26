set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod)

add_library( icar_utils
  assertions_implementation.f90
  assertions_interface.F90
  configuration_implementation.f90
  configuration_interface.f90
)

install(TARGETS icar_utils DESTINATION lib)
