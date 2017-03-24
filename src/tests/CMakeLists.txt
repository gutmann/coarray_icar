add_executable( initialization-test
  test-initialization.f90
  ../objects/domain_implementation.f90
  ../objects/domain_interface.f90
  ../objects/exchangeable_implementation.f90
  ../objects/exchangeable_interface.f90
  ../utilities/assertions_implementation.f90
  ../utilities/assertions_interface.F90
  ../utilities/configuration_implementation.f90
  ../utilities/configuration_interface.f90
)
configure_file(input-parameters.txt ${CMAKE_BINARY_DIR}/input-parameters.txt COPYONLY)
