include_directories(${CMAKE_BINARY_DIR}/mod)

add_executable(test-ideal
  test-ideal.f90
  ../objects/domain_implementation.f90
  ../objects/domain_interface.f90
  ../objects/exchangeable_implementation.f90
  ../objects/exchangeable_interface.f90
  ../objects/grid_implementation.f90
  ../objects/grid_interface.f90
  ../objects/timer_implementation.f90
  ../objects/timer_interface.f90
  ../physics/mp_driver.f90
  ../physics/mp_thompson.f90
)
target_link_libraries(test-ideal icar_utils)

configure_file(input-parameters.txt ${CMAKE_BINARY_DIR}/input-parameters.txt COPYONLY)

install(TARGETS test-ideal DESTINATION bin)
