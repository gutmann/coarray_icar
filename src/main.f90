program main
  use iso_fortran_env, only : input_unit
  use domain_interface, only : domain_t
  use assertions_interface, only : assert
  implicit none
  type(domain_t) :: domain

  block 
    print *,"domain%default_initialize()"
    call domain%default_initialize()
    call assert(domain%get_grid_dimensions()==[200,200,20],"default grid dimensions")
  end block
  
  print *,"domain%initialize_from_file()"
  call domain%initialize_from_file()
  
  print *,"domain%advect(dt = 4.0)"
  call domain%advect(dt = 4.0)
  
  print *,"domain%halo_exchange()"
  call domain%halo_exchange()
end program
