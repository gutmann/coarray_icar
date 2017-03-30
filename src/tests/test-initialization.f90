program main
  use iso_fortran_env, only : input_unit
  use domain_interface, only : domain_t
  use assertions_interface, only : assert
  implicit none

  if (this_image()==1) print *,"Number of images = ",num_images()

  block
    type(domain_t) :: domain
    print *,"domain%default_initialize()"
    call domain%default_initialize()
  end block

  block
    type(domain_t) :: domain
    print *,"domain%initialize_from_file('input-parameters.txt')"
    call domain%initialize_from_file('input-parameters.txt')

   print *,"domain%advect(dt = 4.0)"
   call domain%advect(dt = 4.0)
  
    print *,"domain%halo_exchange()"
    call domain%halo_exchange()
  end block

 print *,"Test passed."
end program
