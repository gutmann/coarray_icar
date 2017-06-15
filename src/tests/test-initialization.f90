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
    integer :: i,nz
    print *,this_image(),"domain%initialize_from_file('input-parameters.txt')"
    call domain%initialize_from_file('input-parameters.txt')

    if (this_image()==1) then
        nz = size(domain%pressure,2)
        do i=nz,1,-1
            print *,domain%z(1,i,1), domain%pressure(1,i,1)/100, domain%temperature(1,i,1)
        end do
    endif

   print *,"domain%advect(dt = 4.0)"
   call domain%advect(dt = 4.0)

    print *,"domain%halo_exchange()"
    call domain%halo_exchange()
  end block

 print *,"Test passed."
end program
