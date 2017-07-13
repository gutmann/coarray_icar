program main
  use iso_fortran_env, only : input_unit
  use domain_interface, only : domain_t
  use assertions_interface, only : assert
  use module_mp_driver, only: microphysics
  implicit none

  if (this_image()==1) print *,"Number of images = ",num_images()


  block
    type(domain_t) :: domain
    if (num_images() < 50) then
        if (this_image()==1) print *,"domain%default_initialize()"
        call domain%default_initialize()
    else
        if (this_image()==1) then
            print *, "Skipping default initialization because there are too many images."
        endif
    endif
  end block

  block
    type(domain_t) :: domain
    integer :: i,nz, ypos
    real :: start, finish

    if (this_image()==1) print *,this_image(),"domain%initialize_from_file('input-parameters.txt')"
    call domain%initialize_from_file('input-parameters.txt')

    if (this_image()==1) then
        nz = size(domain%pressure,2)
        print *, " Layer height       Pressure        Temperature      Water Vapor"
        print *, "     [m]              [hPa]             [K]            [kg/kg]"
        do i=nz,1,-4
            print *,domain%z(1,i,1), domain%pressure(1,i,1)/100, domain%temperature(1,i,1), domain%water_vapor%local(1,i,1)
        end do
    endif

    ypos = (ubound(domain%accumulated_precipitation,2)-lbound(domain%accumulated_precipitation,2))/2
    ypos = ypos + lbound(domain%accumulated_precipitation,2)
    sync all
    call cpu_time(start)
    do i=1,200
        ! print *,"Microphysics"
        ! note should this be wrapped into the domain object(?)
        call microphysics(domain, dt = 20.0)
        ! print *,"domain%advect(dt = 4.0)"
        call domain%advect(dt = 1.0)

        ! print *,"domain%halo_exchange()"
        call domain%halo_exchange()

        ! if (this_image()==(num_images()/2)) then
        !     print*, this_image(), i, domain%accumulated_precipitation(::10,ypos)
        ! endif
        ! call domain%enforce_limits()
    end do
    call cpu_time(finish)

    sync all
    if (this_image()==1) then
        print *,"Model run time:",finish-start
    endif
  end block

 if (this_image()==1) print *,"Test passed."

end program
