program main
  use iso_fortran_env, only : input_unit
  use domain_interface, only : domain_t
  use assertions_interface, only : assert
  use module_mp_driver, only: microphysics
  use timer_interface, only: timer_t
  implicit none

  if (this_image()==1) print *,"Number of images = ",num_images()


  block
    type(domain_t), save :: domain
    integer :: i,nz, ypos
    type(timer_t) :: timer

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

    ! initialize microphysics before starting the timer
    call microphysics(domain, dt = 20.0)
    if (this_image()==1) print*, ""
    if (this_image()==1) print*, "Beginning simulation..."
    sync all
    call timer%start()
    do i=1,200
        ! print *,"Microphysics"
        ! note should this be wrapped into the domain object(?)
        call microphysics(domain, dt = 20.0, halo=1)
        call domain%halo_send()
        call microphysics(domain, dt = 20.0, subset=1)

        call domain%halo_retrieve()

        call domain%advect(dt = 1.0)

        ! if (this_image()==(num_images()/2)) then
        !     print*, domain%accumulated_precipitation(::3,ypos)
        ! endif

    end do
    sync all
    call timer%stop()

    if (this_image()==1) then
        print *,"Model run time:",timer%as_string('(f8.3," seconds")')
    endif
  end block

 if (this_image()==1) print *,"Test passed."

end program
