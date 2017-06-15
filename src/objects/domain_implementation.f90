submodule(domain_interface) domain_implementation
  use assertions_interface, only : assert,assertions
  use iso_fortran_env, only : error_unit
  implicit none

contains

    subroutine master_initialize(this)
      class(domain_t), intent(inout) :: this
      integer :: i, local_nx, local_ny, local_nz

      associate(                                           &
        u_test_val=>0.1, v_test_val=>0.2, w_test_val=>0.0, &
        water_vapor_test_val=>0.001,                       &
        potential_temperature_test_val=>300.0,             &
        cloud_water_mass_test_val=>0.0,                    &
        cloud_ice_mass_test_val=>0.0,                      &
        cloud_ice_number_test_val=>0.0,                    &
        rain_mass_test_val=>0.0,                           &
        rain_number_test_val=>0.0,                         &
        snow_mass_test_val=>0.0,                           &
        graupel_mass_test_val=>0.0,                        &
        nx=>this%nx, ny=>this%ny, nz=>this%nz )

        call this%u%initialize(this%get_grid_dimensions(nx_extra = 1), u_test_val)
        call this%v%initialize(this%get_grid_dimensions(ny_extra = 1), v_test_val)
        call this%w%initialize(this%get_grid_dimensions(), w_test_val)
        print *,"call this%variable%initialize(this%get_grid_dimensions(),variable_test_val)"
        call this%water_vapor%initialize(           this%get_grid_dimensions(),water_vapor_test_val)
        call this%potential_temperature%initialize( this%get_grid_dimensions(),potential_temperature_test_val)
        call this%cloud_water_mass%initialize(      this%get_grid_dimensions(),cloud_water_mass_test_val)
        call this%cloud_ice_mass%initialize(        this%get_grid_dimensions(),cloud_ice_mass_test_val)
        call this%cloud_ice_number%initialize(      this%get_grid_dimensions(),cloud_ice_number_test_val)
        call this%rain_mass%initialize(             this%get_grid_dimensions(),rain_mass_test_val)
        call this%rain_number%initialize(           this%get_grid_dimensions(),rain_number_test_val)
        call this%snow_mass%initialize(             this%get_grid_dimensions(),snow_mass_test_val)
        call this%graupel_mass%initialize(          this%get_grid_dimensions(),graupel_mass_test_val)

        ! Note, this can be used to create a change in water vapor at the upwind boundary so that it
        ! can be advected across the domain and permitted to interact with other species
        ! if (this_image()==1) then
        !     this%water_vapor%local(:,1,1) = water_vapor_test_val * 2
        ! endif
      end associate

      local_ny = size(this%water_vapor%local, 1)   ! number of grid cells in y dimension (in local memory)
      local_nz = size(this%water_vapor%local, 2)   ! number of grid cells in z dimension (in local memory)
      local_nx = size(this%water_vapor%local, 3)   ! number of grid cells in x dimension (in local memory)

      associate(                                    &
          surface_z=>0.0,                           &   ! elevation of the first model level [m]
          dz_value=>500.0,                          &   ! thickness of each model gridcell   [m]
          surface_pressure=>100000.0)                   ! pressure at the first model level  [Pa]
        !   )

          allocate(this%pressure    (local_ny, local_nz, local_nx), source=surface_pressure)
          allocate(this%temperature (local_ny, local_nz, local_nx))
          allocate(this%exner       (local_ny, local_nz, local_nx))
          allocate(this%z           (local_ny, local_nz, local_nx), source=surface_z + dz_value/2)
          allocate(this%dz_interface(local_ny, local_nz, local_nx), source=dz_value)
          allocate(this%z_interface (local_ny, local_nz, local_nx), source=surface_z)
          allocate(this%dz_mass     (local_ny, local_nz, local_nx), source=dz_value)

          this%dz_mass(:,1,:) = this%dz_mass(:,1,:)/2
          this%exner(:,1,:) = exner_function(this%pressure(:,1,:))
          do i=2,local_nz
              this%z(:,i,:)           = this%z(:,i-1,:)           + this%dz_mass(:,i,:)
              this%z_interface(:,i,:) = this%z_interface(:,i-1,:) + this%dz_interface(:,i,:)

              this%pressure(:,i,:)    = pressure_at_elevation(this%pressure(:,1,:), this%z(:,i,:))
          enddo
          this%exner       = exner_function(this%pressure)
          this%temperature = this%exner * this%potential_temperature%local
      end associate

    end subroutine

    !> -------------------------------
    !!
    !! Convert p [Pa] at shifting it to a given elevatiom [m]
    !!
    !! -------------------------------
    elemental function pressure_at_elevation(surface_pressure, elevation) result(pressure)
        implicit none
        real, intent(in) :: surface_pressure, elevation
        real :: pressure

        pressure = surface_pressure * (1 - 2.25577E-5 * elevation)**5.25588

    end function

    elemental function exner_function(pressure) result(exner)
        implicit none
        real, intent(in) :: pressure
        real :: exner

        associate(po=>100000, Rd=>287.058, cp=>1003.5)
            exner = (pressure / po) ** (Rd/cp)

        end associate
    end function

    module subroutine initialize_from_file(this,file_name)
      class(domain_t), intent(inout) :: this
      character(len=*), intent(in) :: file_name
      integer :: nx,ny,nz
      integer :: my_unit,stat
      character(len=64) error_message
      namelist/grid/ nx,ny,nz

      open(file=file_name,newunit=my_unit,iostat=stat,status='old',action='read')
      write(error_message,*) "image ",this_image()," could not open file " // trim(file_name)
      if (assertions) call assert(stat==0,error_message)

      read(unit=my_unit,nml=grid,iostat=stat)
      write(error_message,*)"image ",this_image()," could not read file " // trim(file_name)
      if (assertions) call assert(stat==0,error_message)

      close(my_unit,iostat=stat)
      write(error_message,*)"image ",this_image()," could not close file " // trim(file_name)
      if (assertions) call assert(stat==0,error_message)

      this%nx = nx
      this%ny = my_ny(ny)
      this%nz = nz
      print *,"call master_initialize(this)"
      call master_initialize(this)
    end subroutine

    module subroutine default_initialize(this)
      class(domain_t), intent(inout) :: this
      integer, parameter :: nx_global=200,ny_global=200,nz_global=20

      this%nx = nx_global
      this%ny = my_ny(ny_global)
      this%nz = nz_global
      call master_initialize(this)
    end subroutine

    function my_ny(ny_global) result(ny_local)
       integer, intent(in) :: ny_global
       integer :: ny_local
       associate(me=>this_image(),ni=>num_images())
         ny_local = ny_global/ni + merge(1,0,me <= mod(ny_global,ni)  )
       end associate
    end function

    module function get_grid_dimensions(this, nx_extra, ny_extra) result(n)
      class(domain_t), intent(in) :: this
      integer,         intent(in), optional :: nx_extra, ny_extra
      integer :: n(space_dimension)

      integer :: nx_e, ny_e

      nx_e = 0
      ny_e = 0
      if (present(nx_extra)) nx_e = nx_extra
      if (present(ny_extra)) ny_e = ny_extra

      n = [this%nx + nx_e, this%nz, this%ny + ny_e]

    end function

    module subroutine halo_exchange(this)
      class(domain_t), intent(inout) :: this
      call this%water_vapor%exchange()
      call this%potential_temperature%exchange()
      call this%cloud_water_mass%exchange()
      call this%cloud_ice_mass%exchange()
      call this%cloud_ice_number%exchange()
      call this%rain_mass%exchange()
      call this%rain_number%exchange()
      call this%snow_mass%exchange()
      call this%graupel_mass%exchange()
    end subroutine

    subroutine upwind(q, u,v,w, dt)
        implicit none
        real, intent(inout), dimension(:,:,:) :: q
        real, intent(in),    dimension(:,:,:) :: u, v, w
        real, intent(in)                      :: dt

        real, allocatable :: uflux(:,:,:), vflux(:,:,:), wflux(:,:,:)

        associate(nx=>size(q,1), &
                  ny=>size(q,3), &
                  nz=>size(q,2))

            allocate(uflux(nx, nz, ny))
            allocate(vflux(nx, nz, ny))
            allocate(wflux(nx, nz, ny))

            uflux = u(2:nx+1,:, :    ) * dt * q
            vflux = v( :    ,:,2:ny+1) * dt * q
            wflux = w( :    ,:, :    ) * dt * q

            ! ultimately this will need to be more sophisticated, but for testing purposes this works
            ! q = q + (inflow - outflow)
            q(2:nx-1,:,2:ny-1) = q(2:nx-1,:,2:ny-1)                                 &
                               + (uflux(1:nx-2,:,2:ny-1) - uflux(2:nx-1,:,2:ny-1))  &
                               + (vflux(2:nx-1,:,1:ny-2) - vflux(2:nx-1,:,2:ny-1))
            ! vertical fluxes are handled separately
            q(2:nx-1,1:nz,2:ny-1) = q(2:nx-1,1:nz,2:ny-1) - wflux(2:nx-1,1:nz,2:ny-1)
            q(2:nx-1,2:nz,2:ny-1) = q(2:nx-1,2:nz,2:ny-1) + wflux(2:nx-1,1:nz-1,2:ny-1)

        end associate

    end subroutine upwind

    module subroutine advect(this, dt)
        class(domain_t), intent(inout) :: this
        real,            intent(in)    :: dt

        if (assertions) call assert(this%u%local >= 0, "Restrict wind u values for testing")
        if (assertions) call assert(this%v%local >= 0, "Restrict wind v values for testing")
        if (assertions) call assert(this%w%local == 0, "Restrict wind w values for testing")

        associate(u=>this%u%local, &
                  v=>this%v%local, &
                  w=>this%w%local)
            call upwind(this%water_vapor%local,             u, v, w, dt)
            call upwind(this%potential_temperature%local,   u, v, w, dt)
            call upwind(this%cloud_water_mass%local,        u, v, w, dt)
            call upwind(this%cloud_ice_mass%local,          u, v, w, dt)
            call upwind(this%cloud_ice_number%local,        u, v, w, dt)
            call upwind(this%rain_mass%local,               u, v, w, dt)
            call upwind(this%rain_number%local,             u, v, w, dt)
            call upwind(this%snow_mass%local,               u, v, w, dt)
            call upwind(this%graupel_mass%local,            u, v, w, dt)
        end associate

    end subroutine
end submodule
