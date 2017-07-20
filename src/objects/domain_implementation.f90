submodule(domain_interface) domain_implementation
  use assertions_interface, only : assert,assertions
  use iso_fortran_env, only : error_unit
  implicit none

contains

    subroutine print_in_image_order(input)
        implicit none
        real :: input(:,:)
        integer :: i, j
        integer :: nx, xstep
        integer :: ny, ystep

        nx = size(input,1)
        ny = size(input,2)
        ystep = ny/8
        xstep = nx/8
        do i=1,num_images()
            if (this_image()==i) then
                write(*,*) this_image()
                do j=lbound(input,2),ubound(input,2),ystep
                    write(*,*) input(::xstep,j)
                enddo
            endif
            sync all
        end do

    end subroutine print_in_image_order

    subroutine master_initialize(this)
      class(domain_t), intent(inout) :: this
      integer :: i,j
      real :: sine_curve

      associate(                                            &
        u_test_val=>0.0, v_test_val=>0.5, w_test_val=>0.0,  &
        water_vapor_test_val            => 0.001,           &
        potential_temperature_test_val  => 300.0,           &
        cloud_water_mass_test_val       => 0.0,             &
        cloud_ice_mass_test_val         => 0.0,             &
        cloud_ice_number_test_val       => 0.0,             &
        rain_mass_test_val              => 0.0,             &
        rain_number_test_val            => 0.0,             &
        snow_mass_test_val              => 0.0,             &
        graupel_mass_test_val           => 0.0,             &
        nx=>this%nx, ny=>this%ny, nz=>this%nz )

        call this%u%initialize(this%get_grid_dimensions(nx_extra = 1), u_test_val)
        call this%v%initialize(this%get_grid_dimensions(ny_extra = 1), v_test_val)
        call this%w%initialize(this%get_grid_dimensions(), w_test_val)
        if (this_image()==1) print *,"call this%variable%initialize(this%get_grid_dimensions(),variable_test_val)"
        call this%water_vapor%initialize(           this%get_grid_dimensions(),water_vapor_test_val)
        call this%potential_temperature%initialize( this%get_grid_dimensions(),potential_temperature_test_val)
        call this%cloud_water_mass%initialize(      this%get_grid_dimensions(),cloud_water_mass_test_val)
        call this%cloud_ice_mass%initialize(        this%get_grid_dimensions(),cloud_ice_mass_test_val)
        call this%cloud_ice_number%initialize(      this%get_grid_dimensions(),cloud_ice_number_test_val)
        call this%rain_mass%initialize(             this%get_grid_dimensions(),rain_mass_test_val)
        call this%rain_number%initialize(           this%get_grid_dimensions(),rain_number_test_val)
        call this%snow_mass%initialize(             this%get_grid_dimensions(),snow_mass_test_val)
        call this%graupel_mass%initialize(          this%get_grid_dimensions(),graupel_mass_test_val)

        this%potential_temperature%local(1,:,:)=this%potential_temperature%local(1,:,:)-10
        ! Note, this can be used to create a change in water vapor at the upwind boundary so that it
        ! can be advected across the domain and permitted to interact with other species
        ! if (this_image()==1) then
        !     this%water_vapor%local(:,1,1) = water_vapor_test_val * 2
        ! endif
      end associate

      ! this permits arrays to have starting indices that are not 1
      this%ims = lbound(this%water_vapor%local,1)
      this%kms = lbound(this%water_vapor%local,2)
      this%jms = lbound(this%water_vapor%local,3)
      this%ime = ubound(this%water_vapor%local,1)
      this%kme = ubound(this%water_vapor%local,2)
      this%jme = ubound(this%water_vapor%local,3)

      ! initially set the tile to process to be set in one from the edges of memory
      if (assertions) call assert((this%ime - this%ims+1) >= 2, "x dimension has too few elements")
      if (assertions) call assert((this%jme - this%jms+1) >= 2, "y dimension has too few elements")
      if (assertions) call assert((this%kme - this%kms+1) >= 2, "z dimension has too few elements")
      this%its = this%ims + 1
      this%jts = this%jms + 1
      this%kts = this%kms
      this%ite = this%ime
      this%jte = this%jme - 1
      this%kte = this%kme - 1

      ! The entire model domain begins at 1 and ends at nx,y,z
      this%ids = 1
      this%jds = 1
      this%kds = 1
      this%ide = this%nx
      this%jde = this%ny_global
      this%kde = this%nz

      allocate(this%transfer_array_2d(this%nx, this%ny_global)[*])
      allocate(this%transfer_array_3d(this%nx, this%nz, this%ny_global)[*])

      associate(                                    &
          surface_z            => 0.0,              &   ! elevation of the first model level [m]
          dz_value             => 500.0,            &   ! thickness of each model gridcell   [m]
          sealevel_pressure    => 100000.0,         &   ! pressure at sea level              [Pa]
          hill_height          => 1000.0,           &
          ids=>this%ids, ide=>this%ide,             &
          jds=>this%jds, jde=>this%jde,             &
          kds=>this%kds, kde=>this%kde,             &
          ims=>this%ims, ime=>this%ime,             &
          jms=>this%jms, jme=>this%jme,             &
          kms=>this%kms, kme=>this%kme              &
          )

          allocate(this%accumulated_precipitation(ims:ime, jms:jme), source=0.)
          allocate(this%accumulated_snowfall     (ims:ime, jms:jme), source=0.)
          allocate(this%pressure                 (ims:ime, kms:kme, jms:jme))
          allocate(this%temperature              (ims:ime, kms:kme, jms:jme))
          allocate(this%exner                    (ims:ime, kms:kme, jms:jme))
          allocate(this%z                        (ims:ime, kms:kme, jms:jme))
          allocate(this%dz_interface             (ims:ime, kms:kme, jms:jme), source=dz_value)
          allocate(this%z_interface              (ims:ime, kms:kme, jms:jme))
          allocate(this%dz_mass                  (ims:ime, kms:kme, jms:jme), source=dz_value)

          ! this is a simple sine function for a hill... not the best test case but it's easy
          do j=jms,jme
              do i=ims,ime
                  sine_curve = (sin((i-ids)/real(ide-ids) * 2*3.14159 - 3.14159/2) + 1) / 2  &
                              *(sin((j-jds)/real(jde-jds) * 2*3.14159 - 3.14159/2) + 1) / 2

                  this%z_interface(i,kms,j) = surface_z + sine_curve * hill_height
              enddo
          enddo

          this%z(:,kms,:) = this%z_interface(:,kms,:) + dz_value/2

          this%dz_mass(:,kms,:)     = this%dz_mass(:,kms,:)/2
          this%pressure(:,kms,:)    = pressure_at_elevation(sealevel_pressure, this%z(:,kms,:))
          do i=kms+1,kme
              this%z(:,i,:)           = this%z(:,i-1,:)           + this%dz_mass(:,i,:)
              this%z_interface(:,i,:) = this%z_interface(:,i-1,:) + this%dz_interface(:,i,:)
              this%pressure(:,i,:)    = pressure_at_elevation(sealevel_pressure, this%z(:,i,:))
          enddo
          this%exner       = exner_function(this%pressure)
          this%temperature = this%exner * this%potential_temperature%local
          this%water_vapor%local = sat_mr(this%temperature,this%pressure)
      end associate

    end subroutine


    !>----------------------------------------------------------
    !!  Calculate the saturated mixing ratio for a given temperature and pressure
    !!
    !!  If temperature > 0C: returns the saturated mixing ratio with respect to liquid
    !!  If temperature < 0C: returns the saturated mixing ratio with respect to ice
    !!
    !!  @param temperature  Air Temperature [K]
    !!  @param pressure     Air Pressure [Pa]
    !!  @retval sat_mr      Saturated water vapor mixing ratio [kg/kg]
    !!
    !!  @see http://www.dtic.mil/dtic/tr/fulltext/u2/778316.pdf
    !!   Lowe, P.R. and J.M. Ficke., 1974: The Computation of Saturation Vapor Pressure
    !!   Environmental Prediction Research Facility, Technical Paper No. 4-74
    !!
    !!----------------------------------------------------------
    elemental function sat_mr(temperature,pressure)
    ! Calculate the saturated mixing ratio at a temperature (K), pressure (Pa)
        implicit none
        real,intent(in) :: temperature,pressure
        real :: e_s,a,b
        real :: sat_mr

        ! from http://www.dtic.mil/dtic/tr/fulltext/u2/778316.pdf
        !   Lowe, P.R. and J.M. Ficke., 1974: THE COMPUTATION OF SATURATION VAPOR PRESSURE
        !       Environmental Prediction Research Facility, Technical Paper No. 4-74
        ! which references:
        !   Murray, F. W., 1967: On the computation of saturation vapor pressure.
        !       Journal of Applied Meteorology, Vol. 6, pp. 203-204.
        ! Also notes a 6th order polynomial and look up table as viable options.
        if (temperature < 273.15) then
            a = 21.8745584
            b = 7.66
        else
            a = 17.2693882
            b = 35.86
        endif

        e_s = 610.78 * exp(a * (temperature - 273.16) / (temperature - b)) !(Pa)

        ! alternate formulations
        ! Polynomial:
        ! e_s = ao + t*(a1+t*(a2+t*(a3+t*(a4+t*(a5+a6*t))))) a0-6 defined separately for water and ice
        ! e_s = 611.2*exp(17.67*(t-273.15)/(t-29.65)) ! (Pa)
        ! from : http://www.srh.noaa.gov/images/epz/wxcalc/vaporPressure.pdf
        ! e_s = 611.0*10.0**(7.5*(t-273.15)/(t-35.45))


        if ((pressure - e_s) <= 0) then
            e_s = pressure * 0.99999
        endif
        ! from : http://www.srh.noaa.gov/images/epz/wxcalc/mixingRatio.pdf
        sat_mr = 0.6219907 * e_s / (pressure - e_s) !(kg/kg)
    end function sat_mr

    !> -------------------------------
    !!
    !! Convert p [Pa] at shifting it to a given elevatiom [m]
    !!
    !! -------------------------------
    elemental function pressure_at_elevation(sealevel_pressure, elevation) result(pressure)
        implicit none
        real, intent(in) :: sealevel_pressure, elevation
        real :: pressure

        pressure = sealevel_pressure * (1 - 2.25577E-5 * elevation)**5.25588

    end function

    !> -------------------------------
    !!
    !! Compute exner function to convert potential_temperature to temperature
    !!
    !! -------------------------------
    elemental function exner_function(pressure) result(exner)
        implicit none
        real, intent(in) :: pressure
        real :: exner

        associate(po=>100000, Rd=>287.058, cp=>1003.5)
            exner = (pressure / po) ** (Rd/cp)

        end associate
    end function

    !> -------------------------------
    !!
    !! Initialize the domain reading grid dimensions from an input file
    !!
    !! -------------------------------
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
      this%ny_global = ny
      this%ny = my_ny(ny)
      this%nz = nz
      if (this_image()==1) print *,"call master_initialize(this)"
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

    function my_jstart(ny_global) result(jms)
        implicit none
        integer, intent(in) :: ny_global
        integer :: jms
        integer :: base_ny

        associate(me=>this_image(),ni=>num_images())
            base_ny = ny_global/ni

            jms = (me-1)*(base_ny) + min(me-1,mod(ny_global,ni)) + 1
            ! if (me<=mod(ny_global,ni)) then
            !     jms = (me-1)*(base_ny+1) + 1
            ! else
            !     jms = (me-1)*(base_ny) + mod(ny_global,ni) + 1
            ! endif
        end associate

    end function my_jstart

    module function get_grid_dimensions(this, nx_extra, ny_extra) result(n)
      class(domain_t), intent(in) :: this
      integer,         intent(in), optional :: nx_extra, ny_extra
      integer :: n(space_dimension+1)

      integer :: nx_e, ny_e

      nx_e = 0
      ny_e = 0
      if (present(nx_extra)) nx_e = nx_extra
      if (present(ny_extra)) ny_e = ny_extra

      n = [this%nx + nx_e, this%nz, this%ny + ny_e, my_jstart(this%ny_global + ny_e)]

    end function

    module subroutine enforce_limits(this)
      class(domain_t), intent(inout) :: this
      where(this%water_vapor%local < 0)             this%water_vapor%local = 0
      where(this%potential_temperature%local < 0)   this%potential_temperature%local = 0
      where(this%cloud_water_mass%local < 0)        this%cloud_water_mass%local = 0
      where(this%cloud_ice_mass%local < 0)          this%cloud_ice_mass%local = 0
      where(this%cloud_ice_number%local < 0)        this%cloud_ice_number%local = 0
      where(this%rain_mass%local < 0)               this%rain_mass%local = 0
      where(this%rain_number%local < 0)             this%rain_number%local = 0
      where(this%snow_mass%local < 0)               this%snow_mass%local = 0
      where(this%graupel_mass%local < 0)            this%graupel_mass%local = 0

    end subroutine

    module subroutine halo_exchange(this)
      class(domain_t), intent(inout) :: this
      call this%water_vapor%send()
      call this%potential_temperature%send()
      call this%cloud_water_mass%send()
      call this%cloud_ice_mass%send()
      call this%cloud_ice_number%send()
      call this%rain_mass%send()
      call this%rain_number%send()
      call this%snow_mass%send()
      call this%graupel_mass%send()

      call this%water_vapor%retrieve()
      call this%potential_temperature%retrieve(no_sync=.True.)
      call this%cloud_water_mass%retrieve(no_sync=.True.)
      call this%cloud_ice_mass%retrieve(no_sync=.True.)
      call this%cloud_ice_number%retrieve(no_sync=.True.)
      call this%rain_mass%retrieve(no_sync=.True.)
      call this%rain_number%retrieve(no_sync=.True.)
      call this%snow_mass%retrieve(no_sync=.True.)
      call this%graupel_mass%retrieve(no_sync=.True.)
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

            allocate(uflux(nx, nz, ny), source=0.)
            allocate(vflux(nx, nz, ny), source=0.)
            allocate(wflux(nx, nz, ny), source=0.)

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
