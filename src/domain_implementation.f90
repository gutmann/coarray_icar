submodule(domain_interface) domain_implementation
  use assertions_interface, only : assert
  use iso_fortran_env, only : input_unit
  implicit none

contains

    module subroutine master_initialize(this)
      class(domain_t), intent(inout) :: this
      if ( allocated(this%u) ) deallocate(this%u)
      if ( allocated(this%v) ) deallocate(this%v)
      if ( allocated(this%w) ) deallocate(this%w)
      associate(                                           &
        u_test_val=>0.1, v_test_val=>0.2, w_test_val=>0.0, &
        water_vapor_test_val=>0.1,                         &
        nx=>this%nx, ny=>this%ny, nz=>this%nz )
  
        allocate(this%u(nx+1, nz, ny),  source=u_test_val)
        allocate(this%v(nx,   nz, ny+1),source=v_test_val)
        allocate(this%w(nx,   nz, ny),  source=w_test_val)
        call this%water_vapor%initialize(this%get_grid_dimensions(),water_vapor_test_val)
      end associate
    end subroutine 

    module subroutine initialize_from_file(this)
      class(domain_t), intent(out) :: this
      integer :: nx,ny,nz
      namelist/grid/ nx,ny,nz
      read(input_unit,nml=grid)
      call assert(nx>3 .and. ny>3 .and. nz>3, "minimum grid dimensions" )
      this%nx = nx
      this%ny = my_ny(ny)
      this%nz = nz
      call master_initialize(this)
    end subroutine

    module subroutine default_initialize(this)
      class(domain_t), intent(out) :: this
      integer, parameter :: nx_global=200,ny_global=200,nz_global=20

      this%nx = nx_global
      this%ny = my_ny(ny_global)
      this%nz = nz_global
      call master_initialize(this)
    end subroutine

    module function my_ny(ny_global) result(ny_local)
       integer, intent(in) :: ny_global
       integer :: ny_local
       associate(me=>this_image(),ni=>num_images())
         ny_local = ny_global/ni + merge(1,0,me <= mod(ny_global,ni)  )    
       end associate
    end function

    module function get_grid_dimensions(this) result(n)
      class(domain_t), intent(in) :: this
      integer :: n(space_dimension)
      n = [this%nx,this%ny,this%nz]
    end function

    module subroutine halo_exchange(this)
      class(domain_t), intent(inout) :: this
      call this%water_vapor%exchange()
    end subroutine

    module subroutine advect(this, dt)
        class(domain_t), intent(inout) :: this
        real,            intent(in)    :: dt
        real, allocatable :: uflux(:,:,:), vflux(:,:,:), wflux(:,:,:)

        associate(nx=>this%nx,ny=>this%ny,nz=>this%nz)
          allocate(uflux(nx, nz, ny))
          allocate(vflux(nx, nz, ny))
        !   allocate(wflux(nx, nz, ny))

          call assert(this%u >= 0, "Restrict wind u values for testing")
          call assert(this%v >= 0, "Restrict wind v values for testing")
          call assert(this%w == 0, "Restrict wind w values for testing")

          uflux = this%u(2:nx+1,:, :    ) * dt * this%water_vapor%local
          vflux = this%v( :    ,:,2:ny+1) * dt * this%water_vapor%local

          !   wflux = this%w( :    ,:, :    ) * dt * this%water_vapor%local 
          ! since we assert w==0 this is irrelevant for now
          
          ! ultimately this will need to be more sophisticated, but for testing purposes this works
          ! q = q + (inflow - outflow)
          this%water_vapor%local(2:nx-1,:,2:ny-1) = this%water_vapor%local(2:nx-1,:,2:ny-1)         &
                                              + (uflux(1:nx-2,:,2:ny-1) - uflux(2:nx-1,:,2:ny-1))   &
                                              + (vflux(2:nx-1,:,1:ny-2) - vflux(2:nx-1,:,2:ny-1))
          
        end associate
        
    end subroutine
end submodule
