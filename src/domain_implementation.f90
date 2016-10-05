submodule(domain_interface) domain_implementation
  use assertions_interface, only : assert
  use iso_fortran_env, only : input_unit
  implicit none

contains

    module subroutine initialize_from_file(this)
      class(domain_t), intent(out) :: this
      integer :: nx,ny,nz
      namelist/grid/ nx,ny,nz
      read(input_unit,nml=grid)
      call assert(nx>3 .and. ny>3 .and. nz>3, "minimum grid dimensions" )
      this%nx = nx
      this%ny = ny
      this%nz = nz
      if ( allocated(this%u) ) deallocate(this%u)
      if ( allocated(this%v) ) deallocate(this%v)
      if ( allocated(this%w) ) deallocate(this%w)
      if ( allocated(this%water_vapor) ) deallocate(this%water_vapor)
      associate(u_test_val=>0.1,v_test_val=>0.2,w_test_val=>0.0,water_vapor_test_val=>0.1)
        allocate(this%u(nx+1, nz, ny),  source=u_test_val)
        allocate(this%v(nx,   nz, ny+1),source=v_test_val)
        allocate(this%w(nx,   nz, ny),  source=w_test_val)
        allocate(this%water_vapor(nx,nz,ny),source=water_vapor_test_val)
      end associate
    end subroutine

    module subroutine default_initialize(this)
      class(domain_t), intent(out) :: this
      integer, parameter :: nx=200,ny=200,nz=20
      this%nx = nx
      this%ny = ny
      this%nz = nz
      if ( allocated(this%u) ) deallocate(this%u)
      if ( allocated(this%v) ) deallocate(this%v)
      if ( allocated(this%w) ) deallocate(this%w)
      if ( allocated(this%water_vapor) ) deallocate(this%water_vapor)
      associate(u_test_val=>0.1,v_test_val=>0.2,w_test_val=>0.0,water_vapor_test_val=>0.1)
        allocate(this%u(nx+1, nz, ny),  source=u_test_val)
        allocate(this%v(nx,   nz, ny+1),source=v_test_val)
        allocate(this%w(nx,   nz, ny),  source=w_test_val)
        allocate(this%water_vapor(nx,nz,ny),source=water_vapor_test_val)
      end associate
    end subroutine

    module function get_grid_dimensions(this) result(n)
      class(domain_t), intent(in) :: this
      integer :: n(space_dimension)
      n = [this%nx,this%ny,this%nz]
    end function

    module subroutine advect(this, dt)
        class(domain_t), intent(inout) :: this
        real,            intent(in)    :: dt
        real, allocatable :: uflux(:,:,:), vflux(:,:,:), wflux(:,:,:)

        associate(nx=>this%nx,ny=>this%ny,nz=>this%nz)
          allocate(uflux(nx, nz, ny), source=0.0)
          allocate(vflux(nx, nz, ny), source=0.0)
          allocate(wflux(nx, nz, ny), source=0.0)

          uflux = this%u(1:nx,:,:) * dt * this%water_vapor
          vflux = this%v(:,:,1:ny) * dt * this%water_vapor
          wflux = this%w(:,:,:)    * dt * this%water_vapor
        end associate
        
    end subroutine
end submodule
