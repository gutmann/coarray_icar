module configuration_interface
  implicit none

  private
  public :: configuration_t

  type configuration_t
  contains
    generic :: initialize => default_initialize
    procedure, private :: default_initialize
  end type

  interface
    module subroutine default_initialize(this)
      class(configuration_t), intent(inout) :: this
    end subroutine
  end interface

end module

submodule(configuration_interface) configuration_implementation
contains
    module subroutine default_initialize(this)
      class(configuration_t), intent(inout) :: this
    end subroutine
end submodule


module domain_interface
  use configuration_interface, only : configuration_t
  implicit none

  private
  public :: domain_t
  
  type domain_t
    private
    ! water vapor field to be advected
    real, allocatable :: water_vapor(:,:,:)
    ! wind field to control advection
    real, allocatable :: u(:,:,:)
    real, allocatable :: v(:,:,:)
    real, allocatable :: w(:,:,:)
    
    ! contains the size of the domain (or the local tile?)
    integer :: nx, ny, nz
    
    type(configuration_t) :: domain_configuration
  contains
    procedure :: default_initialize
    procedure :: get_grid_dimensions
   !procedure, private :: initialize_with_configuration
   !procedure :: update_boundary
   !procedure :: halo_exchange
    procedure :: initialize_from_file
   !generic :: read(formatted)=>initialize_from_file
    procedure :: advect
  end type

  integer, parameter :: space_dimension=3

  interface

    ! Set default component values
    module subroutine default_initialize(this)
      class(domain_t), intent(out) :: this
    end subroutine

    ! MPDATA algorithm
    module subroutine advect(this, dt)
      class(domain_t), intent(inout) :: this
      real,            intent(in)    :: dt
    end subroutine

    ! Return x, y, z dimennsions of grid
    module function get_grid_dimensions(this) result(n)
      class(domain_t), intent(in) :: this
      integer :: n(space_dimension)
    end function

    ! Input domain_t object from file
    module subroutine initialize_from_file(this)
      class(domain_t), intent(out) :: this
    end subroutine

  end interface

end module

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

program main
  use iso_fortran_env, only : input_unit
  use domain_interface, only : domain_t
  use assertions_interface, only : assert
  implicit none
  type(domain_t) :: domain[*]

  block 
    print *,"domain%default_initialize()"
    call domain%default_initialize()
    call assert(domain%get_grid_dimensions()==[200,200,20],"default grid dimensions")
  end block
  
  print *,"domain%initialize()"
  call domain%initialize_from_file()
  
  print *,"domain%advect(dt = 4.0)"
  call domain%advect(dt = 4.0)
  
end program
