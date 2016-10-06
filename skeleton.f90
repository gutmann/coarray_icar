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
    generic :: initialize => default_initialize!, initialize_with_configuration
    procedure, private :: default_initialize
   !procedure, private :: initialize_with_configuration
   !procedure :: update_boundary
   !procedure :: halo_exchange
   procedure :: advect
  end type

  interface
    module subroutine default_initialize(this)
      class(domain_t), intent(inout) :: this
    end subroutine
    module subroutine advect(this, dt)
      class(domain_t), intent(inout) :: this
      real,            intent(in)    :: dt
    end subroutine
  end interface

end module

submodule(domain_interface) domain_implementation
contains
    module subroutine default_initialize(this)
      class(domain_t), intent(inout) :: this
      integer, parameter :: nx=200,ny=200,nz=20

      ! note this allocation needs to use nx,ny,nz specific to the image size, not the full domain?
      allocate(this%water_vapor(nx,nz,ny),source=0.1)
      ! allocate wind field
      allocate(this%u(nx+1, nz, ny),  source=0.1)
      allocate(this%v(nx,   nz, ny+1),source=0.2)
      allocate(this%w(nx,   nz, ny),  source=0.0)
      
      ! while these can be retrieved from  size(water_vapor) shortcuts can help
      this%nx = nx
      this%ny = ny
      this%nz = nz
      
    end subroutine
    
    module subroutine advect(this, dt)
        class(domain_t), intent(inout) :: this
        real,            intent(in)    :: dt
        real, allocatable :: uflux(:,:,:), vflux(:,:,:), wflux(:,:,:)

        allocate(uflux(nx, nz, ny), source=0.0)
        allocate(vflux(nx, nz, ny), source=0.0)
        allocate(wflux(nx, nz, ny), source=0.0)

        uflux = this%u(1:this%nx,:,:) * dt * this%water_vapor
        vflux = this%v(:,:,1:this%ny) * dt * this%water_vapor
        wflux = this%w(:,:,:)         * dt * this%water_vapor
        
    end subroutine
end submodule

program main
  use domain_interface, only : domain_t
  implicit none
  type(domain_t) :: domain[*]
  
  call domain%initialize()
  
  call domain%advect(dt = 4.0)
  
end program
