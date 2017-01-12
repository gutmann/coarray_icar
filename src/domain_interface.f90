module domain_interface
  use configuration_interface, only : configuration_t
  use exchangeable_interface, only : exchangeable_t
  implicit none

  private
  public :: domain_t

  type domain_t
    private
    ! water vapor field to be advected
    type(exchangeable_t) :: water_vapor
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
    procedure :: initialize_from_file
    procedure :: advect
    procedure :: halo_exchange
   !generic :: read(formatted)=>initialize_from_file
   !procedure, private :: initialize_with_configuration
   !procedure :: update_boundary
  end type

  integer, parameter :: space_dimension=3

  interface

    ! Set default component values
    module subroutine default_initialize(this)
      class(domain_t), intent(inout) :: this
    end subroutine

    ! MPDATA algorithm
    module subroutine advect(this, dt)
      class(domain_t), intent(inout) :: this
      real,            intent(in)    :: dt
    end subroutine

    ! Exchange subdomain boundary information 
    module subroutine halo_exchange(this)
      class(domain_t), intent(inout) :: this
    end subroutine

    ! Return x, y, z dimennsions of grid
    module function get_grid_dimensions(this) result(n)
      class(domain_t), intent(in) :: this
      integer :: n(space_dimension)
    end function

    ! Input domain_t object from file
    module subroutine initialize_from_file(this)
      class(domain_t), intent(inout) :: this
    end subroutine

  end interface

end module
