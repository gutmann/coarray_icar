module domain_interface
  use configuration_interface, only : configuration_t
  use exchangeable_interface, only : exchangeable_t
  implicit none

  private
  public :: domain_t

  type domain_t
    private
    ! core model species to be advected
    type(exchangeable_t) :: water_vapor
    type(exchangeable_t) :: potential_temperature
    type(exchangeable_t) :: cloud_water_mass
    type(exchangeable_t) :: cloud_ice_mass
    type(exchangeable_t) :: cloud_ice_number
    type(exchangeable_t) :: rain_mass
    type(exchangeable_t) :: rain_number
    type(exchangeable_t) :: snow_mass
    type(exchangeable_t) :: graupel_mass

    ! core model variables (not advected)
    real, allocatable :: exner(:,:,:)
    real, allocatable :: pressure(:,:,:)
    real, allocatable :: temperature(:,:,:)
    real, allocatable :: z(:,:,:)
    real, allocatable :: dz_interface(:,:,:)
    real, allocatable :: z_interface(:,:,:)
    real, allocatable :: dz_mass(:,:,:)

    ! wind field to control advection
    type(exchangeable_t) :: u
    type(exchangeable_t) :: v
    type(exchangeable_t) :: w

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
      implicit none
      class(domain_t), intent(inout) :: this
    end subroutine

    ! MPDATA algorithm
    module subroutine advect(this, dt)
      implicit none
      class(domain_t), intent(inout) :: this
      real,            intent(in)    :: dt
    end subroutine

    ! Exchange subdomain boundary information
    module subroutine halo_exchange(this)
      implicit none
      class(domain_t), intent(inout) :: this
    end subroutine

    ! Return x, y, z dimennsions of grid
    module function get_grid_dimensions(this, nx_extra, ny_extra) result(n)
      implicit none
      class(domain_t), intent(in) :: this
      integer,         intent(in), optional :: nx_extra, ny_extra
      integer :: n(space_dimension)
    end function

    ! Input domain_t object from file
    module subroutine initialize_from_file(this,file_name)
      implicit none
      class(domain_t), intent(inout) :: this
      character(len=*), intent(in) :: file_name
    end subroutine

  end interface

end module
