module exchangeable_interface
  implicit none

  private
  public :: exchangeable_t

  type exchangeable_t
    private
    real, allocatable, public :: local(:,:,:)     
    real, allocatable :: halo_south_out(:,:,:)[*],halo_south_in(:,:,:)[*]
    real, allocatable :: halo_north_out(:,:,:)[*],halo_north_in(:,:,:)[*]
    logical :: north_boundary=.false.
    logical :: south_boundary=.false.
  contains
    procedure :: const
    procedure :: exchange
    generic :: initialize=>const
  end type

  integer, parameter :: space_dim=3

  interface

    module subroutine const(this,grid_dims,initial_value)
      implicit none
      class(exchangeable_t), intent(out) :: this
      integer , intent(in) :: grid_dims(:)
      real, intent(in) :: initial_value
    end subroutine

    module subroutine exchange(this)
      implicit none
      class(exchangeable_t), intent(inout) :: this
    end subroutine

  end interface

end module
