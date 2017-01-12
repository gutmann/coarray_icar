submodule(exchangeable_interface) exchangeable_implementation
  implicit none

  integer, parameter :: default_halo_size=5

contains

  module subroutine const(this,grid_dims,initial_value,halo_width)
    class(exchangeable_t), intent(inout) :: this
    integer, intent(in) :: grid_dims(:)
    real, intent(in) :: initial_value
    integer, intent(in), optional :: halo_width
    integer :: halo_size
    if (present(halo_width)) then
        halo_size = halo_width
    else
        halo_size = default_halo_size 
    end if
    if (allocated(this%local)) deallocate(this%local)
    this%north_boundary = (this_image() == num_images())
    this%south_boundary = (this_image() == 1)
    associate( local_halo_size=>2*halo_size - merge(halo_size,0,this%south_boundary) - merge(halo_size,0,this%north_boundary) ) 
      allocate(this%local(grid_dims(1),grid_dims(2),grid_dims(3) + local_halo_size),source=initial_value)
    end associate
    allocate( this%halo_south_in( grid_dims(1),grid_dims(2),halo_size)[*], source=initial_value)
    allocate( this%halo_north_in( grid_dims(1),grid_dims(2),halo_size)[*], source=initial_value)
  end subroutine

  module subroutine exchange(this)
    class(exchangeable_t), intent(inout) :: this
    print *,"exchange unimplemented"
  end subroutine

end submodule
