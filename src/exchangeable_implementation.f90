submodule(exchangeable_interface) exchangeable_implementation
  implicit none

contains

  subroutine const(this,grid_dims,initial_value)
    class(exchangeable_t), intent(out) :: this
    integer, intent(in) :: grid_dims(:)
    real, intent(in) :: initial_value
    if (allocated(this%local)) deallocate(this%local)
    allocate(this%local(grid_dims(1),grid_dims(2),grid_dims(3)),source=initial_value)
    this%north_boundary = (this_image() == num_images())
    this%south_boundary = (this_image() == 1)
    associate( halo_size=>5  )
      allocate( this%halo_south_in( grid_dims(1),grid_dims(2),halo_size)[*], source=initial_value)
      allocate( this%halo_south_out(grid_dims(1),grid_dims(2),halo_size)[*], source=initial_value)
      allocate( this%halo_north_in( grid_dims(1),grid_dims(2),halo_size)[*], source=initial_value)
      allocate( this%halo_north_out(grid_dims(1),grid_dims(2),halo_size)[*], source=initial_value)
    end associate
  end subroutine

  subroutine exchange(this)
    class(exchangeable_t), intent(inout) :: this
    print *,"exchange unimplemented"
  end subroutine

end submodule
