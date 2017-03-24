submodule(exchangeable_interface) exchangeable_implementation
  use assertions_interface, only : assert, assertions 
  implicit none

  integer, parameter :: default_halo_size=5
  integer, save, allocatable :: neighbors(:)
  integer, save :: north_neighbor, south_neighbor, halo_size

contains

  module subroutine const(this,grid_dims,initial_value,halo_width)
    class(exchangeable_t), intent(inout) :: this
    integer, intent(in) :: grid_dims(:)
    real, intent(in) :: initial_value
    integer, intent(in), optional :: halo_width

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

    associate(me=>this_image())
      south_neighbor = me-1
      north_neighbor = me+1
      if (this%north_boundary .and. this%south_boundary) then
        neighbors = [me]
      else if (this%south_boundary) then
        neighbors = [north_neighbor]
      else if (this%north_boundary) then
        neighbors = [south_neighbor]
      else ! internal cells
        neighbors = [me-1,me+1]
      end if
    end associate

  end subroutine

  module subroutine exchange(this)
    class(exchangeable_t), intent(inout) :: this
    if (.not. this%north_boundary) call put_north
    if (.not. this%south_boundary) call put_south
    sync images( neighbors )
    if (.not. this%north_boundary) call retrieve_north_halo
    if (.not. this%south_boundary) call retrieve_south_halo
  contains

    subroutine put_north
      integer :: n
      n = size(this%local,3)
      if (assertions) then
        !! gfortran 6.3.0 doesn't check coarray shape conformity with -fcheck=all so we verify with an assertion
        call assert( shape(this%halo_south_in(:,:,:)[north_neighbor]) == shape(this%local(:,:,n-halo_size+1:n)), &
                     "put_north: conformable halo_south_in and local " )
      end if
      this%halo_south_in(:,:,:)[north_neighbor] = this%local(:,:,n-halo_size+1:n)
    end subroutine

    subroutine put_south
      if (assertions) then
        !! gfortran 6.3.0 doesn't check coarray shape conformity with -fcheck=all so we verify with an assertion
        call assert( shape(this%halo_north_in(:,:,:)[south_neighbor]) == shape(this%local(:,:,1:halo_size)), &
                     "put_south: conformable halo_north_in and local " )
      end if
      this%halo_north_in(:,:,:)[south_neighbor] = this%local(:,:,1:halo_size)
    end subroutine

    subroutine retrieve_north_halo
      integer :: n
      n = size(this%local,3)
      this%local(:,:,n-halo_size+1:n) = this%halo_north_in
    end subroutine

    subroutine retrieve_south_halo
      this%local(:,:,1:halo_size) = this%halo_south_in
    end subroutine

  end subroutine

end submodule
