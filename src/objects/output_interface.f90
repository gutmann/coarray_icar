module output_interface
  use constants
  use netcdf

  use variable_interface, only : variable_t

  implicit none

  private
  public :: output_t

  type variable_ptr
      type(variable_t), pointer :: ptr
  end type variable_ptr

  type output_t
      private
      logical :: is_initialized
      integer :: n_variables
      type(variable_ptr), allocatable :: variables(:)

      character(len=kMAX_FILE_LENGTH) :: filename
      integer :: ncfile_id

      integer :: n_dims
      integer :: dim_ids(kMAX_DIMENSIONS)
      character(len=kMAX_DIM_LENGTH) :: dimensions(kMAX_DIMENSIONS)

  contains

      procedure, public  :: add_to_output
      procedure, public  :: write

      procedure, private :: init
      procedure, private :: increase_holding_capacity
  end type

  interface

      module subroutine init(this)
          implicit none
          class(output_t),   intent(inout)  :: this
      end subroutine

      module subroutine increase_holding_capacity(this)
          implicit none
          class(output_t),   intent(inout)  :: this
      end subroutine

      module subroutine add_to_output(this, variable)
          implicit none
          class(output_t),   intent(inout)  :: this
          class(variable_t), intent(in)     :: variable
      end subroutine

      module subroutine write(this, filename)
          implicit none
          class(output_t),   intent(in)     :: this
          character(len=kMAX_FILE_LENGTH), intent(in) :: filename
      end subroutine

  end interface
end module
