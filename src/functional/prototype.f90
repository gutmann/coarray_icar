module scalar_module
  implicit none
  type scalar
    real, allocatable :: values(:,:,:)[:]
  end type

  type local_scalar
    real, allocatable :: values(:,:,:)
  end type

end module

module vector_module
  implicit none

  type vector
    real, allocatable :: values(:,:,:,:)[:]
  contains
    procedure :: times_scalar
    procedure :: divergence
    generic :: operator(.x.)=>times_scalar
    generic :: operator(.div.)=>divergence
  end type

  type local_vector
    real, allocatable :: values(:,:,:,:)[:]
  end type

  interface

    pure module function times_scalar(lhs,rhs) result(scalar_product)
      class(vector), intent(in) :: lhs
      class(scalar), intent(in) :: rhs
      type(local_vector) :: scalar_product
    end function

    pure module function divergence(operand) result(div_operand)
      class(vector), intent(in) :: operand
      type(local_scalar) :: div_operand
    end function

  end interface


end module

program main
  !! Implement the MPDATA algorithm using pure operators to express
  !! the advection term
  implicit none
  use scalar_module, only : scalar
  use vector_module, only : vector

  type(scalar) :: Qv, d_dt_Qv ! specific_humidity
  type(vector) :: v, v_Qv  ! wind velocity


  v_Qv = v .x. Qv
   !! The .x. operator result is of type(local_vector)

  d_dt_Qv  =  - .div. (v_Qv)

end program
