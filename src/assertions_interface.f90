module assertions_interface
  implicit none
  private
  public :: assert
  interface
    elemental impure module subroutine assert(assertion,assertion_name)
      implicit none
      logical, intent(in) :: assertion
      character(len=*), intent(in) :: assertion_name
    end subroutine
  end interface
end module
