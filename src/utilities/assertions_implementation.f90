submodule(assertions_interface) assertions_implementation
  implicit none
contains
  elemental impure module subroutine assert(assertion,assertion_name)
    use iso_fortran_env, only : error_unit
    logical, intent(in) :: assertion
    character(len=*), intent(in) :: assertion_name
    if (.not.assertion) then
      write(error_unit,*) 'Assertion "',assertion_name,'" failed on image ',this_image()
      error stop 
    end if
  end subroutine
end submodule
