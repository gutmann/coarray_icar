submodule(assertions_interface) assertions_implementation
  implicit none
contains
  elemental impure module subroutine assert(assertion,description,success)
    use iso_fortran_env, only : error_unit
    logical, intent(in) :: assertion
    character(len=*), intent(in) :: description
    logical, intent(out), optional :: success
    if (present(success)) success=assertion
    if (.not.assertion) then
      write(error_unit,*) 'Assertion "',description,'" failed on image ',this_image()
      if (.not. present(success)) error stop  
    end if
  end subroutine
end submodule
