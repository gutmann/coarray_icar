module assertions_interface
  !! summary: Utility for runtime checking of logical assertions.
  !!
  !! Compile with -DNO_ASSERTIONS to turn assertions off
  !!
  !! Use case 1 
  !! ----------
  !!    Pass the optional success argument & check for false return value as an indication of assertion failure:
  !!
  !!    use assertions_interface, only : assert,assertions
  !!    if (assertions) call assert( 2 > 1, "always true inequality", success)
  !!    if (error_code/=0) call my_error_handler()
  !!
  !! Use case 2
  !! ----------
  !!    Error-terminate if the assertion fails:
  !!
  !!    use assertions_interface, only : assert,assertions
  !!    if (assertions) call assert( 2 > 1, "always true inequality")
  !!
  implicit none
  private
  public :: assert
  public :: assertions
  
! Conditioning assertion calls on this compile-time constant enables optimizing compilers
! to eliminate assertion calls during a dead-code removal phase of optimization
#ifdef NO_ASSERTIONS
  logical, parameter :: assertions=.false.
#else
  logical, parameter :: assertions=.true.
#endif

  interface
    elemental impure module subroutine assert(assertion,description,success)
      !! Report on the truth of an assertion or error-terminate on assertion failure
      implicit none
      logical, intent(in) :: assertion
        !! Most assertions will be expressions, e.g., call assert( i>0, "positive i")
      character(len=*), intent(in) :: description
        !! Brief statement of what is being asserted
      logical, intent(out), optional :: success
        !! Optional assertion result 
    end subroutine
  end interface
end module
