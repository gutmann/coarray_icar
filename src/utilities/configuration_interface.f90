module configuration_interface
  !! Encapsulate problem set-up type and type-bound procedures
  implicit none

  private
  public :: configuration_t

  type configuration_t
    !! Store problem definition
  contains
    generic :: initialize => default_initialize
    procedure, private :: default_initialize
  end type

  interface
    module subroutine default_initialize(this)
      !! Set default values for all problem set-up data
      class(configuration_t), intent(inout) :: this
    end subroutine
  end interface

end module
