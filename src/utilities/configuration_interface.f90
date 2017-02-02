module configuration_interface
  implicit none

  private
  public :: configuration_t

  type configuration_t
  contains
    generic :: initialize => default_initialize
    procedure, private :: default_initialize
  end type

  interface
    module subroutine default_initialize(this)
      class(configuration_t), intent(inout) :: this
    end subroutine
  end interface

end module
