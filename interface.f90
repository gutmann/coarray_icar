module foo_module
  implicit none
  interface
    module function f(x) result(y)
      real :: x,y
    end function
  end interface
!end module

!submodule(foo_module) foo_implementation
contains
  module function f(x) result(y)
    real :: x,y
    y = x*x
  end function
!end submodule
end module
