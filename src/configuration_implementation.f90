submodule(configuration_interface) configuration_implementation
contains
    module subroutine default_initialize(this)
      class(configuration_t), intent(inout) :: this
    end subroutine
end submodule
