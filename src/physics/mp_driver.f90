module module_mp_driver

    use domain_interface
    use module_mp_thompson

    logical :: initialized
contains

    subroutine mp_init(domain)
        implicit none
        type(domain_t), intent(inout) :: domain

        call thompson_init()

    end subroutine mp_init

    subroutine microphysics(domain)
        implicit none
        type(domain_t), intent(inout) :: domain

        if (.not.initialized) call mp_init(domain)

    end subroutine microphysics

end module module_mp_driver
