module mp_driver

    use module_mp_thompson

    logical :: initialized
contains

    subroutine microphysics(domain)
        implicit none
        type(domain_t), intent(inout) :: domain



    end subroutine microphysics
end module mp_driver
