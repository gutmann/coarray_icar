module variable_interface
    use constants

    implicit none

    ! defines a variable type that can store data and attributes
    type variable_t
        real, allocatable :: local(:,:,:)

        character(len=kMAX_NAME_LENGTH) :: name
        character(len=kMAX_DIM_LENGTH),  allocatable :: dimensions(:)
        character(len=kMAX_ATTR_LENGTH), allocatable :: attribute_names(:)
        character(len=kMAX_ATTR_LENGTH), allocatable :: attribute_values(:)

        ! note these are used for netcdf output
        integer, allocatable :: dim_ids(:)
        integer :: var_id = -99

        ! Note this dummy coarray is only here because types that inherit from variable_t have coarrays
        ! so the parent type is required to...
        integer, allocatable :: dummy[:]


    end type variable_t

    !  complains that new_variable procedure doesn't exist
    ! interface variable_t
    !     module procedure new_variable
    ! end interface
    !
    ! interface
    !     module function new_variable(name)
    !         implicit none
    !         character(len=kMAX_NAME_LENGTH), intent(in) :: name
    !         type(variable_t) :: new_variable
    !     end function
    ! end interface
    !
    ! complains that it can't have a coarray in the output
    ! interface variable_t
    !     module function new_variable(name)
    !         implicit none
    !         character(len=kMAX_NAME_LENGTH), intent(in) :: name
    !         type(variable_t) :: new_variable
    !     end function
    ! end interface

end module
