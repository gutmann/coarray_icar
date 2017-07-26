module constants

    implicit none

    ! string lengths
    integer, parameter :: kMAX_FILE_LENGTH = 1024
    integer, parameter :: kMAX_DIM_LENGTH  = 1024
    integer, parameter :: kMAX_NAME_LENGTH = 1024
    integer, parameter :: kMAX_ATTR_LENGTH = 1024

    ! Initial number of output variables for which pointers are created
    integer, parameter :: kINITIAL_VAR_SIZE= 1024

    ! Maximum number of dimensions
    ! Note this is defined in NetCDF, though not enforced (a file can have more than 1024 dimensions)
    integer, parameter :: kMAX_DIMENSIONS  = 1024

end module constants
