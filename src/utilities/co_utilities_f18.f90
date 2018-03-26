! Stubs for utility functions which are already standardized in Fortran 2018.
! Few compilers support these currently, ! so we only compile them if F18=TRUE is
! specified in the makefile.

module co_util

    implicit none

contains

    subroutine co_bcast(coarray, source_image, first_image, last_image)
        implicit none
        real(kind=8), intent(inout) :: coarray(:,:,:,:)[*]
        integer, intent(in) :: source_image, first_image, last_image
        integer :: dest_image

        ! Broadcast intrinsic defined in Fortran 2018. Currently supported only by
        ! Cray. We don't need the first_image/last_image args, but keep the function
        ! prototype the same so we can switch between the custom version and the
        ! standard version easily.

        call co_broadcast(coarray, source_image)

    end subroutine co_bcast

end module co_util
