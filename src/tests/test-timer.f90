program test_timer
    use timer_interface, only : timer_t

    implicit none

    type(timer_t) :: timer
    real :: temp_time

    print*, "Testing timer class"
    print*, "For now just prints what the value of the results should be, will add tests later"
    call timer%start()

    call use_time(1)

    temp_time = timer%get_time()
    print*, "Should be 1 :", timer%as_string()
    print*, "Should be 1 :", temp_time

    call use_time(1)
    call timer%stop()
    print*, "Should be 2 :", timer%as_string()
    print*, "Should be 2 :", timer%get_time()

    call timer%start()
    call use_time(2)
    call timer%stop()
    print*, "Should be 4 :",timer%as_string("(f10.2)")
    print*, "Should be 4 :",timer%get_time()

    call timer%reset()
    print*, "Should be 0 :",timer%as_string("(f5.0)")
    call timer%start()
    call use_time(1)
    call timer%stop()
    print*, "Should be 1 :",timer%as_string("(f5.0)")

contains
    subroutine use_time(n)
        implicit none
        integer, intent(in) :: n
        real :: start_time, stop_time

        call cpu_time(start_time)
        call cpu_time(stop_time)

        do while ((stop_time-start_time)<n)
            call cpu_time(stop_time)
        end do

    end subroutine use_time

end program test_timer
