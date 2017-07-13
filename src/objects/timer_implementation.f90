submodule(timer_interface) timer_implementation

    implicit none

contains

    !>-----------------------------------
    !! Start the timer
    !!
    !! Sets the internal start_time and marks the timer as running
    !!
    !------------------------------------
    subroutine start(this)
        class(timer_t), intent(inout) :: this

        this%is_running = .True.
        call cpu_time(this%start_time)
    end subroutine start

    !>-----------------------------------
    !! Stop the timer
    !!
    !! Sets the internal stop_time and marks the timer as not running
    !!
    !! Also computes the total_time the timer has been running
    !!
    !------------------------------------
    module subroutine stop(this)
        class(timer_t), intent(inout) :: this

        ! do this before we even test anything else because we want the timer to "stop" as soon as possible
        call cpu_time(this%end_time)

        if (this%is_running) then
            this%is_running = .False.
            this%total_time = this%total_time + (this%end_time - this%start_time)
        endif
    end subroutine stop

    !>-----------------------------------
    !! Reset the timer
    !!
    !! Resets timer internal variables as if it was never running
    !!
    !------------------------------------
    module subroutine reset(this)
        class(timer_t), intent(inout) :: this

        this%total_time = 0
        this%start_time = 0
        this%end_time   = 0
        this%is_running = .False.
    end subroutine reset

    !>-----------------------------------
    !! Return the time as a real
    !!
    !! If the timer is running, it includes the current time in the total reported
    !!
    !------------------------------------
    module function get_time(this) result(time)
        class(timer_t),    intent(inout)        :: this

        real :: time ! return value

        real :: current_time

        if (this%is_running) then
            call cpu_time(current_time)
            time = this%total_time + (current_time - this%start_time)
        else
            time = this%total_time
        endif

    end function get_time


    !>-----------------------------------
    !! Return the time as a string
    !!
    !! If the timer is running, it includes the current time in the total reported
    !!
    !------------------------------------
    module function as_string(this, format) result(time)
        class(timer_t),    intent(inout)        :: this
        character(len=*), intent(in), optional :: format

        character(len=25) :: time ! return value

        real :: temporary_time, current_time

        if (this%is_running) then
            call cpu_time(current_time)
            temporary_time = this%total_time + (current_time - this%start_time)
        else
            temporary_time = this%total_time
        endif

        ! if the user specified a format string, use that when creating the output
        if (present(format)) then
            write(time,format) temporary_time
        else
            write(time,*) temporary_time
        endif

    end function as_string

end submodule timer_implementation
