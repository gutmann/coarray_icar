submodule(timer_interface) timer_implementation

    implicit none

contains

    !>-----------------------------------
    !! Start the timer
    !!
    !! Sets the internal start_time and marks the timer as running
    !!
    !------------------------------------
    module subroutine start(this, use_cpu_time)
        class(timer_t), intent(inout) :: this
        logical,        intent(in),   optional :: use_cpu_time

        logical :: cpu_only
        cpu_only = merge(use_cpu_time, .false., present(use_cpu_time))

        this%is_running = .True.
        this%use_cpu_time = cpu_only
        if (cpu_only) then
            call cpu_time(this%start_time)
        else
            call system_clock(this%counter, this%COUNT_RATE, this%COUNT_MAX)
        endif
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
        integer :: count_end

        ! do this before we even test anything else because we want the timer to "stop" as soon as possible
        if (this%use_cpu_time) then
            call cpu_time(this%end_time)
        else
            call system_clock(count_end)
            this%end_time = (count_end - this%counter) / real(this%count_rate)
        endif

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
        integer :: count_end

        if (this%is_running) then
            if (this%use_cpu_time) then
                call cpu_time(current_time)
            else
                call system_clock(count_end)
                current_time = (count_end - this%counter) / real(this%count_rate)
            endif
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
        integer :: count_end

        if (this%is_running) then
            if (this%use_cpu_time) then
                call cpu_time(current_time)
            else
                call system_clock(count_end)
                current_time = (count_end - this%counter) / real(this%count_rate)
            endif
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
