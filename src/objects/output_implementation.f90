submodule(output_interface) output_implementation
  implicit none

contains

    module subroutine add_to_output(this, variable)
        class(output_t),   intent(inout)  :: this
        class(variable_t), intent(in)     :: variable

        if (.not.this%is_initialized) call this%init()

        if (this%n_variables == size(this%variables)) call this%increase_holding_capacity()

        this%n_variables = this%n_variables + 1
        this%variables(this%n_variables)%ptr => variable

    end subroutine


    module subroutine write(this, filename)
        class(output_t), intent(inout)  :: this
        character(len=kMAX_FILE_LENGTH), intent(in) :: filename

        if (.not.this%is_initialized) call this%init()

        ! open file
        this%filename = filename
        call check( nf90_open(filename, NF90_WRITE, this%ncfile_id), "Opening file "//trim(filename))

        ! define variables or find variable IDs (and dimensions)
        call setup_variables(this)

        ! store output
        call save_data(this)

        ! close file
        call check(nf90_close(this%ncfile_id), "Closing file "//trim(filename))
    end subroutine


    subroutine setup_variables(this)
        implicit none
        class(output_t), intent(inout) :: this
        integer :: i

        ! iterate through variables creating or setting up variable IDs if they exist, also dimensions
        do i=1,this%n_variables
            ! create all dimensions or find dimension IDs if they exist already
            call setup_dims_for_var(this, this%variables(i)%ptr)

            call setup_variable(this, this%variables(i)%ptr)
        end do

        ! End define mode. This tells netCDF we are done defining metadata.
        call check( nf90_enddef(ncid) )

    end subroutine setup_variables


    subroutine save_data(this)
        implicit none
        class(output_t), intent(in) :: this

        do i=1,this%n_variables
            associate(var => this%variables(i)%ptr )
                call check( nf90_put_var(this%ncfile_id, var%var_id,  var%local),
                            "saving:"//trim(var%name) )
            end associate
        end do

    end subroutine save_data

    subroutine setup_dims_for_var(this, var)
        implicit none
        class(output_t), intent(inout) :: this
        class(variable_t), intent(inout) :: var

        do i = 1, var%n_dims

            ! Try to find the dimension ID if it exists already.
            err = nf90_inq_dimid(this%ncfile_id, trim(var%dimensions(i)), var%dim_ids(i))

            ! probably the dimension doesn't exist in the file, so we will create it.
            if (err/=NF90_NOERR) then

                ! dim_len == -1 if the dimension should be "unlimited"
                if (var%dim_len(i) == -1) then
                    call check( nf90_def_dim(ncid, trim(var%dimensions(i)), NF90_UNLIMITED, var%dim_ids(i) ), \
                                trim(err)//var%dimensions(i) )
                else
                    call check( nf90_def_dim(ncid, var%dimensions(i), var%dim_len(i), var%dim_ids(i) ), \
                                trim(err)//var%dimensions(i) )
                endif
            endif
        end do

    end subroutine setup_dims_for_var

    subroutine setup_variable(this, var)
        implicit none
        class(output_t),   intent(inout) :: this
        class(variable_t), intent(inout) :: var
        integer :: i, err

        err = nf90_inq_varid(this%ncfile_id, var%name, var%var_id)

        ! if the variable was not found in the netcdf file then we will define it.
        if (err /= NF90_NOERR) then
            call check( nf90_def_var(this%ncfile_id, var%name, NF90_REAL, var%dim_ids, var%var_id),
                        "Defining variable:"//trim(var%name) )
        endif

        ! setup attributes
        do i=1,size(var%attribute_names)
            call check( nf90_put_att(this%ncfile_id,         \
                                     var%var_id,             \
                                     var%attribute_names(i), \
                                     var%attribute_values(i)))
        enddo

    end subroutine setup_variable

    module subroutine init(this)
        implicit none
        class(output_t),   intent(inout)  :: this

        allocate(this%variables(kINITIAL_VAR_SIZE))
        this%n_variables = 0
        this%n_dims      = 0
        this%is_initialized = .True.


    end subroutine

    module subroutine increase_holding_capacity(this)
        implicit none
        class(output_t),   intent(inout)  :: this

        type(variable_ptr), allocatable :: new_variables(:)

        allocate(new_variables(size(this%variables)))

        new_variables = this%variables

        deallocate(this%variables)

        allocate(this%variables(size(new_variables)*2))
        this%variables(:size(new_variables)) = new_variables

        deallocate(new_variables)

    end subroutine


    !>------------------------------------------------------------
    !! Simple error handling for common netcdf file errors
    !!
    !! If status does not equal nf90_noerr, then print an error message and STOP
    !! the entire program.
    !!
    !! @param   status  integer return code from nc_* routines
    !! @param   extra   OPTIONAL string with extra context to print in case of an error
    !!
    !!------------------------------------------------------------
    subroutine check(status,extra)
        implicit none
        integer, intent ( in) :: status
        character(len=*), optional, intent(in) :: extra

        ! check for errors
        if(status /= nf90_noerr) then
            ! print a useful message
            print *, trim(nf90_strerror(status))
            if(present(extra)) then
                ! print any optionally provided context
                write(*,*) trim(extra)
            endif
            ! STOP the program execution
            stop "Stopped"
        end if
    end subroutine check


end submodule
