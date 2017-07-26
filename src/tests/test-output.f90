!> ------------------------------
!!  Test for the NetCDF Creation output object
!!
!! If it is working correctly, it should create a netcdf file "test.nc"
!! which looks like this:
!! <pre>
!! netcdf test {
!! dimensions:
!!  x = 20 ;
!!  y = 2 ;
!!  z = 30 ;
!! variables:
!!  float test_b(z, y, x) ;
!!      test_b:a = "a1" ;
!!      test_b:b = "b1" ;
!!      test_b:c = "c1" ;
!!  float test_d(z, y, x) ;
!!      test_d:a = "a1" ;
!!      test_d:b = "b1" ;
!!      test_d:c = "c1" ;
!! }
!! </pre>
!!
!! --------------------------------

program test_output

    use constants
    use output_interface, only : output_t
    use variable_interface, only : variable_t

    implicit none

    type(output_t) :: dataset
    type(variable_t) :: vars(4)

    real, allocatable, target :: local(:,:,:)

    allocate(local(20,2,30))

    call setup(vars(1), "test_a", local)
    call setup(vars(2), "test_b", local)
    call setup(vars(3), "test_c", local)
    call setup(vars(4), "test_d", local)

    call dataset%add_to_output(vars(2))
    call dataset%add_to_output(vars(4))

    call dataset%write("test.nc")

contains
    subroutine setup(var, name, input_data)
        implicit none
        type(variable_t), intent(inout) :: var
        character(len=*), intent(in) :: name
        real, target :: input_data(:,:,:)

        var%name=name
        var%n_attrs=3
        var%dim_len = [size(input_data,1),size(input_data,2),size(input_data,3),0]
        var%dimensions=[character(len=1024) :: "x", "y", "z",""]
        var%attribute_names=["a","b","c"]
        var%attribute_values=["a1","b1","c1"]
        var%local => input_data

    end subroutine setup


end program test_output
