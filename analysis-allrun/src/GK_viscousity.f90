program main
    use,intrinsic :: iso_fortran_env
    use read_condition_mod
    use aggregate_x_y_mod
    implicit none
    integer(int32):: ndata, fst_run, lst_run

    read*, fst_run, lst_run
    call input_condition(ndata)
    call aggregate_x_y('GK_viscousity/integ_acf_stress', fst_run, lst_run, ndata/10)
contains
    subroutine input_condition(ndata)
        integer(int32),intent(out):: ndata
        type(condition_type):: condition

        call read_condition(condition)
        ndata = condition%nstep
    end subroutine
end program main
