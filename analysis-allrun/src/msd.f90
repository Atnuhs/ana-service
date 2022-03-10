program main
    use,intrinsic :: iso_fortran_env
    use read_condition_mod
    use aggregate_x_mod
    use aggregate_x_y_mod
    implicit none
    integer(int32):: ndata
    integer(int32):: fst_run, lst_run

    read*, fst_run, lst_run
    call input_condition(ndata)
    call aggregate_x('msd/translational_diffusion_coefficient', fst_run, lst_run)
    call aggregate_x_y_ysd('msd/msd_mean', fst_run, lst_run, ndata)
contains
    subroutine input_condition(ndata)
        integer(int32),intent(out):: ndata
        type(condition_type):: condition

        call read_condition(condition)
        ndata = condition%nstep/condition%intd
    end subroutine
end program main