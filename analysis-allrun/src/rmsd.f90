program main
    use,intrinsic :: iso_fortran_env
    use read_condition_mod
    use aggregate_y_data_mod
    use aggregate_xy_ysd_data_mod
    implicit none
    integer(int32):: ndata
    integer(int32):: fst_run, lst_run

    read*, fst_run, lst_run
    call input_condition(ndata)
    call aggregate_y_data('rmsd/rotational_diffusion_coefficient', fst_run, lst_run)
    call aggregate_xy_ysd_data('rmsd/mean_rmsd', fst_run, lst_run, ndata)
contains
    subroutine input_condition(ndata)
        integer(int32),intent(out):: ndata
        type(condition_type):: condition

        call read_condition(condition)
        ndata = condition%nstep/condition%intd
    end subroutine
end program main