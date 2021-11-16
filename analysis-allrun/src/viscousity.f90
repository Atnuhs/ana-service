program main
    use,intrinsic :: iso_fortran_env
    use read_condition_mod
    implicit none
    integer(int32):: fst_run, lst_run

    read*, fst_run, lst_run
    call generate_vis_from_run_vis(fst_run, lst_run)
    call generate_vis_from_gk_vis()
   
contains
 subroutine generate_vis_from_run_vis(fst_run, lst_run)
        use aggregate_y_ysd_data_mod
        integer(int32),intent(in):: fst_run, lst_run
        
        call aggregate_y_ysd_data('viscousity/viscousity', fst_run, lst_run)
    end subroutine


    subroutine generate_vis_from_gk_vis()
        use convergence_green_kubo_formula_mod
        character(100),parameter:: file_integ = 'GK_viscousity/integ_stress_mean.txt'
        character(100),parameter:: file_conv = 'viscousity/viscousity_stress_mean.txt'
        character(100),parameter:: file_integ_lim = 'viscousity/integ_stress_mean_lm.txt'
        integer(int32),parameter:: fst_calc=3000, lst_calc=6000
        integer(int32):: ndata
        call input_condition(ndata)
        call convergence_green_kubo_formula(trim(file_integ), trim(file_conv), trim(file_integ_lim), fst_calc, lst_calc, ndata / 10)
    end subroutine


    subroutine input_condition(ndata)
        integer(int32),intent(out):: ndata
        type(condition_type):: condition

        call read_condition(condition)
        ndata = condition%nstep
    end subroutine
end program main