program main
    use,intrinsic :: iso_fortran_env
    use read_condition_mod
    implicit none
    integer(int32):: fst_run, lst_run
    read*, fst_run, lst_run
    call generate_thcd_from_run_thcd(fst_run, lst_run)
    call generate_thcd_from_gk_thcd()
contains
    subroutine generate_thcd_from_run_thcd(fst_run, lst_run)
        use aggregate_x_mod
        integer(int32),intent(in):: fst_run, lst_run
        
        call aggregate_x_xsd('thcd/thcd', fst_run, lst_run)
    end subroutine


    subroutine generate_thcd_from_gk_thcd()
        use convergence_green_kubo_formula_mod
        character(100),parameter:: file_integ = 'GK_thcd/integ_acf_ej_mean.txt'
        character(100),parameter:: file_conv = 'thcd/thcd_acf_ej_mean.txt'
        character(100),parameter:: file_integ_lim = 'thcd/integ_acf_ej_mean_lm.txt'
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
