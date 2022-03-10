program main
    use,intrinsic :: iso_fortran_env
    use md_condition_for_ana_mod
    use convergence_green_kubo_formula_mod
    implicit none
    character(100),parameter:: file_integ = 'GK_thcd/integ_acf_ej.dat'
    character(100),parameter:: file_conv = 'thcd/thcd.dat'
    integer(int32):: fst_calc = 4000, lst_calc = 7000, ndata
    real(real64), allocatable:: integ_acf_stress(:)

    call load_condition_for_thcd_ana(ndata)
    call convergence_green_kubo_formula(file_integ, file_conv, fst_calc, lst_calc, ndata)
end program