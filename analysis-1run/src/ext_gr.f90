program main
    use ext_gr_calculation_mod
    use,intrinsic :: iso_fortran_env
    implicit none
    integer(int32), parameter:: np=500, gr_len=1000

    call init_ext_gr(this_np=np, this_gr_len=gr_len)
    call  calculate_ext_gr()
end program main