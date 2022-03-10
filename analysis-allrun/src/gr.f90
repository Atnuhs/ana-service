program main
    use,intrinsic :: iso_fortran_env
    use aggregate_x_y_mod
    implicit none
    integer(int32),parameter:: gr_len = 1000
    integer(int32):: fst_run, lst_run

    read*, fst_run, lst_run
    call aggregate_x_y('gr/gr', fst_run, lst_run, gr_len)
end program main