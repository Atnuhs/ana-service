program main
    use,intrinsic :: iso_fortran_env
    use aggregate_xyz_data_mod
    implicit none
    integer(int32),parameter:: x_len=100, y_len=90
    integer(int32):: fst_run, lst_run, run

    read*, fst_run, lst_run
    call aggregate_xyz_data('molecular_orientation/molecular_orientation', fst_run, lst_run, x_len, y_len)
    call aggregate_xyz_data('molecular_orientation/molecular_orientation2', fst_run, lst_run, x_len, y_len)
end program main