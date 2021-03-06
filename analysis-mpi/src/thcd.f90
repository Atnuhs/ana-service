program main
    use,intrinsic :: iso_fortran_env
    use md_condition_for_ana_mod
    use representative_value_mod
    use io_file_mod
    implicit none
    character(100),parameter:: file_integ = 'GK_thcd/integ.txt'
    character(100),parameter:: file_thcd = 'thcd/thcd.txt'
    integer(int32):: ndata, fst_calc, lst_calc
    real(real64), allocatable:: x(:), integ(:), integ_stdev(:)
    real(real64):: temp0, temp, temp_adjust
    real(real64):: convergence, convergence_stdev

    read*, fst_calc, lst_calc
    call load_condition_for_thcd_ana(ndata, temp0)
    call read_x('./temp/temp_mean.txt', temp)
    allocate(x(ndata), integ(ndata), integ_stdev(ndata))
    call read_arx_ary_arysd(file_integ, ndata, x, integ, integ_stdev)
    convergence = mean(integ(fst_calc:lst_calc), stdev=convergence_stdev)
    temp_adjust = (temp0/temp)**0.25d0
    print*, temp, temp0, temp_adjust
    convergence = convergence * temp_adjust
    convergence_stdev = convergence_stdev * temp_adjust
    call write_x_xsd(file_thcd, convergence, convergence_stdev)
end program