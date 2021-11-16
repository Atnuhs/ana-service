module convergence_green_kubo_formula_mod
    use,intrinsic :: iso_fortran_env
    use representative_value_mod
    implicit none
contains
    subroutine read_integ(data_len, file_data, x, integ_gk, integ_gk_sd)
        integer(int32),intent(in):: data_len
        character(*),intent(in):: file_data
        real(real64),intent(out):: x(data_len), integ_gk(data_len), integ_gk_sd(data_len)
        integer(int32):: u_data, i

        open(newunit=u_data, file=file_data, status='old')
            read(u_data, *) (x(i), integ_gk(i), integ_gk_sd(i), i=1,data_len)
        close(u_data)
    end subroutine


    subroutine convergence_green_kubo_formula(file_integ, file_conv, file_integ_lim, fst_calc, lst_calc, data_len)
        ! intent
        character(*),intent(in):: file_integ, file_conv, file_integ_lim
        integer(int32),intent(in):: fst_calc, lst_calc, data_len
        ! var
        real(real64):: x(data_len), integ_gk(data_len), integ_gk_sd(data_len)
        real(real64):: conv_gk, conv_gk_sd
        integer(int32):: i, u_data

        ! reading_part
        call read_integ(data_len, file_integ, x, integ_gk, integ_gk_sd)

        ! calc stat 
        conv_gk = mean(integ_gk(fst_calc:lst_calc))
        conv_gk_sd = sqrt(mean(integ_gk_sd(fst_calc:lst_calc)**2))

        ! output
        open(newunit=u_data, file=file_conv, status="replace")
            write(u_data, '(2e20.10)') conv_gk, conv_gk_sd
        close(u_data)

        open(newunit=u_data, file=file_integ_lim, status="replace")
            write(u_data, '(3e20.10)') (x(i), integ_gk(i), integ_gk_sd(i), i=1,lst_calc)
        close(u_data)
    end subroutine
end module
