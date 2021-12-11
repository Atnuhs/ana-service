module io_x_data_mod
    use,intrinsic :: iso_fortran_env
    implicit none
contains
    subroutine read_x(file_data, x)
        character(*),intent(in):: file_data
        real(real64),intent(out):: x
        integer(int32):: u_data

        open(newunit=u_data, file=file_data, status='old')
            read(u_data, *) x
        close(u_data)
    end subroutine


    subroutine write_x(file_data, x)
        character(*),intent(in):: file_data
        real(real64),intent(out):: x
        integer(int32):: u_data

        open(newunit=u_data, file=file_data, status='replace')
            write(u_data, *) x
        close(u_data)
    end subroutine
end module io_x_data_mod



module io_x_xsd_data_mod
    use,intrinsic :: iso_fortran_env
    implicit none
contains
    subroutine read_x_xsd(file_data, x, x_sd)
        character(*),intent(in):: file_data
        real(real64),intent(out):: x, x_sd
        integer(int32):: u_data

        open(newunit=u_data, file=file_data, status='old')
            read(u_data, *) x, x_sd
        close(u_data)
    end subroutine


    subroutine write_x_xsd(file_data, x, x_sd)
        character(*),intent(in):: file_data
        real(real64),intent(out):: x, x_sd
        integer(int32):: u_data

        open(newunit=u_data, file=file_data, status='replace')
            write(u_data, *) x, x_sd
        close(u_data)
    end subroutine
end module



module io_arx_ary_data_mod
    use,intrinsic :: iso_fortran_env
    implicit none
contains
     subroutine read_arx_ary(data_len, file_data, arx, ary)
        integer(int32),intent(in):: data_len
        character(*),intent(in):: file_data
        real(real64),intent(out):: arx(:), ary(:)
        integer(int32):: u_data, i

        open(newunit=u_data, file=file_data, status='old')
            read(u_data, *) (arx(i), ary(i), i=1,data_len)
        close(u_data)
    end subroutine

    subroutine write_arx_ary(data_len, file_data, arx, ary)
        integer(int32),intent(in):: data_len
        character(*),intent(in):: file_data
        real(real64),intent(out):: arx(:), ary(:)
        integer(int32):: u_data, i

        open(newunit=u_data, file=file_data, status='replace')
            write(u_data, "(2e20.10)") (arx(i), ary(i), i=1,data_len)
        close(u_data)
    end subroutine
end module io_arx_ary_data_mod



module io_arx_ary_arysd_data_mod
    use,intrinsic :: iso_fortran_env
    implicit none
contains
    subroutine read_x_y_ysd(data_len, file_data, arx, ary, arysd)
        integer(int32),intent(in):: data_len
        character(*),intent(in):: file_data
        real(real64),intent(out):: arx(:), ary(:), arysd(:)
        integer(int32):: u_data, i

        open(newunit=u_data, file=file_data, status='old')
            read(u_data, *) (arx(i), ary(i), arysd(i), i=1,data_len)
        close(u_data)
    end subroutine


    subroutine write_x_y_ysd(data_len, file_data, arx, ary, arysd)
        integer(int32),intent(in):: data_len
        character(*),intent(in):: file_data
        real(real64),intent(out):: arx(:), ary(:), arysd(:)
        integer(int32):: u_data, i

        open(newunit=u_data, file=file_data, status='replace')
            write(u_data, '(3e20.10)') (arx(i), ary(i), arysd(i), i=1,data_len)
        close(u_data)
    end subroutine
end module io_arx_ary_arysd_data_mod



module io_arx_ary_arz_data_mod
    use,intrinsic :: iso_fortran_env
    implicit none
contains
    subroutine read_arx_ary_arz(arx_len, ary_len, file_data, arx, ary, arz)
        integer(int32),intent(in):: arx_len, ary_len
        character(*),intent(in):: file_data
        real(real64),intent(out):: arx(arx_len), ary(ary_len), arz(ary_len, arx_len)
        integer(int32):: u_data, ix, iy

        open(newunit=u_data, file=file_data, status='old')
            read(u_data, *) ((arx(ix), ary(iy), arz(iy,ix), iy=1,ary_len), ix=1,arx_len)
        close(u_data)
    end subroutine


    subroutine write_arx_ary_arz(arx_len, ary_len, file_data, arx, ary, arz)
        integer(int32),intent(in):: arx_len, ary_len
        character(*),intent(in):: file_data
        real(real64),intent(out):: arx(arx_len), ary(ary_len), arz(ary_len, arx_len)
        integer(int32):: u_data, ix, iy

        open(newunit=u_data, file=file_data, status='replace')
            write(u_data, '(3e20.10)') ((arx(ix), ary(iy), arz(iy,ix), iy=1,ary_len), ix=1,arx_len)
        close(u_data)
    end subroutine
end module









module io_file_mod
    use,intrinsic :: iso_fortran_env
    use io_x_data_mod
    use io_x_xsd_data_mod
    use io_arx_ary_data_mod
    use io_arx_ary_arysd_data_mod
    use io_arx_ary_arz_data_mod
    implicit none
contains
    function rundir(run)
        ! if input: run = 1 => output: rundir = 'run01'
        integer(int32),intent(in):: run
        character(5):: rundir

        write(rundir, "('run', I2.2)") run
    end function
end module io_file_mod