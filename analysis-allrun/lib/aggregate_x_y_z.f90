module aggregate_x_y_z_mod
    use io_file_mod
    use,intrinsic :: iso_fortran_env
    use representative_value_mod
    implicit none
contains
    subroutine aggregate_x_y_z(filename, fst_run, lst_run, x_len, y_len)
        ! intent
        character(*),intent(in):: filename
        integer(int32),intent(in):: fst_run, lst_run, x_len, y_len
        ! var
        real(real64):: data_all(y_len, x_len, fst_run:lst_run)
        real(real64):: x(x_len), y(y_len), data_mean(y_len, x_len), data_sd(y_len, x_len)
        integer(int32):: run, ix, iy, u_data
        character(100):: file_data, cformat

        ! reading_part
        do run=fst_run, lst_run
            file_data = "../calculation/" // rundir(run) // "/Analysis/" // filename // ".dat"
            call read_x_y_z(x_len, y_len, trim(file_data), x, y, data_all(:,:,run))
        end do

        ! calc stat
        do ix=1, x_len
            do iy=1, y_len
                data_mean(iy, ix) = mean(data_all(iy, ix, :), sd=data_sd(iy, ix))
            end do
        end do

        ! output
        open(newunit=u_data, file=filename // "_mean.txt", status="replace")
            write(cformat, "('(4e20.10)')")
            write(u_data, cformat) ((x(ix), y(iy), data_mean(iy, ix), data_sd(iy, ix), iy=1,y_len), ix=1,x_len)
        close(u_data)
    end subroutine
end module
