module aggregate_xyz_data_mod
    use,intrinsic :: iso_fortran_env
    use representative_value_mod
    implicit none
contains
    function rundir(run)
        integer(int32),intent(in):: run
        character(5):: rundir

        write(rundir, "('run', I2.2)") run
    end function

    subroutine read_xyz_file(x_len, y_len, file_data, x, y, z)
        integer(int32),intent(in):: x_len, y_len
        character(*),intent(in):: file_data
        real(real64),intent(out):: x(x_len), y(y_len), z(y_len, x_len)
        integer(int32):: u_data, ix, iy

        open(newunit=u_data, file=file_data, status='old')
            read(u_data, *) ((x(ix), y(iy), z(iy,ix), iy=1,y_len), ix=1,x_len)
        close(u_data)
    end subroutine


    subroutine aggregate_xyz_data(filename, fst_run, lst_run, x_len, y_len)
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
            call read_xyz_file(x_len, y_len, trim(file_data), x, y, data_all(:,:,run))
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
