module aggregate_xy_ysd_data_mod
    use,intrinsic :: iso_fortran_env
    use representative_value_mod
    implicit none
contains
    function rundir(run)
        integer(int32),intent(in):: run
        character(5):: rundir

        write(rundir, "('run', I2.2)") run
    end function

    subroutine read_x_y_ysd(data_len, file_data, x, y, y_sd)
        integer(int32),intent(in):: data_len
        character(*),intent(in):: file_data
        real(real64),intent(out):: x(:), y(:), y_sd(:)
        integer(int32):: u_data, i

        open(newunit=u_data, file=file_data, status='old')
            read(u_data, *) (x(i), y(i), y_sd(i), i=1,data_len)
        close(u_data)
    end subroutine


    subroutine aggregate_xy_ysd_data(filename, fst_run, lst_run, data_len)
        ! intent
        character(*),intent(in):: filename
        integer(int32),intent(in):: fst_run, lst_run, data_len
        ! var
        real(real64):: y_all(data_len, fst_run:lst_run), y_sd_all(data_len, fst_run:lst_run)
        real(real64):: x(data_len), y_mean(data_len), y_sd(data_len)
        integer(int32):: run, i, u_data
        character(100):: file_data, cformat

        ! reading_part
        do run=fst_run, lst_run
            file_data = "../calculation/" // rundir(run) // "/Analysis/" // filename // ".dat"
            call read_x_y_ysd(data_len, trim(file_data), x, y_all(:,run), y_sd_all(:,run))
        end do

        ! calc stat 
        do i=1,data_len
            ! run数回の測定それぞれで分散が等しいと考えられるので
            ! 分散の平均 = それぞれの分散の平均となる。
            y_mean(i) = mean(y_all(i,:))
            y_sd(i) = sqrt(mean(y_sd_all(i,:)**2))
        end do

        ! output
        open(newunit=u_data, file=filename // "_all.txt", status="replace")
            write(cformat, "('(', I0, 'e20.10)')") lst_run-fst_run+2
            write(u_data, cformat) (x(i), y_all(i,:), i=1,data_len)
        close(u_data)

        open(newunit=u_data, file=filename // "_mean.txt", status="replace")
            write(cformat, "('(3e20.10)')")
            write(u_data, cformat) (x(i), y_mean(i), y_sd(i), i=1,data_len)
        close(u_data)
    end subroutine
end module
