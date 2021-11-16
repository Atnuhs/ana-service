module aggregate_y_ysd_data_mod
    use,intrinsic :: iso_fortran_env
    use representative_value_mod
    implicit none
contains
    function rundir(run)
        integer(int32),intent(in):: run
        character(5):: rundir

        write(rundir, "('run', I2.2)") run
    end function

    subroutine read_y_ysd_file(file_data, y, y_sd)
        character(*),intent(in):: file_data
        real(real64),intent(out):: y, y_sd
        integer(int32):: u_data

        open(newunit=u_data, file=file_data, status='old')
            read(u_data, *) y, y_sd
        close(u_data)
    end subroutine


    subroutine aggregate_y_ysd_data(filename, fst_run, lst_run)
        ! intent
        character(*),intent(in):: filename
        integer(int32),intent(in):: fst_run, lst_run
        ! var
        real(real64):: data_all(fst_run:lst_run), data_sd_all(fst_run:lst_run)
        real(real64):: data_mean, data_sd
        integer(int32):: run, i, u_data
        character(100):: file_data, cformat

        ! reading_part
        do run=fst_run, lst_run
            file_data = "../calculation/" // rundir(run) // "/Analysis/" // filename // ".dat"
            call read_y_ysd_file(trim(file_data), data_all(run), data_sd_all(run))
        end do

        ! calc stat
        data_mean = mean(data_all(:))
        data_sd = sqrt(mean(data_sd_all**2))

        ! output
        open(newunit=u_data, file=filename // "_all.txt", status="replace")
            write(cformat, "('(2e20.10)')") 
            write(u_data, cformat) (data_all(i), data_sd_all(i), i=fst_run, lst_run)
        close(u_data)

        open(newunit=u_data, file=filename // "_mean.txt", status="replace")
            write(cformat, "('(2e20.10)')")
            write(u_data, cformat) data_mean, data_sd
        close(u_data)
    end subroutine
end module
