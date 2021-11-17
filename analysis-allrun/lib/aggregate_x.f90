module aggregate_x_mod
    use,intrinsic :: iso_fortran_env
    use io_file_mod
    use representative_value_mod
    implicit none
contains
    subroutine aggregate_x(filename, fst_run, lst_run)
        ! intent
        character(*),intent(in):: filename
        integer(int32),intent(in):: fst_run, lst_run
        ! var
        real(real64):: data_all(fst_run:lst_run)
        real(real64):: data_mean, data_sd
        integer(int32):: run, i, u_data
        character(100):: file_data, cformat

        ! reading_part
        do run=fst_run, lst_run
            file_data = "../calculation/" // rundir(run) // "/Analysis/" // filename // ".dat"
            call read_x(trim(file_data), data_all(run))
        end do

        ! calc stat
        data_mean = mean(data_all(:), sd=data_sd)

        ! output
        open(newunit=u_data, file=filename // "_all.txt", status="replace")
            write(cformat, "('(e20.10)')") 
            write(u_data, cformat) (data_all(i), i=fst_run, lst_run)
        close(u_data)

        open(newunit=u_data, file=filename // "_mean.txt", status="replace")
            write(cformat, "('(2e20.10)')")
            write(u_data, cformat) data_mean, data_sd
        close(u_data)
    end subroutine


    subroutine aggregate_x_xsd(filename, fst_run, lst_run)
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
            call read_x_xsd(trim(file_data), data_all(run), data_sd_all(run))
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
