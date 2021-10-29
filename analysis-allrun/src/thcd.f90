program main
    use,intrinsic :: iso_fortran_env
    use read_condition_mod
    use representative_value_mod
    implicit none
    integer(int32):: fst_run, lst_run
    read*, fst_run, lst_run
    call generate_thcd_from_run_thcd(fst_run, lst_run)
    call generate_thcd_from_gk_thcd()
contains
    subroutine generate_thcd_from_run_thcd(fst_run, lst_run)
        integer(int32),intent(in):: fst_run, lst_run
        integer(int32):: run
        real(real64):: thcd_run(fst_run:lst_run)
        real(real64):: thcd, thcd_sd

        ! thcdの平均を計算
        do run=fst_run, lst_run
            call read_thcd_run(thcd_run(run), run)
        end do

        call calc_thcd_run(thcd_run, thcd, thcd_sd, fst_run, lst_run)
        call output_thcd_run(thcd, thcd_sd)
    end subroutine


    subroutine read_thcd_run(thcd, run)
        integer(int32),intent(in):: run
        real(real64),intent(out):: thcd
        character(100):: file_thcd, crun
        integer(int32):: u_thcd

        write(crun,'(I2.2)') run
        file_thcd = '../calculation/run' // trim(crun) // '/Analysis/thcd/thcd.dat'
        open(newunit=u_thcd, file=file_thcd, status='old')
            read(u_thcd, *) thcd
        close(u_thcd)
    end subroutine


    subroutine calc_thcd_run(thcd_run, thcd, thcd_sd, fst_run, lst_run)
        real(real64),intent(out):: thcd, thcd_sd
        integer(int32),intent(in):: fst_run, lst_run
        real(real64),intent(in):: thcd_run(fst_run:lst_run)
        
        thcd = mean(thcd_run(fst_run:lst_run), sd=thcd_sd)
    end subroutine


    subroutine output_thcd_run(thcd, thcd_sd)
        character(100),parameter:: file_thcd_mean = 'thcd/thcd_mean.txt'
        real(real64):: thcd, thcd_sd
        integer(int32):: u_thcd_mean

        open(newunit=u_thcd_mean, file=file_thcd_mean, status='replace')
            write(u_thcd_mean, *) 'thcd, thcd_sd'
            write(u_thcd_mean, *) thcd, thcd_sd
        close(u_thcd_mean)
    end subroutine


    subroutine generate_thcd_from_gk_thcd()
        integer(int32),parameter:: fst_calc=3000, lst_calc=6000, all_calc=lst_calc-fst_calc+1
        integer(int32):: ndata
        real(real64),allocatable:: integ_ej_mean(:), integ_ej_se(:), x(:)
        real(real64):: thcd, thcd_sd

        call input_condition(ndata)
        allocate(integ_ej_mean(ndata), integ_ej_se(ndata), x(ndata))
        call read_integ_ej_mean(integ_ej_mean, integ_ej_se, x, ndata)
        thcd = mean(integ_ej_mean(fst_calc:lst_calc), sd=thcd_sd)
        call output_thcd_ej_mean(thcd, thcd_sd)
        call output_integ_ej_mean_lm(integ_ej_mean, integ_ej_se, x, lst_calc)
    end subroutine


    subroutine input_condition(ndata)
        integer(int32),intent(out):: ndata
        type(condition_type):: condition

        call read_condition(condition)
        ndata = condition%nstep
    end subroutine


    subroutine read_integ_ej_mean(integ_ej_mean, integ_ej_se, x, ndata)
        character(100),parameter:: file_integ_ej_mean = 'GK_thcd/integ_ej_mean.txt'
        integer(int32),intent(in):: ndata
        real(real64),intent(out):: integ_ej_mean(ndata),  x(ndata), integ_ej_se(ndata)
        integer(int32):: u_integ_ej_mean, i

        open(newunit=u_integ_ej_mean, file=file_integ_ej_mean, status='old')
            read(u_integ_ej_mean, '(3e20.8)') (x(i), integ_ej_mean(i), integ_ej_se(i), i=1,ndata/10)
        close(u_integ_ej_mean)
    end subroutine


    subroutine output_thcd_ej_mean(thcd, thcd_sd)
        character(100),parameter:: file_thcd_ej_mean = 'thcd/thcd_ej_mean.txt'
        real(real64):: thcd, thcd_sd
        integer(int32):: u_thcd_ej_mean

        open(newunit=u_thcd_ej_mean, file=file_thcd_ej_mean, status='replace')
            write(u_thcd_ej_mean, *) 'thcd, thcd_sd'
            write(u_thcd_ej_mean, *) thcd, thcd_sd
        close(u_thcd_ej_mean)
    end subroutine


    subroutine output_integ_ej_mean_lm(integ_ej_mean, integ_ej_se, x, lst_calc)
        character(100),parameter:: file_integ_ej_mean = 'thcd/integ_ej_mean_lm.txt'
        integer(int32),intent(in):: lst_calc
        real(real64),intent(in):: integ_ej_mean(:), integ_ej_se(:), x(:)
        integer(int32):: u_integ_ej_mean, i

        open(newunit=u_integ_ej_mean, file=file_integ_ej_mean, status='replace')
            write(u_integ_ej_mean, '(3e20.8)') (x(i), integ_ej_mean(i), integ_ej_se(i), i=1,lst_calc)
        close(u_integ_ej_mean)
    end subroutine
end program main