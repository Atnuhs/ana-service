program main
    use,intrinsic :: iso_fortran_env
    use read_condition_mod
    use representative_value_mod
    implicit none
    integer(int32):: fst_run, lst_run

    read*, fst_run, lst_run
    call generate_vis_from_run_vis(fst_run, lst_run)
    call generate_vis_from_gk_vis()
   
contains
    subroutine generate_vis_from_run_vis(fst_run, lst_run)
        integer(int32),intent(in):: fst_run, lst_run
        integer(int32):: run
        real(real64):: viscousity_run(fst_run:lst_run)
        real(real64):: viscousity, viscousity_sd

        do run=fst_run, lst_run
            call read_viscousity_run(viscousity_run(run), run)
        end do
    
        call calc_viscousity(viscousity_run, viscousity, viscousity_sd, fst_run, lst_run)
        call output_viscousity(viscousity, viscousity_sd)
    end subroutine


    subroutine read_viscousity_run(viscousity, run)
        real(real64),intent(out):: viscousity
        character(100):: file_viscousity, crun
        integer(int32):: u_viscousity, run

        write(crun,'(I2.2)') run
        file_viscousity = '../calculation/run' // trim(crun) // '/Analysis/viscousity/viscousity.dat'
        open(newunit=u_viscousity, file=file_viscousity, status='old')
            read(u_viscousity, *) viscousity
        close(u_viscousity)
    end subroutine


    subroutine calc_viscousity(viscousity_run, viscousity, viscousity_sd, fst_run, lst_run)
        real(real64),intent(out):: viscousity, viscousity_sd
        integer(int32),intent(in):: fst_run, lst_run
        real(real64),intent(in):: viscousity_run(fst_run:lst_run)
        
        viscousity = mean(viscousity_run(fst_run:lst_run), sd=viscousity_sd)
    end subroutine

    subroutine input_condition(ndata)
        integer(int32),intent(out):: ndata
        type(condition_type):: condition

        call read_condition(condition)
        ndata = condition%nstep
    end subroutine


    subroutine output_viscousity(viscousity, viscousity_sd)
        character(100),parameter:: file_viscousity_mean = 'viscousity/viscousity.txt'
        real(real64):: viscousity, viscousity_sd
        integer(int32):: u_viscousity_mean

        open(newunit=u_viscousity_mean, file=file_viscousity_mean, status='replace')
            write(u_viscousity_mean, *) 'viscousity, viscousity_sd'
            write(u_viscousity_mean, *) viscousity, viscousity_sd
        close(u_viscousity_mean)
    end subroutine


    subroutine generate_vis_from_gk_vis()
        integer(int32),parameter:: fst_calc=3000, lst_calc=6000, all_calc=lst_calc-fst_calc+1
        integer(int32):: ndata
        real(real64),allocatable:: integ_stress_mean(:), integ_stress_sd(:), x(:)
        real(real64):: viscousity, viscousity_sd

        call input_condition(ndata)
        allocate(integ_stress_mean(ndata), integ_stress_sd(ndata), x(ndata))
        call read_integ_stress_mean(integ_stress_mean, integ_stress_sd, x, ndata)
        viscousity = mean(integ_stress_mean(fst_calc:lst_calc), sd=viscousity_sd)
        call output_stress_mean(viscousity, viscousity_sd)
        call output_integ_stress_mean_lm(integ_stress_mean, integ_stress_sd, x, lst_calc)
    end subroutine


    subroutine read_integ_stress_mean(integ_stress_mean, integ_stress_sd, x, ndata)
        character(100),parameter:: file_integ_stress_mean = 'GK_viscousity/integ_stress_mean.txt'
        integer(int32),intent(in):: ndata
        real(real64),intent(out):: integ_stress_mean(ndata),  x(ndata), integ_stress_sd(ndata)
        integer(int32):: u_integ_stress_mean, i

        open(newunit=u_integ_stress_mean, file=file_integ_stress_mean, status='old')
            read(u_integ_stress_mean, '(3e20.8)') (x(i), integ_stress_mean(i), integ_stress_sd(i), i=1,ndata/10)
        close(u_integ_stress_mean)
    end subroutine


    subroutine output_stress_mean(viscousity, viscousity_sd)
        character(100),parameter:: file_vis_stress_mean = 'viscousity/viscousity_stress_mean.txt'
        real(real64):: viscousity, viscousity_sd
        integer(int32):: u_vis_stress_mean

        open(newunit=u_vis_stress_mean, file=file_vis_stress_mean, status='replace')
            write(u_vis_stress_mean, *) 'viscousity, viscousity_sd'
            write(u_vis_stress_mean, *) viscousity, viscousity_sd
        close(u_vis_stress_mean)
    end subroutine

    subroutine output_integ_stress_mean_lm(integ_stress_mean, integ_stress_se, x, lst_calc)
        character(100),parameter:: file_integ_stress_mean = 'viscousity/integ_stress_mean_lm.txt'
        integer(int32),intent(in):: lst_calc
        real(real64),intent(in):: integ_stress_mean(:), integ_stress_se(:), x(:)
        integer(int32):: u_integ_stress_mean, i

        open(newunit=u_integ_stress_mean, file=file_integ_stress_mean, status='replace')
            write(u_integ_stress_mean, '(3e20.8)') (x(i), integ_stress_mean(i), integ_stress_se(i), i=1,lst_calc)
        close(u_integ_stress_mean)
    end subroutine
end program main