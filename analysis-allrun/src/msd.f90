program main
    use,intrinsic :: iso_fortran_env
    use read_condition_mod
    use representative_value_mod
    implicit none
    integer(int32):: ndata
    integer(int32):: fst_run, lst_run, run
    real(real64),allocatable:: x(:), msd_all(:,:), msd_mean(:), msd_sd(:)
    real(real64),allocatable:: tdc_all(:)
    real(real64):: tdc_mean, tdc_sd

    read*, fst_run, lst_run
    call input_condition(ndata)
    allocate(msd_all(ndata, fst_run:lst_run))
    allocate(tdc_all(fst_run:lst_run))
    allocate(msd_mean(ndata), msd_sd(ndata))
    allocate(x(ndata))

    do run=fst_run, lst_run
        call read_msd(x=x, msd=msd_all(:,run), run=run, ndata=ndata)
        call read_tdc(tdc=tdc_all(run))
    end do
    call calc_msd_mean(msd_all=msd_all, msd_mean=msd_mean, msd_sd=msd_sd)
    call calc_tdc_mean(tdc_all=tdc_all, tdc_mean=tdc_mean, tdc_sd=tdc_sd)
    call output_msd_all(x, msd_all, ndata)
    call output_msd_mean(x, msd_mean, msd_sd, ndata)
    call output_tdc_mean(tdc_mean, tdc_sd)
contains
    subroutine input_condition(ndata)
        integer(int32),intent(out):: ndata
        type(condition_type):: condition

        call read_condition(condition)
        ndata = condition%nstep/condition%intd
    end subroutine


    subroutine read_msd(x, msd, run, ndata)
        integer(int32),intent(in):: run, ndata
        real(real64),intent(out):: msd(:), x(:)
        character(100):: file_msd, crun
        integer(int32):: u_msd, i
        real(real64):: se

        write(crun,'(I2.2)') run
        file_msd = '../calculation/run' // trim(crun) // '/Analysis/msd/mean_msd.dat'
        open(newunit=u_msd, file=file_msd, status='old')
            read(u_msd, *) (x(i), msd(i), se, i=1,ndata)
        close(u_msd)
    end subroutine


    subroutine read_tdc(tdc)
        real(real64),intent(out):: tdc
        character(100):: file_tdc, crun
        integer(int32):: u_tdc

        write(crun,'(I2.2)') run
        file_tdc = '../calculation/run' // trim(crun) // '/Analysis/msd/translational_diffusion_coefficient.dat'
        open(newunit=u_tdc, file=file_tdc, status='old')
            read(u_tdc, *) tdc
        close(u_tdc)
    end subroutine


    subroutine calc_msd_mean(msd_all, msd_mean, msd_sd)
        real(real64), intent(out):: msd_mean(:), msd_sd(:)
        real(real64), intent(in):: msd_all(:,:)
        integer(int32):: i

        do i=lbound(msd_all,1), ubound(msd_all,1)
            msd_mean(i) = mean(msd_all(i,:), sd=msd_sd(i))
        end do
    end subroutine


    subroutine calc_tdc_mean(tdc_all, tdc_mean, tdc_sd)
        real(real64):: tdc_mean, tdc_sd
        real(real64):: tdc_all(:)

        tdc_mean = mean(tdc_all(:), sd=tdc_sd)
    end subroutine


    subroutine output_msd_all(x, msd_all, ndata)
        character(100),parameter:: file_msd_all = 'msd/msd_all.txt'
        integer(int32),intent(in):: ndata
        real(real64):: x(:), msd_all(:,:)
        integer(int32):: u_msd_all, i

        open(newunit=u_msd_all, file=file_msd_all, status='replace')
            do i=1, ndata
                write(u_msd_all, '(100e20.8)') x(i), msd_all(i,:)
            end do
        close(u_msd_all)
    end subroutine


    subroutine output_msd_mean(x, msd_mean, msd_sd, ndata)
        character(100),parameter:: file_msd_mean = 'msd/msd_mean.txt'
        integer(int32),intent(in):: ndata
        real(real64), intent(in):: x(:), msd_mean(:), msd_sd(:)
        integer(int32):: u_msd_mean, i

        open(newunit=u_msd_mean, file=file_msd_mean, status='replace')
            write(u_msd_mean, '(3e20.8)') (x(i), msd_mean(i), msd_sd(i), i=1,ndata)
        close(u_msd_mean)
    end subroutine


    subroutine output_tdc_mean(tdc_mean, tdc_sd)
        character(100),parameter:: file_msd_mean = 'msd/tdc_mean.txt'
        real(real64), intent(in)::tdc_mean, tdc_sd
        integer(int32):: u_tdc_mean

        open(newunit=u_tdc_mean, file=file_msd_mean, status='replace')
            write(u_tdc_mean, '(2e20.8)') tdc_mean, tdc_sd
        close(u_tdc_mean)
    end subroutine
end program main