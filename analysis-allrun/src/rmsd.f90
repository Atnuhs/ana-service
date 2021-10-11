program main
    use,intrinsic :: iso_fortran_env
    use read_condition_mod
    use representative_value_mod
    implicit none
    integer(int32):: ndata
    integer(int32):: fst_run, lst_run, run
    real(real64),allocatable:: x(:), rmsd_all(:,:), rmsd_mean(:), rmsd_sd(:)
    real(real64),allocatable:: rdc_all(:)
    real(real64):: rdc_mean, rdc_sd

    read*, fst_run, lst_run
    call input_condition(ndata)
    allocate(rmsd_all(ndata, fst_run:lst_run))
    allocate(rdc_all(fst_run:lst_run))
    allocate(rmsd_mean(ndata), rmsd_sd(ndata))
    allocate(x(ndata))

    do run=fst_run, lst_run
        call read_rmsd(x=x, rmsd=rmsd_all(:,run), run=run, ndata=ndata)
        call read_rdc(rdc=rdc_all(run))
    end do
    call calc_rmsd_mean(rmsd_all=rmsd_all, rmsd_mean=rmsd_mean, rmsd_sd=rmsd_sd)
    call calc_rdc_mean(rdc_all=rdc_all, rdc_mean=rdc_mean, rdc_sd=rdc_sd)
    call output_rmsd_all(x, rmsd_all, ndata)
    call output_rmsd_mean(x, rmsd_mean, rmsd_sd, ndata)
    call output_rdc_mean(rdc_mean, rdc_sd)
contains
    subroutine input_condition(ndata)
        integer(int32),intent(out):: ndata
        type(condition_type):: condition

        call read_condition(condition)
        ndata = condition%nstep/condition%intd
    end subroutine


    subroutine read_rmsd(x, rmsd, run, ndata)
        integer(int32),intent(in):: run, ndata
        real(real64),intent(out):: rmsd(:), x(:)
        character(100):: file_rmsd, crun
        integer(int32):: u_rmsd, i
        real(real64):: se

        write(crun,'(I2.2)') run
        file_rmsd = '../calculation/run' // trim(crun) // '/Analysis/rmsd/mean_rmsd.dat'
        open(newunit=u_rmsd, file=file_rmsd, status='old')
            read(u_rmsd, *) (x(i), rmsd(i), se, i=1,ndata)
        close(u_rmsd)
    end subroutine


    subroutine read_rdc(rdc)
        real(real64),intent(out):: rdc
        character(100):: file_rdc, crun
        integer(int32):: u_rdc

        write(crun,'(I2.2)') run
        file_rdc = '../calculation/run' // trim(crun) // '/Analysis/rmsd/rotational_diffusion_coefficient.dat'
        open(newunit=u_rdc, file=file_rdc, status='old')
            read(u_rdc, *) rdc
        close(u_rdc)
    end subroutine


    subroutine calc_rmsd_mean(rmsd_all, rmsd_mean, rmsd_sd)
        real(real64), intent(out):: rmsd_mean(:), rmsd_sd(:)
        real(real64), intent(in):: rmsd_all(:,:)
        integer(int32):: i

        do i=lbound(rmsd_all,1), ubound(rmsd_all,1)
            rmsd_mean(i) = mean(rmsd_all(i,:), sd=rmsd_sd(i))
        end do
    end subroutine


    subroutine calc_rdc_mean(rdc_all, rdc_mean, rdc_sd)
        real(real64):: rdc_mean, rdc_sd
        real(real64):: rdc_all(:)

        rdc_mean = mean(rdc_all(:), sd=rdc_sd)
    end subroutine


    subroutine output_rmsd_all(x, rmsd_all, ndata)
        character(100),parameter:: file_rmsd_all = 'rmsd/rmsd_all.txt'
        integer(int32),intent(in):: ndata
        real(real64):: x(:), rmsd_all(:,:)
        integer(int32):: u_rmsd_all, i

        open(newunit=u_rmsd_all, file=file_rmsd_all, status='replace')
            do i=1, ndata
                write(u_rmsd_all, '(100e20.8)') x(i), rmsd_all(i,:)
            end do
        close(u_rmsd_all)
    end subroutine


    subroutine output_rmsd_mean(x, rmsd_mean, rmsd_sd, ndata)
        character(100),parameter:: file_rmsd_mean = 'rmsd/rmsd_mean.txt'
        integer(int32),intent(in):: ndata
        real(real64), intent(in):: x(:), rmsd_mean(:), rmsd_sd(:)
        integer(int32):: u_rmsd_mean, i

        open(newunit=u_rmsd_mean, file=file_rmsd_mean, status='replace')
            write(u_rmsd_mean, '(3e20.8)') (x(i), rmsd_mean(i), rmsd_sd(i), i=1,ndata)
        close(u_rmsd_mean)
    end subroutine


    subroutine output_rdc_mean(rdc_mean, rdc_sd)
        character(100),parameter:: file_rmsd_mean = 'rmsd/rdc_mean.txt'
        real(real64), intent(in)::rdc_mean, rdc_sd
        integer(int32):: u_rdc_mean

        open(newunit=u_rdc_mean, file=file_rmsd_mean, status='replace')
            write(u_rdc_mean, '(2e20.8)') rdc_mean, rdc_sd
        close(u_rdc_mean)
    end subroutine
end program main