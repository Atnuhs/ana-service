program main
    use,intrinsic :: iso_fortran_env
    use read_condition_mod
    use representative_value_mod
    implicit none
    integer(int32):: ndata
    integer(int32):: fst_run, lst_run, run
    integer(int32):: fst_calc, lst_calc
    real(real64),allocatable:: x(:), integ_ej_all(:,:), integ_ej_mean(:), integ_ej_se(:)
    real(real64),allocatable:: thcd_run(:)
    real(real64):: thcd, thcd_sd

    read*, fst_run, lst_run
    call input_condition(ndata)
    allocate(integ_ej_all(ndata, fst_run:lst_run))
    allocate(integ_ej_mean(ndata), integ_ej_se(ndata))
    allocate(x(ndata))
    allocate(thcd_run(fst_run:lst_run))


    do run=fst_run, lst_run
        print*, 'read run =>', run
        call read_integ_ej(x, integ_ej_all(:,run), run, ndata)
        call read_thcd_run(thcd_run(run))
    end do

    print*, 'calc'
    call calc_integ_ej_mean(integ_ej_all, integ_ej_mean, integ_ej_se)
    call calc_thcd(thcd_run, thcd, thcd_sd, fst_run, lst_run)

    print*, 'output'
    call output_integ_ej_mean(x, integ_ej_mean, integ_ej_se, ndata)
    call output_integ_ej_all(x, integ_ej_all, ndata)
    call output_thcd(thcd, thcd_sd)

    print*, 'end'
contains
    subroutine input_condition(ndata)
        integer(int32),intent(out):: ndata
        type(condition_type):: condition

        call read_condition(condition)
        ndata = condition%nstep
    end subroutine


    subroutine read_integ_ej(x, thcd, run, ndata)
        integer(int32),intent(in):: run, ndata
        real(real64),intent(out):: thcd(:), x(:)
        character(100):: file_thcd, crun
        integer(int32):: u_thcd, i

        write(crun,'(I2.2)') run
        file_thcd = '../calculation/run' // trim(crun) // '/Analysis/thcd/integ_acf_ej.dat'
        open(newunit=u_thcd, file=file_thcd, status='old')
            read(u_thcd, *) (x(i), thcd(i), i=1,ndata)
        close(u_thcd)
    end subroutine


    subroutine read_thcd_run(thcd)
        real(real64),intent(out):: thcd
        character(100):: file_thcd, crun
        integer(int32):: u_thcd, i

        write(crun,'(I2.2)') run
        file_thcd = '../calculation/run' // trim(crun) // '/Analysis/thcd/thcd.dat'
        open(newunit=u_thcd, file=file_thcd, status='old')
            read(u_thcd, *) thcd
        close(u_thcd)
    end subroutine


    subroutine calc_integ_ej_mean(integ_ej_all, integ_ej_mean, integ_ej_se)
        real(real64), intent(out):: integ_ej_mean(:), integ_ej_se(:)
        real(real64), intent(in):: integ_ej_all(:,:)
        integer(int32):: i

        do i=lbound(integ_ej_all,1), ubound(integ_ej_all,1)
            integ_ej_mean(i) = mean(integ_ej_all(i,:), se=integ_ej_se(i))
        end do
    end subroutine


    subroutine calc_thcd(thcd_run, thcd, thcd_sd, fst_run, lst_run)
        real(real64),intent(out):: thcd, thcd_sd
        integer(int32),intent(in):: fst_run, lst_run
        real(real64),intent(in):: thcd_run(fst_run:lst_run)
        
        thcd = mean(thcd_run(fst_run:lst_run), sd=thcd_sd)
    end subroutine


    subroutine output_integ_ej_mean(x, integ_ej_mean, integ_ej_se, ndata)
        character(100),parameter:: file_thcd_mean = 'thcd/integ_ej_mean.txt'
        integer(int32),intent(in):: ndata
        real(real64), intent(in):: x(:), integ_ej_mean(:), integ_ej_se(:)
        integer(int32):: u_thcd_mean, i

        open(newunit=u_thcd_mean, file=file_thcd_mean, status='replace')
            write(u_thcd_mean, '(3e20.8)') (x(i), integ_ej_mean(i), integ_ej_se(i), i=1,ndata/10)
        close(u_thcd_mean)
    end subroutine


    subroutine output_integ_ej_all(x, integ_ej_all, ndata)
        character(100),parameter:: file_thcd_all = 'thcd/integ_ej_all.txt'
        integer(int32),intent(in):: ndata
        real(real64), intent(in):: x(:), integ_ej_all(:,:)
        integer(int32):: u_thcd_all, i

        open(newunit=u_thcd_all, file=file_thcd_all, status='replace')
            do i=1,ndata/10
                if (mod(i,10000)==0) print*, 'output ej all => ', i, '/', ndata/10
                write(u_thcd_all, '(*(e20.8))') x(i), integ_ej_all(i,:)
            end do
        close(u_thcd_all)
    end subroutine


    subroutine output_thcd(thcd, thcd_sd)
        character(100),parameter:: file_thcd_mean = 'thcd/thcd.txt'
        real(real64):: thcd, thcd_sd
        integer(int32):: u_thcd_mean, i

        open(newunit=u_thcd_mean, file=file_thcd_mean, status='replace')
            write(u_thcd_mean, *) 'thcd, thcd_sd'
            write(u_thcd_mean, *) thcd, thcd_sd
        close(u_thcd_mean)
    end subroutine
end program main