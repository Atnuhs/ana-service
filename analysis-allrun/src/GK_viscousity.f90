program main
    use,intrinsic :: iso_fortran_env
    use read_condition_mod
    use representative_value_mod
    implicit none
    integer(int32):: ndata
    integer(int32):: fst_run, lst_run, run
    real(real64),allocatable:: x(:), integ_stress_all(:,:), integ_stress_mean(:), integ_stress_se(:)

    read*, fst_run, lst_run
    call input_condition(ndata)
    allocate(integ_stress_all(ndata, fst_run:lst_run))
    allocate(integ_stress_mean(ndata), integ_stress_se(ndata))
    allocate(x(ndata))


    do run=fst_run, lst_run
        print*, 'read run =>', run
        call read_integ_stress(x, integ_stress_all(:,run), run, ndata)
    end do

    print*, 'calc'
    call calc_integ_stress_mean(integ_stress_all, integ_stress_mean, integ_stress_se)
    print*, 'output'
    call output_integ_stress_mean(x, integ_stress_mean, integ_stress_se, ndata)
    call output_integ_stress_all(x, integ_stress_all, ndata)
    print*, 'end'
contains
    subroutine input_condition(ndata)
        integer(int32),intent(out):: ndata
        type(condition_type):: condition

        call read_condition(condition)
        ndata = condition%nstep
    end subroutine


    subroutine read_integ_stress(x, viscousity, run, ndata)
        integer(int32),intent(in):: run, ndata
        real(real64),intent(out):: viscousity(:), x(:)
        character(100):: file_viscousity, crun
        integer(int32):: u_viscousity, i

        write(crun,'(I2.2)') run
        file_viscousity = '../calculation/run' // trim(crun) // '/Analysis/GK_viscousity/integ_acf_stress.dat'
        open(newunit=u_viscousity, file=file_viscousity, status='old')
            read(u_viscousity, *) (x(i), viscousity(i), i=1,ndata)
        close(u_viscousity)
    end subroutine


    subroutine calc_integ_stress_mean(integ_stress_all, integ_stress_mean, integ_stress_se)
        real(real64), intent(out):: integ_stress_mean(:), integ_stress_se(:)
        real(real64), intent(in):: integ_stress_all(:,:)
        integer(int32):: i

        do i=lbound(integ_stress_all,1), ubound(integ_stress_all,1)
            integ_stress_mean(i) = mean(integ_stress_all(i,:), se=integ_stress_se(i))
        end do
    end subroutine


    subroutine output_integ_stress_mean(x, integ_stress_mean, integ_stress_se, ndata)
        character(100),parameter:: file_viscousity_mean = 'GK_viscousity/integ_stress_mean.txt'
        integer(int32),intent(in):: ndata
        real(real64), intent(in):: x(:), integ_stress_mean(:), integ_stress_se(:)
        integer(int32):: u_viscousity_mean, i

        open(newunit=u_viscousity_mean, file=file_viscousity_mean, status='replace')
            write(u_viscousity_mean, '(3e20.8)') (x(i), integ_stress_mean(i), integ_stress_se(i), i=1,ndata/10)
        close(u_viscousity_mean)
    end subroutine


    subroutine output_integ_stress_all(x, integ_stress_all, ndata)
        character(100),parameter:: file_viscousity_all = 'GK_viscousity/integ_stress_all.txt'
        integer(int32),intent(in):: ndata
        real(real64), intent(in):: x(:), integ_stress_all(:,:)
        integer(int32):: u_viscousity_all, i

        open(newunit=u_viscousity_all, file=file_viscousity_all, status='replace')
            do i=1,ndata/10
                if (mod(i,10000)==0) print*, 'output stress all => ', i, '/', ndata/10
                write(u_viscousity_all, '(*(e20.8))') x(i), integ_stress_all(i,:)
            end do
        close(u_viscousity_all)
    end subroutine
end program main