program main
    use,intrinsic :: iso_fortran_env
    use representative_value_mod
    implicit none
    integer(int32),parameter:: mo_len=30
    integer(int32):: fst_run, lst_run, run
    real(real64),allocatable:: x(:), mo_all(:,:,:), mo_mean(:,:), mo_se(:,:)

    read*, fst_run, lst_run
    allocate(mo_all(90, mo_len, fst_run:lst_run))
    allocate(mo_mean(90, mo_len), mo_se(90, mo_len))
    allocate(x(90))

    do run=fst_run, lst_run
        call read_mo(x, mo_all(:,:,run), run, mo_len)
    end do
    call calc_mo_mean(mo_all, mo_mean, mo_se)
    call output_gr_mean(x, mo_mean, mo_se, mo_len)
contains
    subroutine read_mo(x, mo, run, mo_len)
        integer(int32),intent(in):: run, mo_len
        real(real64),intent(out):: mo(:,:), x(:)
        character(100):: file_mo, crun
        integer(int32):: u_mo, i

        write(crun,'(I2.2)') run
        file_mo = '../calculation/run' // trim(crun) // '/Analysis/molecular_orientation/molecular_orientation.dat'
        open(newunit=u_mo, file=file_mo, status='old')
            do i=1,90
                read(u_mo,*) x(i), mo(i,:)
            end do
        close(u_mo)
    end subroutine


    subroutine calc_mo_mean(mo_all, mo_mean, mo_se)
        real(real64), intent(out):: mo_mean(:,:), mo_se(:,:)
        real(real64), intent(in):: mo_all(:,:,:)
        integer(int32):: i,j

        do i=lbound(mo_all,2), ubound(mo_all,2)
            do j=lbound(mo_all,1), ubound(mo_all,1)
                mo_mean(j,i) = mean(mo_all(j,i,:), se=mo_se(j,i))
            end do
        end do
    end subroutine


    subroutine output_gr_mean(x, mo_mean, mo_se, mo_len)
        character(100),parameter:: file_gr_mean = 'molecular_orientation/molecular_orientation_mean.txt'
        integer(int32),intent(in):: mo_len
        real(real64), intent(in):: x(:), mo_mean(:,:), mo_se(:,:)
        integer(int32):: u_mo_mean, i, j

        open(newunit=u_mo_mean, file=file_gr_mean, status='replace')
            do i=1,90
                write(u_mo_mean, *) x(i), (mo_mean(i,j), mo_se(i,j), j=1,mo_len)
            end do
        close(u_mo_mean)
    end subroutine
end program main