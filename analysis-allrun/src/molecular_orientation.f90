program main
    use,intrinsic :: iso_fortran_env
    use representative_value_mod
    implicit none
    integer(int32),parameter:: mo_len=100
    integer(int32):: fst_run, lst_run, run
    real(real64),allocatable:: x(:), mo1_all(:,:,:), mo1_mean(:,:), mo1_sd(:,:)
    real(real64),allocatable:: mo2_all(:,:,:), mo2_mean(:,:), mo2_sd(:,:)

    read*, fst_run, lst_run
    allocate(mo1_all(90, mo_len, fst_run:lst_run))
    allocate(mo1_mean(90, mo_len), mo1_sd(90, mo_len))
    allocate(mo2_all(90, mo_len, fst_run:lst_run))
    allocate(mo2_mean(90, mo_len), mo2_sd(90, mo_len))
    allocate(x(90))

    do run=fst_run, lst_run
        call read_mo(x, mo1_all(:,:,run), mo2_all(:,:,run), run)
    end do
    call calc_mo_mean(mo1_all, mo1_mean, mo1_sd)
    call calc_mo_mean(mo2_all, mo2_mean, mo2_sd)
    call output_gr_mean(x, mo1_mean, mo1_sd, mo2_mean, mo2_sd, mo_len)
contains
    subroutine read_mo(x, mo1, mo2, run)
        integer(int32),intent(in):: run
        real(real64),intent(out):: mo1(:,:), mo2(:,:), x(:)
        character(100):: file_mo, crun
        integer(int32):: u_mo, i

        write(crun,'(I2.2)') run

        file_mo = '../calculation/run' // trim(crun) // '/Analysis/molecular_orientation/molecular_orientation.dat'
        open(newunit=u_mo, file=file_mo, status='old')
            do i=1,90
                read(u_mo,*) x(i), mo1(i,:)
            end do
        close(u_mo)

        file_mo = '../calculation/run' // trim(crun) // '/Analysis/molecular_orientation/molecular_orientation2.dat'
        open(newunit=u_mo, file=file_mo, status='old')
            do i=1,90
                read(u_mo,*) x(i), mo2(i,:)
            end do
        close(u_mo)
    end subroutine


    subroutine calc_mo_mean(mo_all, mo_mean, mo_sd)
        real(real64), intent(out):: mo_mean(:,:), mo_sd(:,:)
        real(real64), intent(in):: mo_all(:,:,:)
        integer(int32):: i,j

        do i=lbound(mo_all,2), ubound(mo_all,2)
            do j=lbound(mo_all,1), ubound(mo_all,1)
                mo_mean(j,i) = mean(mo_all(j,i,:), sd=mo_sd(j,i))
            end do
        end do
    end subroutine


    subroutine output_gr_mean(x, mo1_mean, mo1_sd, mo2_mean, mo2_sd, mo_len)
        character(100),parameter:: file_mo1_mean = 'molecular_orientation/molecular_orientation_mean.txt'
        character(100),parameter:: file_mo2_mean = 'molecular_orientation/molecular_orientation2_mean.txt'
        integer(int32),intent(in):: mo_len
        real(real64), intent(in):: x(:), mo1_mean(:,:), mo1_sd(:,:), mo2_mean(:,:), mo2_sd(:,:)
        integer(int32):: u_mo_mean, i, j

        open(newunit=u_mo_mean, file=file_mo1_mean, status='replace')
            do i=1,90
                do j=1,mo_len
                    write(u_mo_mean, *) x(i), j, mo1_mean(i,j), mo1_sd(i,j)
                end do
            end do
        close(u_mo_mean)

        open(newunit=u_mo_mean, file=file_mo2_mean, status='replace')
            do i=1,90
                do j=1,mo_len
                    write(u_mo_mean, *) x(i), j, mo2_mean(i,j), mo2_sd(i,j)
                end do
            end do
        close(u_mo_mean)
    end subroutine
end program main