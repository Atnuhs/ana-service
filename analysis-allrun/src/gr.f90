program main
    use,intrinsic :: iso_fortran_env
    use representative_value_mod
    implicit none
    integer(int32),parameter:: gr_len=1000
    integer(int32):: fst_run, lst_run, run
    real(real64),allocatable:: x(:), gr_all(:,:), gr_mean(:), gr_se(:)

    read*, fst_run, lst_run
    allocate(gr_all(gr_len, fst_run:lst_run))
    allocate(gr_mean(gr_len), gr_se(gr_len))
    allocate(x(gr_len))

    do run=fst_run, lst_run
        call read_gr(x, gr_all(:,run), run, gr_len)
    end do
    call calc_gr_mean(gr_all, gr_mean, gr_se)
    call output_gr_all(x, gr_all, gr_len)
    call output_gr_mean(x, gr_mean, gr_se, gr_len)
contains
    subroutine read_gr(x, gr, run, gr_len)
        integer(int32),intent(in):: run, gr_len
        real(real64),intent(out):: gr(:), x(:)
        character(100):: file_gr, crun
        integer(int32):: u_gr, i

        write(crun,'(I2.2)') run
        file_gr = '../calculation/run' // trim(crun) // '/Analysis/gr/gr.dat'
        open(newunit=u_gr, file=file_gr, status='old')
            read(u_gr, *) (x(i), gr(i), i=1,gr_len)
        close(u_gr)
    end subroutine


    subroutine calc_gr_mean(gr_all, gr_mean, gr_se)
        real(real64), intent(out):: gr_mean(:), gr_se(:)
        real(real64), intent(in):: gr_all(:,:)
        integer(int32):: i

        do i=lbound(gr_all,1), ubound(gr_all,1)
            gr_mean(i) = mean(gr_all(i,:), se=gr_se(i))
        end do
    end subroutine



    subroutine output_gr_all(x, gr_all, gr_len)
        character(100),parameter:: file_gr_all = 'gr/gr_all.txt'
        integer(int32),intent(in):: gr_len
        real(real64):: x(:), gr_all(:,:)
        integer(int32):: u_gr_all, i

        open(newunit=u_gr_all, file=file_gr_all, status='replace')
            do i=1, gr_len
                write(u_gr_all, '(100e20.8)') x(i), gr_all(i,:)
            end do
        close(u_gr_all)
    end subroutine


    subroutine output_gr_mean(x, gr_mean, gr_se, gr_len)
        character(100),parameter:: file_gr_mean = 'gr/gr_mean.txt'
        integer(int32),intent(in):: gr_len
        real(real64), intent(in):: x(:), gr_mean(:), gr_se(:)
        integer(int32):: u_gr_mean, i

        open(newunit=u_gr_mean, file=file_gr_mean, status='replace')
            write(u_gr_mean, '(3e20.8)') (x(i), gr_mean(i), gr_se(i), i=1,gr_len)
        close(u_gr_mean)
    end subroutine
end program main