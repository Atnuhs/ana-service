program main
    use,intrinsic :: iso_fortran_env
    use read_condition_mod
    implicit none
    integer(int32),parameter:: gr_len=10000
    integer(int32):: fst_run, lst_run, run
    real(real64),allocatable:: x(:), gr_all(:,:), gr_mean(:)

    read*, fst_run, lst_run
    allocate(gr_all(gr_len, fst_run:lst_run))
    allocate(x(gr_len))

    do run=fst_run, lst_run
        call read_gr(x, gr_all(:,run), run, gr_len)
    end do

    call output_gr_all(x, gr, gr_len)
    call output_gr_mean(x, gr, gr_len)
contains
    subroutine read_gr(x, gr, run, gr_len)
        integer(int32),intent(in):: run, gr_len
        real(real64),intent(out):: gr(:), x(:)
        character(100):: file_gr, crun
        integer(int32):: u_gr, i

        write(crun,'(I2.2)') run
        file_gr = '../calculation/run' // trim(crun) // '/Analysis/temp/temp_mean.dat'
        open(newunit=u_gr, file=file_gr, status='old')
            read(u_gr, *) (x(i), gr(i), i=1,gr_len)
        close(u_gr)
    end subroutine

    subroutine output_gr_all(x, gr, gr_len)
        character(100),parameter:: file_gr_all = 'gr/gr_all.txt'
        integer(int32),intent(in):: gr_len
        real(real64):: x(:), gr(:,:)
        integer(int32):: u_gr_all

        open(newunit=u_gr_all, file=file_gr_all, status='replace')
            read(u_gr_all, '(2e20.8)') (x(i), gr(i,:), i=1,gr_all)
        close(u_gr_all)
    end subroutine


    subroutine output_gr_mean(x, gr, gr_len)
        character(100),parameter:: file_gr_mean = 'gr/gr_mean.txt'
        integer(int32),intent(in):: gr_len
        real(real64):: x(:), gr(:,:)
        integer(int32):: u_gr_mean

        open(newunit=u_gr_mean, file=file_gr_mean, status='replace')
            read(u_gr_mean, '(2e20.8)') (x(i), sum(gr(i,:)), i=1,gr_mean)
        close(u_gr_mean)
    end subroutine
end program main