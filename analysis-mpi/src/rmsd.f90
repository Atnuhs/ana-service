
program main
    use,intrinsic :: iso_fortran_env
    use md_condition_for_ana_mod
    use mean_square_displacement_mod
    use io_file_mod
    implicit none
    include 'mpif.h'
    integer(int32):: procs, rank, ierr
    ! prcos:: プロセス数 rank:: 自分のプロセスid(0-indexed)

    call mpi_init(ierr)
    call mpi_comm_size(mpi_comm_world, procs, ierr)
    call mpi_comm_rank(mpi_comm_world, rank, ierr)
    call main_routine()
    call mpi_finalize(ierr)
contains
    subroutine main_routine()
        integer(int32):: ndata, np, intd, i
        integer(int32):: fst_run, lst_run, all_run
        real(real64):: dt
        real(real64), allocatable:: tdphi(:,:,:)
        real(real64), allocatable:: rmsd(:)
        real(real64), allocatable:: x(:)
    
        if (rank==0) read*, fst_run, lst_run
        all_run = lst_run-fst_run+1

        call mpi_bcast(fst_run, 1, mpi_integer, 0, mpi_comm_world, ierr)
        call mpi_bcast(lst_run, 1, mpi_integer, 0, mpi_comm_world, ierr)

        ! インプットの読み込みなどの準備
        call load_condition_for_rmsd_ana(ndata, np, dt, intd)
        allocate(tdphi(ndata, np, 3))
        allocate(rmsd(ndata))
        allocate(x(ndata))
        call init_msd(ndata)

        x = [((i-1)*(dt*intd), i=1,ndata)]

        do i=fst_run+rank, lst_run, procs
            ! rmsd
            print*, 'reading ... ', '../calculation/'//rundir(i)//'/tdphi.dat', rank
            call read_tdphi('../calculation/'//rundir(i)//'/tdphi.dat', ndata, np, tdphi)
            call calc_rmsd(rmsd, tdphi, ndata, np)
            call write_arx_ary('rmsd/rmsd_'//rundir(i)//'.txt', ndata, x, rmsd)
        end do
    end subroutine

    
    subroutine calc_rmsd(rmsd, tdphi, ndata, np)
        ! rmsdを自己相関関数、累積和を用いて計算O(N)
        real(real64),intent(out):: rmsd(:)
        real(real64),intent(in):: tdphi(:,:,:)
        integer(int32),intent(in):: ndata, np
        integer(int32):: i,j

        rmsd(:) = 0d0
        do i=1,3
            do j=1,np
                rmsd(:) = rmsd(:) + calc_mean_square_displacement(ndata, tdphi(:,j,i))
            end do
        end do
        rmsd(:) = rmsd(:) / dble(np)
    end subroutine
end program
