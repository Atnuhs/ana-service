
program main
    use,intrinsic :: iso_fortran_env
    use md_condition_for_ana_mod
    use io_file_mod
    use mean_square_displacement_mod
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
        real(real64), allocatable:: dxyz(:,:,:)
        real(real64), allocatable:: msd(:)
        real(real64), allocatable:: x(:)
    
        if (rank==0) read*, fst_run, lst_run
        all_run = lst_run-fst_run+1

        call mpi_bcast(fst_run, 1, mpi_integer, 0, mpi_comm_world, ierr)
        call mpi_bcast(lst_run, 1, mpi_integer, 0, mpi_comm_world, ierr)

        ! インプットの読み込みなどの準備
        call load_condition_for_msd_ana(ndata, np, dt, intd)
        allocate(dxyz(ndata, np, 3))
        allocate(msd(ndata))
        allocate(x(ndata))
        call init_msd(ndata)

        x = [((i-1)*(dt*intd), i=1,ndata)]


        do i=fst_run+rank, lst_run, procs
            print*, 'reading ... ', '../calculation/'//rundir(i)//'/dxyz.dat', rank
            call read_dxyz('../calculation/'//rundir(i)//'/dxyz.dat', ndata, np, dxyz)
            call calc_msd(msd, dxyz, ndata, np)
            call write_arx_ary('msd/msd_'//rundir(i)//'.dat', ndata, x, msd)
        end do
    end subroutine

    
    subroutine calc_msd(msd, dxyz, ndata, np)
        ! MSDを自己相関関数、累積和を用いて計算O(N)
        real(real64),intent(out):: msd(:)
        real(real64),intent(in):: dxyz(:,:,:)
        integer(int32),intent(in):: ndata, np
        integer(int32):: i,j

        msd(:) = 0d0
        do i=1,3
            do j=1,np
                msd(:) = msd(:) + calc_mean_square_displacement(ndata, dxyz(:,j,i))
            end do
        end do
        msd(:) = msd(:) / dble(np)
    end subroutine
end program
