
program main
    use,intrinsic :: iso_fortran_env
    use md_condition_for_ana_mod
    use representative_value_mod
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
        real(real64):: dt, tdc, temp0, temp, cell, viscousity
        real(real64), allocatable:: dxyz(:,:,:)
        real(real64), allocatable:: msd(:), msd_sum(:), msd2_sum(:)
        real(real64), allocatable:: msd_sum_global(:), msd2_sum_global(:)
        real(real64), allocatable:: x(:), msd_mean(:), msd_stdev(:)
        real(real64):: tdc_sum, tdc2_sum, tdc_sum_global, tdc2_sum_global
        real(real64):: tdc_mean, tdc_stdev
    
        if (rank==0) read*, fst_run, lst_run
        all_run = lst_run-fst_run+1

        call mpi_bcast(fst_run, 1, mpi_integer, 0, mpi_comm_world, ierr)
        call mpi_bcast(lst_run, 1, mpi_integer, 0, mpi_comm_world, ierr)

        ! インプットの読み込みなどの準備
        call load_condition_for_msd_ana(ndata, np, dt, intd, temp0, cell)
        allocate(dxyz(ndata, np, 3))
        allocate(msd(ndata), msd_sum(ndata), msd2_sum(ndata))
        allocate(msd_sum_global(ndata), msd2_sum_global(ndata))
        allocate(msd_mean(ndata), msd_stdev(ndata))
        allocate(x(ndata))
        call init_msd(ndata)

        msd_sum(:) = 0d0
        msd2_sum(:) = 0d0
        tdc_sum = 0d0
        tdc2_sum = 0d0
        x = [((i-1)*(dt*intd), i=1,ndata)]


        do i=fst_run+rank, lst_run, procs
            ! msd
            print*, 'reading ... ', '../calculation/'//rundir(i)//'/dxyz.dat', rank
            call read_dxyz('../calculation/'//rundir(i)//'/dxyz.dat', ndata, np, dxyz)
            call calc_msd(msd, dxyz, ndata, np)
            msd_sum(:) = msd_sum(:) + msd(:)
            msd2_sum(:) = msd2_sum(:) + msd(:)*msd(:)

            ! translational_diffusion_coefficient
            call read_x('./temp/temp_'//rundir(i)//'.dat', temp)
            call read_x('./viscousity/viscousity.txt', viscousity)
            call calc_tdc(ndata, x, msd, temp, cell, viscousity, tdc)
            tdc = tdc * (temp/temp0)    
            tdc_sum = tdc_sum + tdc
            tdc2_sum = tdc2_sum + tdc*tdc
        end do
        
        call mpi_reduce(msd_sum, msd_sum_global, ndata, mpi_double_precision, mpi_sum, 0, mpi_comm_world, ierr)
        call mpi_reduce(msd2_sum, msd2_sum_global, ndata, mpi_double_precision, mpi_sum, 0, mpi_comm_world, ierr)
        call mpi_reduce(tdc_sum, tdc_sum_global, 1, mpi_double_precision, mpi_sum, 0, mpi_comm_world, ierr)
        call mpi_reduce(tdc2_sum, tdc2_sum_global, 1, mpi_double_precision, mpi_sum, 0, mpi_comm_world, ierr)

        if (rank==0) then
            ! msd
            do i=1,ndata
                msd_mean(i) = mean_from_sum(msd_sum_global(i), msd2_sum_global(i), dble(all_run), stdev=msd_stdev(i))
            end do
            call write_arx_ary_arysd('msd/msd.txt', ndata, x, msd_mean, msd_stdev)


            ! translational_diffusion_coefficient
            tdc_mean = mean_from_sum(tdc_sum_global, tdc2_sum_global, dble(all_run), stdev=tdc_stdev)
            call write_x_xsd('msd/tdc.txt', tdc_mean, tdc_stdev)
        end if
        
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


    subroutine calc_tdc(ndata, x, msd_mean, temp, cell, viscousity, tdc)
        real(real64),parameter:: zeta = 2.837297d0
        integer(int32),intent(in):: ndata
        real(real64),intent(in):: x(ndata), msd_mean(ndata), temp, cell, viscousity
        real(real64),intent(out):: tdc
        integer(int32):: l,r,n,i
        real(real64):: a, tdc_pbc

        l = ndata/10
        r = ndata/4
        n = r-l+1
        call least_squares_method(len_arr=n, arx=x(l:r), ary=msd_mean(l:r), a=a)
        tdc_pbc = a / 6d0
        tdc = tdc_pbc + (kbt*temp*zeta) / (6d0*pi*cell*viscousity) ! セルサイズ補正
    end subroutine
end program
