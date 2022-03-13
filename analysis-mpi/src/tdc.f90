
program main
    use,intrinsic :: iso_fortran_env
    use md_condition_for_ana_mod
    use representative_value_mod
    use io_file_mod
    implicit none
    include 'mpif.h'
    integer(int32):: procs, rank, ierr 
    integer(int32):: ndata
    integer(int32):: fst_calc, lst_calc
    real(real64):: tdc, temp, viscousity
    real(real64):: temp0, cell
    real(real64), allocatable:: x(:), msd(:)
    ! prcos:: プロセス数 rank:: 自分のプロセスid(0-indexed)

    call mpi_init(ierr)
    call mpi_comm_size(mpi_comm_world, procs, ierr)
    call mpi_comm_rank(mpi_comm_world, rank, ierr)
    call main_routine()
    call mpi_finalize(ierr)
contains
    subroutine main_routine()
        integer(int32)::  i
        integer(int32):: fst_run, lst_run, all_run
        real(real64), allocatable:: msd_sum(:), msd2_sum(:)
        real(real64), allocatable:: msd_sum_global(:), msd2_sum_global(:)
        real(real64), allocatable:: msd_mean(:), msd_stdev(:)
        real(real64):: tdc_sum, tdc2_sum, tdc_sum_global, tdc2_sum_global
        real(real64):: tdc_mean, tdc_stdev
    
        if (rank==0) read*, fst_run, lst_run, fst_calc, lst_calc
        all_run = lst_run-fst_run+1

        call mpi_bcast(fst_run, 1, mpi_integer, 0, mpi_comm_world, ierr)
        call mpi_bcast(lst_run, 1, mpi_integer, 0, mpi_comm_world, ierr)
        call mpi_bcast(fst_calc, 1, mpi_integer, 0, mpi_comm_world, ierr)
        call mpi_bcast(lst_calc, 1, mpi_integer, 0, mpi_comm_world, ierr)
        

        ! インプットの読み込みなどの準備
        call load_condition_for_tdc_ana(ndata, temp0, cell)
        allocate(msd(ndata), msd_sum(ndata), msd2_sum(ndata))
        allocate(msd_sum_global(ndata), msd2_sum_global(ndata))
        allocate(msd_mean(ndata), msd_stdev(ndata))
        allocate(x(ndata))

        msd_sum(:) = 0d0
        msd2_sum(:) = 0d0
        tdc_sum = 0d0
        tdc2_sum = 0d0

        do i=fst_run+rank, lst_run, procs
            print*, 'reading ... ', 'msd/msd_'//rundir(i)//'.dat', rank
            call read_arx_ary('msd/msd_'//rundir(i)//'.dat', ndata, x, msd)
            msd_sum(:) = msd_sum(:) + msd(:)
            msd2_sum(:) = msd2_sum(:) + msd(:)*msd(:)

            ! translational_diffusion_coefficient
            call read_x('./temp/temp_'//rundir(i)//'.dat', temp)
            call read_x('./viscousity/viscousity.txt', viscousity)
            tdc = calc_tdc()
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
            call write_arx_ary_arysd('tdc/msd_mean.txt', ndata, x, msd_mean, msd_stdev)

            ! translational_diffusion_coefficient
            tdc_mean = mean_from_sum(tdc_sum_global, tdc2_sum_global, dble(all_run), stdev=tdc_stdev)
            call write_x_xsd('tdc/tdc.txt', tdc_mean, tdc_stdev)
        end if
    end subroutine


    function calc_tdc() result(tdc)
        real(real64),parameter:: zeta = 2.837297d0
        real(real64):: tdc
        integer(int32):: l,r,n
        real(real64):: a, tdc_pbc

        l = fst_calc
        r = lst_calc
        n = r-l+1
        call least_squares_method(len_arr=n, arx=x(l:r), ary=msd(l:r), a=a)
        tdc_pbc = a / 6d0 * (temp0/temp)
        tdc = tdc_pbc + (kbt*temp0*zeta) / (6d0*pi*cell*viscousity) ! セルサイズ補正
    end function
end program
