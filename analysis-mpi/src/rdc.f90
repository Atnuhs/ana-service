
program main
    use,intrinsic :: iso_fortran_env
    use md_condition_for_ana_mod
    use representative_value_mod
    use io_file_mod
    implicit none
    include 'mpif.h'
    integer(int32):: procs, rank, ierr 
    integer(int32):: fst_calc, lst_calc, ndata
    real(real64):: rdc, temp0, temp
    real(real64), allocatable:: x(:), rmsd(:)

    ! prcos:: プロセス数 rank:: 自分のプロセスid(0-indexed)

    call mpi_init(ierr)
    call mpi_comm_size(mpi_comm_world, procs, ierr)
    call mpi_comm_rank(mpi_comm_world, rank, ierr)
    call main_routine()
    call mpi_finalize(ierr)
contains
    subroutine main_routine()
        integer(int32):: i
        integer(int32):: fst_run, lst_run, all_run
        real(real64), allocatable::  rmsd_sum(:), rmsd2_sum(:)
        real(real64), allocatable:: rmsd_sum_global(:), rmsd2_sum_global(:)
        real(real64), allocatable:: rmsd_mean(:), rmsd_stdev(:)
        real(real64):: rdc_sum, rdc2_sum, rdc_sum_global, rdc2_sum_global
        real(real64):: rdc_mean, rdc_stdev
    
        if (rank==0) read*, fst_run, lst_run, fst_calc, lst_calc
        all_run = lst_run-fst_run+1

        call mpi_bcast(fst_run, 1, mpi_integer, 0, mpi_comm_world, ierr)
        call mpi_bcast(lst_run, 1, mpi_integer, 0, mpi_comm_world, ierr)
        call mpi_bcast(fst_calc, 1, mpi_integer, 0, mpi_comm_world, ierr)
        call mpi_bcast(lst_calc, 1, mpi_integer, 0, mpi_comm_world, ierr)

        ! インプットの読み込みなどの準備
        call load_condition_for_rdc_ana(ndata, temp0)
        allocate(rmsd(ndata), rmsd_sum(ndata), rmsd2_sum(ndata))
        allocate(rmsd_sum_global(ndata), rmsd2_sum_global(ndata))
        allocate(rmsd_mean(ndata), rmsd_stdev(ndata))
        allocate(x(ndata))

        rmsd_sum(:) = 0d0
        rmsd2_sum(:) = 0d0
        rdc_sum = 0d0
        rdc2_sum = 0d0

        do i=fst_run+rank, lst_run, procs
            ! rmsd
            print*, 'reading ... ', 'rmsd/rmsd_/'//rundir(i)//'.txt', rank
            call read_arx_ary('rmsd/rmsd_'//rundir(i)//'.txt', ndata, x, rmsd)
            rmsd_sum(:) = rmsd_sum(:) + rmsd(:)
            rmsd2_sum(:) = rmsd2_sum(:) + rmsd(:)*rmsd(:)

            ! translational_diffusion_coefficient
            call read_x('./temp/temp_'//rundir(i)//'.dat', temp)
            rdc = calc_rdc()
                
            rdc_sum = rdc_sum + rdc
            rdc2_sum = rdc2_sum + rdc*rdc
        end do
        
        call mpi_reduce(rmsd_sum, rmsd_sum_global, ndata, mpi_double_precision, mpi_sum, 0, mpi_comm_world, ierr)
        call mpi_reduce(rmsd2_sum, rmsd2_sum_global, ndata, mpi_double_precision, mpi_sum, 0, mpi_comm_world, ierr)
        call mpi_reduce(rdc_sum, rdc_sum_global, 1, mpi_double_precision, mpi_sum, 0, mpi_comm_world, ierr)
        call mpi_reduce(rdc2_sum, rdc2_sum_global, 1, mpi_double_precision, mpi_sum, 0, mpi_comm_world, ierr)

        if (rank==0) then
            ! rmsd
            do i=1,ndata
                rmsd_mean(i) = mean_from_sum(rmsd_sum_global(i), rmsd2_sum_global(i), dble(all_run), stdev=rmsd_stdev(i))
            end do
            call write_arx_ary_arysd('rdc/rmsd_mean.txt', ndata, x, rmsd_mean, rmsd_stdev)


            ! translational_diffusion_coefficient
            rdc_mean = mean_from_sum(rdc_sum_global, rdc2_sum_global, dble(all_run), stdev=rdc_stdev)
            call write_x_xsd('rdc/rdc.txt', rdc_mean, rdc_stdev)
        end if
        
    end subroutine


    function calc_rdc() result(rdc)
        real(real64):: rdc
        integer(int32):: l,r,n
        real(real64):: a

        l = fst_calc
        r = lst_calc
        n = r-l+1
        call least_squares_method(len_arr=n, arx=x(l:r), ary=rmsd(l:r), a=a)
        rdc = a / 4d0 * (temp/temp0)
    end function
end program
