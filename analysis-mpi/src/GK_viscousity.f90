program main
    use,intrinsic :: iso_fortran_env
    use const_mod
    use fft_mod
    use io_file_mod
    use md_condition_for_ana_mod
    use numerical_integration_mod
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
        ! プロセス並列
        integer(int32):: i
        integer(int32):: fst_run, lst_run, all_run
        integer(int32):: ndata
        real(real64):: temp, dt, vol, visc_size
        real(real64),allocatable:: stress(:,:), acf(:), integ(:), x(:)
        real(real64),allocatable:: integ_sum(:), integ2_sum(:)
        real(real64),allocatable:: integ_sum_global(:), integ2_sum_global(:)
        real(real64),allocatable:: integ_mean(:), integ_stdev(:)
        real(real64):: integ_var

        if (rank==0) then
            ! 入出力はrank0で受け取って
            read*, fst_run, lst_run
        end if

        ! rank0から各rankに伝播
        call mpi_bcast(fst_run, 1, mpi_integer, 0, mpi_comm_world, ierr)
        call mpi_bcast(lst_run, 1, mpi_integer, 0, mpi_comm_world, ierr)

        ! input読み込み
        call load_condition_for_GK_viscousity_ana(ndata, dt, vol)
        call init_acf(ndata)
        all_run = lst_run-fst_run + 1
        allocate(stress(ndata, 3))
        allocate(acf(ndata), integ(ndata))
        allocate(integ_sum(ndata), integ2_sum(ndata))
        allocate(integ_sum_global(ndata), integ2_sum_global(ndata))
        allocate(integ_mean(ndata), integ_stdev(ndata))
        ! 計算
        integ_sum(:) = 0d0
        integ2_sum(:) = 0d0
        do i=fst_run+rank, lst_run, procs
            print*, 'reading ... ', '../calculation/'//rundir(i)//'/stress.dat', rank

            call read_temp(temp, i)
            call read_tdc('../calculation/'//rundir(i)//'/stress.dat', ndata, stress)
            call calc_acf_stress(stress, ndata, vol, temp, acf)
            integ(:) = trapezoidal_integration(f=acf, dx=dt, n=ndata)

            integ_sum(:) = integ_sum(:) + integ(:)
            integ2_sum(:) = integ2_sum(:) + integ(:)*integ(:) 
        end do  
        
        
        ! 計算結果をrank0に格納、rank0が最終出力
        call mpi_reduce(integ_sum, integ_sum_global, ndata, mpi_double_precision, mpi_sum, 0, mpi_comm_world, ierr)
        call mpi_reduce(integ2_sum, integ2_sum_global, ndata, mpi_double_precision, mpi_sum, 0, mpi_comm_world, ierr)
        if (rank==0) then
            ! 出力
            visc_size = dble(all_run)
            do i=1,ndata
                integ_mean(i) = integ_sum_global(i) / visc_size
                integ_var = integ2_sum_global(i) / (visc_size-1) - integ_sum_global(i)**2 / visc_size / (visc_size-1)
                integ_stdev(i) = sqrt(integ_var)
            end do

            allocate(x(ndata), source=[(dt*dble(i-1), i=1,ndata)])
            call write_arx_ary_arysd('GK_viscousity/integ.txt', ndata, x, integ_mean, integ_stdev)
        end if
    end subroutine


    subroutine read_temp(temp, i)
        integer(int32),intent(in):: i
        real(real64),intent(out):: temp

        call read_x('temp/temp_'//rundir(i)//'.dat', temp)
    end subroutine


    subroutine calc_acf_stress(stress, ndata, vol, temp, acf)
        integer(int32),intent(in):: ndata
        real(real64),intent(in):: vol, temp, stress(:,:)
        real(real64),intent(out):: acf(:)
        integer(int32):: i

        acf(:) = 0d0
        call init_acf(ndata)
        do i=1,3
            acf(:) = acf(:) + auto_correlation_function(stress(:,i))
        end do
        acf(:) = acf(:) / (dble(ndata)*vol*kbt*temp*3d0)
    end subroutine
end program main