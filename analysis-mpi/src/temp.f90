program main
    use,intrinsic :: iso_fortran_env
    use io_file_mod
    use md_condition_for_ana_mod
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
        real(real64):: temp
        real(real64):: temp_sum, temp2_sum
        real(real64):: temp_sum_global, temp2_sum_global
        real(real64):: temp_size, temp_mean, temp_var, temp_stdev

        if (rank==0) then
            ! 入出力はrank0で受け取って
            read*, fst_run, lst_run
        end if

        ! rank0から各rankに伝播
        call mpi_bcast(fst_run, 1, mpi_integer, 0, mpi_comm_world, ierr)
        call mpi_bcast(lst_run, 1, mpi_integer, 0, mpi_comm_world, ierr)

        ! input読み込み
        call load_condition_for_temp_ana(ndata)
        all_run = lst_run-fst_run + 1
        
        ! 計算
        temp_sum = 0d0
        temp2_sum = 0d0
        do i=fst_run+rank, lst_run, procs
            temp = read_temp_mean(ndata, i)
            temp_sum = temp_sum + temp
            temp2_sum = temp2_sum + temp*temp
            call write_x('./temp/temp_'//rundir(i)//'.dat', temp)
        end do
        
        ! 計算結果をrank0に格納、rank0が最終出力
        call mpi_reduce(temp_sum, temp_sum_global, 1, mpi_double_precision, mpi_sum, 0, mpi_comm_world, ierr)
        call mpi_reduce(temp2_sum, temp2_sum_global, 1, mpi_double_precision, mpi_sum, 0, mpi_comm_world, ierr)
        if (rank==0) then
            temp_size = dble(all_run)
            temp_mean = temp_sum_global / temp_size
            temp_var = temp2_sum_global / (temp_size-1) - temp_sum_global*temp_sum_global / temp_size / (temp_size-1)
            temp_stdev = sqrt(temp_var)
            print*, temp_mean, temp_stdev, temp_stdev/temp_mean*1d2
            call write_x_xsd('./temp/temp_mean.txt', temp_mean, temp_stdev)
        end if
    end subroutine


    function read_temp_mean(ndata, i) result(temp_mean)
        integer(int32),intent(in):: ndata, i
        real(real64):: temp_mean
        real(real64):: temp_raw(ndata)

        print*, 'reading ... ', '../calculation/'//rundir(i)//'/enrg.dat', rank
        call read_temp_from_enrgdat('../calculation/'//rundir(i)//'/enrg.dat', temp_raw, ndata)
        temp_mean = sum(temp_raw) / dble(ndata)
    end function
end program main