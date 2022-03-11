program main
    use,intrinsic :: iso_fortran_env
    use io_file_mod
    use md_condition_for_ana_mod
    use const_mod
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
        integer(int32),parameter:: gr_len=1000
        integer(int32):: i
        integer(int32):: fst_run, lst_run, all_run
        integer(int32):: ndata, np
        real(real64):: cell, dr
        real(real64):: gr_local(gr_len), gr_global(gr_len), x(gr_len)
        real(real64),allocatable:: rxyz(:,:,:)

        if (rank==0) then
            ! 入出力はrank0で受け取って
            read*, fst_run, lst_run
        end if

        ! rank0から各rankに伝播
        call mpi_bcast(fst_run, 1, mpi_integer, 0, mpi_comm_world, ierr)
        call mpi_bcast(lst_run, 1, mpi_integer, 0, mpi_comm_world, ierr)

        ! input読み込み
        call load_condition_for_gr_ana(ndata, np, cell)
        
        all_run = lst_run-fst_run + 1
        dr = cell/dble(gr_len)
        allocate(rxyz(3,np,ndata))
        gr_local(:) = 0d0 ! 各プロセスでのデータ

        ! 計算
        do i=fst_run+rank, lst_run, procs
            print*, 'reading ... ', '../calculation/'//rundir(i)//'/rxyz.dat', rank
            call read_rxyz('../calculation/'//rundir(i)//'/rxyz.dat', ndata, np, rxyz)
            call calc_gr(rxyz, ndata, np, cell, gr_len, dr, gr_local)
        end do
        call normalize_gr(gr_local, ndata, np, all_run, cell, dr)

        ! 計算結果をrank0に格納、rank0が最終出力
        call mpi_reduce(gr_local, gr_global, gr_len,mpi_double_precision, mpi_sum, 0, mpi_comm_world, ierr)
        if (rank==0) then
            x(:) = [(i*dr, i=1,gr_len)]
            call write_arx_ary('./gr/gr_mean.txt', gr_len, x, gr_global)
        end if
    end subroutine


    subroutine adjust_periodic(vec, cell)
        
        real(real64),intent(in):: cell
        real(real64),intent(inout):: vec(3)
        real(real64):: hcell

        hcell = cell * 0.5d0

        where(vec > hcell)
            vec=vec-cell
        else where(vec < -hcell)
            vec=vec+cell
        end where
    end subroutine


    subroutine calc_gr(rxyz, ndata, np, cell, gr_len, dr, gr_local)
        integer(int32),intent(in):: ndata, np, gr_len
        real(real64),intent(in):: cell, dr, rxyz(:,:,:)
        real(real64),intent(inout):: gr_local(:)
        real(real64):: ri(3), rij(3)
        integer(int32):: idata,id,i,j

        do idata=1,ndata
            do i=1,np
                ri(:) = rxyz(:,i,idata)
                do j=i+1,np
                    rij(:) = rxyz(:,j,idata) - ri(:)
                    call adjust_periodic(rij, cell)
                    id = ceiling(norm2(rij)/dr)
                    if (id <= gr_len) gr_local(id) = gr_local(id) + 2
                end do
            end do
        end do
    end subroutine


    subroutine normalize_gr(gr_local, ndata, np, all_run, cell, dr)
        real(real64), intent(in):: cell, dr
        real(real64), intent(inout):: gr_local(:)
        integer(int32), intent(in)::ndata, np, all_run
        integer(int32):: i
        real(real64):: v,num_dens,factor

        num_dens = np / (cell*cell*cell)
        gr_local(:) = gr_local(:) / num_dens / dble(np*ndata*all_run)

        factor=4d0/3d0*pi * dr*dr*dr
        
        do i=lbound(gr_local,1), ubound(gr_local,1)
            v = factor * dble(3*i*(i-1)+1)
            gr_local(i)=gr_local(i)/v
        end do
    end subroutine
end program main