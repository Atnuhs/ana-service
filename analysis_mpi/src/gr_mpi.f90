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
        integer(int32):: fst_run, lst_run

        if (rank==0) then
            ! 入出力はrank0で受け取って
            read*, fst_run, lst_run
            call load_condition_for_gr_ana(ndata, cell)

        end if
        call mpi_bcast(fst_run, 1, mpi_integer, 0, mpi_comm_world, ierr)
        call mpi_bcast(lst_run, 1, mpi_integer, 0, mpi_comm_world, ierr)

        do i=fst_run+rank, lst_run, procs
            print*, rundir(i)
            ! dr = cell/dble(gr_len)
            ! allocate(rxyz(3,np,ndata))
            ! gr(:) = 0d0
            ! call read_rxyz(rxyz, ndata, np)
            ! call calc_gr(rxyz, ndata, np, cell, gr_len, dr, gr)
            ! call normalize_gr(gr,ndata,np,cell,dr)
        end do
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


    subroutine calc_gr(rxyz, ndata, np, cell, gr_len, dr, gr)
        integer(int32),intent(in):: ndata, np, gr_len
        real(real64),intent(in):: cell, dr, rxyz(:,:,:)
        real(real64),intent(inout):: gr(:)
        real(real64):: ri(3), rij(3)
        integer(int32):: idata,id,i,j

        do idata=1,ndata
            do i=1,np
                ri(:) = rxyz(:,i,idata)
                do j=i+1,np
                    rij(:) = rxyz(:,j,idata) - ri(:)
                    call adjust_periodic(rij, cell)
                    id = ceiling(norm2(rij)/dr)
                    if (id <= gr_len) gr(id) = gr(id) + 2
                end do
            end do
        end do
    end subroutine


    subroutine normalize_gr(gr, ndata, np, cell, dr)
        real(real64), parameter:: pi = acos(-1d0) 
        real(real64), intent(in):: cell, dr
        real(real64), intent(inout):: gr(:)
        integer(int32), intent(in)::ndata, np
        integer(int32):: i
        real(real64):: v,num_dens,factor

        num_dens = np / (cell*cell*cell)
        gr(:) = gr(:) / num_dens / dble(np*ndata)

        factor=4d0/3d0*pi * dr*dr*dr
        
        do i=lbound(gr,1), ubound(gr,1)
            v = factor * dble(3*i*(i-1)+1)
            gr(i)=gr(i)/v
        end do
    end subroutine
end program main