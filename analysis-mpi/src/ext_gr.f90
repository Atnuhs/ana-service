program main
    use,intrinsic :: iso_fortran_env
    use const_mod
    use md_condition_for_ana_mod
    use io_file_mod
    implicit none
    include 'mpif.h'
    integer(int32), parameter:: gr_len=1000
    integer(int32):: procs, rank, ierr
    integer(int32):: np, ndata
    real(real64):: cell, dr
    real(real64), allocatable:: sxyz(:,:,:,:)
    real(real64), allocatable:: gr_gp(:), gr_pp(:), gr_gg(:)
    
    call mpi_init(ierr)
    call mpi_comm_size(mpi_comm_world, procs, ierr)
    call mpi_comm_rank(mpi_comm_world, rank, ierr)
    call main_routine()
    call mpi_finalize(ierr)
contains
    subroutine main_routine()
        integer(int32):: fst_run, lst_run, all_run
        integer(int32):: i 
        character(100):: file_sxyz
        real(real64),allocatable:: gr_gp_global(:)
        real(real64),allocatable:: gr_pp_global(:)
        real(real64),allocatable:: gr_gg_global(:)
        real(real64), allocatable:: x(:)

        if (rank==0) read*, fst_run, lst_run

        call mpi_bcast(fst_run, 1, mpi_integer, 0, mpi_comm_world, ierr)
        call mpi_bcast(lst_run, 1, mpi_integer, 0, mpi_comm_world, ierr)
        call load_condition_for_ext_gr_ana(ndata, np, cell)
        all_run = lst_run - fst_run + 1
        dr = 2d-11 ! 球殻の厚み
        allocate(sxyz(3,3,np,ndata))
        allocate(gr_gp(gr_len), gr_pp(gr_len), gr_gg(gr_len), source=0d0)

        do i=fst_run+rank, lst_run, procs
            file_sxyz = '../calculation/' // rundir(i) // '/sxyz.dat'
            print*, 'reading ... ', trim(file_sxyz), rank
            call read_sxyz(file_sxyz, ndata, np, sxyz)
            call calc_ext_gr()
        end do
        allocate(gr_gp_global(gr_len))
        allocate(gr_pp_global(gr_len))
        allocate(gr_gg_global(gr_len))
        call mpi_reduce(gr_gp, gr_gp_global, gr_len, mpi_double_precision, mpi_sum, 0, mpi_comm_world, ierr)
        call mpi_reduce(gr_pp, gr_pp_global, gr_len, mpi_double_precision, mpi_sum, 0, mpi_comm_world, ierr)
        call mpi_reduce(gr_gg, gr_gg_global, gr_len, mpi_double_precision, mpi_sum, 0, mpi_comm_world, ierr)

        
        if (rank==0) then
            gr_gp_global(:) = gr_gp_global(:) / dble(np*ndata*all_run) * 0.25d0
            gr_pp_global(:) = gr_pp_global(:) / dble(np*ndata*all_run) * 0.25d0
            gr_gg_global(:) = gr_gg_global(:) / dble(np*ndata*all_run)
           
            call normalize_gr(gr_gp_global)
            call normalize_gr(gr_pp_global)
            call normalize_gr(gr_gg_global)
            
            allocate(x(gr_len), source=[(i*dr, i=1,gr_len)])
            
            call write_arx_ary('./ext_gr/gr_gp.txt', gr_len, x, gr_gp_global)
            call write_arx_ary('./ext_gr/gr_pp.txt', gr_len, x, gr_pp_global)
            call write_arx_ary('./ext_gr/gr_gg.txt', gr_len, x, gr_gg_global)
        end if
    end subroutine
    

    subroutine calc_ext_gr()
        integer(int32):: idata, i1, i2, j1, j2, id
        real(real64):: r1(3,3), r2(3,3)
        integer(int32):: cnt(gr_len, 3, 3)

        
        do idata=1,ndata
            if (mod(idata,1000)==0) print*, idata, ndata
            cnt(:,:,:) = 0
            do i1=1,np-1
                r1(:,:) = sxyz(:,:,i1,idata)
                do i2=i1+1,np
                    r2(:,:) = sxyz(:,:,i2,idata)
                    do j1=1,3
                        do j2=1,3
                            id = calc_id(r1(:,j1), r2(:,j2))
                            if (id <= gr_len) cnt(id,j2,j1) = cnt(id,j2,j1) + 2
                        end do
                    end do
                end do
            end do
            
            ! 重心-重心間gr
            gr_gg(:)=gr_gg(:) + cnt(:,3,3)

            ! 重心-粒子間gr
            gr_gp(:)=gr_gp(:) + cnt(:,1,3)
            gr_gp(:)=gr_gp(:) + cnt(:,2,3)
            gr_gp(:)=gr_gp(:) + cnt(:,3,1)
            gr_gp(:)=gr_gp(:) + cnt(:,3,2)
            
            ! 粒子-粒子間gr
            gr_pp(:)=gr_pp(:) + cnt(:,1,1)
            gr_pp(:)=gr_pp(:) + cnt(:,2,1)
            gr_pp(:)=gr_pp(:) + cnt(:,1,2)
            gr_pp(:)=gr_pp(:) + cnt(:,2,2)
        end do
    end subroutine

    
    function calc_id(r1,r2) result(id)
        real(real64),intent(in):: r1(3), r2(3)
        real(real64):: d(3)
        integer(int32):: id

        d(:) = r1(:) - r2(:)
        call adjust_periodic(d)
        id = ceiling(norm2(d) / dr) 
    end function

    subroutine adjust_periodic(vec)
        real(real64),intent(inout):: vec(3)
        real(real64):: hcell

        hcell = cell * 0.5d0

        where(vec > hcell)
            vec=vec-cell
        else where(vec < -hcell)
            vec=vec+cell
        end where
    end subroutine


    subroutine normalize_gr(gr)
        real(real64), intent(inout):: gr(:)
        integer(int32):: i
        real(real64):: v,num_dens,factor

        num_dens = dble(np) / (cell**3)
        factor = 4d0/3d0*pi * dr*dr*dr
        do i=1,size(gr)
            v = factor * dble(3*i*(i-1)+1)
            gr(i) = (gr(i) / v) / num_dens  
        end do
    end subroutine
end program main