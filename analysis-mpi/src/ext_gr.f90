program main
    use,intrinsic :: iso_fortran_env
    use const_mod
    use md_condition_for_ana_mod
    use io_file_mod
    implicit none
    include 'mpif.h'
    integer(int32), parameter:: gr_len=1000, gr_3d_len=100
    integer(int32):: procs, rank, ierr
    integer(int32):: np, ndata
    real(real64):: cell, dr, dr_3d
    real(real64), allocatable:: sxyz(:,:,:,:)
    real(real64), allocatable:: gr_gp(:), gr_pp(:), gr_gg(:)
    real(real64), allocatable:: gr_3d(:,:) 
    
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
        real(real64),allocatable:: gr_3d_global(:,:)
        real(real64), allocatable:: x(:)

        if (rank==0) read*, fst_run, lst_run

        call mpi_bcast(fst_run, 1, mpi_integer, 0, mpi_comm_world, ierr)
        call mpi_bcast(lst_run, 1, mpi_integer, 0, mpi_comm_world, ierr)
        call load_condition_for_ext_gr_ana(ndata, np, cell)
        ndata = ndata / 50
        all_run = lst_run - fst_run + 1
        dr = 1d-12 ! 球殻の厚み / m
        dr_3d = 1d-11 ! 球殻の厚み(gr_3d) / m
        allocate(sxyz(3,3,np,ndata))
        allocate(gr_gp(gr_len), gr_pp(gr_len), gr_gg(gr_len), source=0d0)
        allocate(gr_3d(gr_3d_len,gr_3d_len), source=0d0)

        do i=fst_run+rank, lst_run, procs
            file_sxyz = '../calculation/' // rundir(i) // '/sxyz.dat'
            print*, 'reading ... ', trim(file_sxyz), rank
            call read_sxyz(file_sxyz, ndata, np, sxyz)
            call calc_ext_gr()
        end do
        allocate(gr_gp_global(gr_len))
        allocate(gr_pp_global(gr_len))
        allocate(gr_gg_global(gr_len))
        allocate(gr_3d_global(gr_3d_len, gr_3d_len))
        call mpi_reduce(gr_gp, gr_gp_global, gr_len, mpi_double_precision, mpi_sum, 0, mpi_comm_world, ierr)
        call mpi_reduce(gr_pp, gr_pp_global, gr_len, mpi_double_precision, mpi_sum, 0, mpi_comm_world, ierr)
        call mpi_reduce(gr_gg, gr_gg_global, gr_len, mpi_double_precision, mpi_sum, 0, mpi_comm_world, ierr)
        call mpi_reduce(gr_3d, gr_3d_global, gr_3d_len*gr_3d_len, mpi_double_precision, mpi_sum, 0, mpi_comm_world, ierr)
        
        if (rank==0) then
            print*, sum(gr_gp_global)
            print*, sum(gr_pp_global)
            print*, sum(gr_gg_global)
            print*, sum(gr_3d_global)


            gr_gp_global(:) = gr_gp_global(:) / dble(np*ndata*all_run) * 0.25d0
            gr_pp_global(:) = gr_pp_global(:) / dble(np*ndata*all_run) * 0.25d0
            gr_gg_global(:) = gr_gg_global(:) / dble(np*ndata*all_run)
            gr_3d_global(:,:) = gr_3d_global(:,:) / dble(np*ndata*all_run) * 0.25d0

            call normalize_gr(gr_gp_global)
            call normalize_gr(gr_pp_global)
            call normalize_gr(gr_gg_global)
            
            call normalize_gr_3d(gr_3d_global)
            allocate(x(gr_len), source=[(i*dr, i=1,gr_len)])
            
            call write_arx_ary('./ext_gr/gr_gp.txt', gr_len, x, gr_gp_global)
            call write_arx_ary('./ext_gr/gr_pp.txt', gr_len, x, gr_pp_global)
            call write_arx_ary('./ext_gr/gr_gg.txt', gr_len, x, gr_gg_global)
            
            deallocate(x)
            allocate(x(gr_3d_len), source=[(i*dr_3d, i=1,gr_3d_len)])
            
            call write_arx_ary_arz('./ext_gr/gr_3d.txt', gr_3d_len, gr_3d_len, x, x, gr_3d_global)
        end if
    end subroutine
    

    subroutine calc_ext_gr()
        integer(int32):: idata, i1, i2, j1, j2, id, id11, id12, id21, id22
        real(real64):: r1(3,3), r2(3,3)
        integer(int32):: cnt(gr_len, 3, 3)
        integer(int32):: cnt_3d(gr_3d_len, gr_3d_len)

        
        do idata=1,ndata
            if (mod(idata,100)==0) print*, idata, ndata
            cnt(:,:,:) = 0
            cnt_3d(:,:) = 0
            do i1=1,np-1
                r1(:,:) = sxyz(:,:,i1,idata)
                do i2=i1+1,np
                    r2(:,:) = sxyz(:,:,i2,idata)
                    
                    ! gr_gg, gr_pp, gr_gpのカウント
                    do j1=1,3
                        do j2=1,3
                            id = calc_id(r1(:,j1), r2(:,j2), dr)
                            if (id <= gr_len) cnt(id,j2,j1) = cnt(id,j2,j1) + 2
                        end do
                    end do

                    ! gr_3dのカウント
                    id11 = calc_id(r1(:,1), r2(:,1), dr_3d)
                    id12 = calc_id(r1(:,1), r2(:,2), dr_3d)
                    id21 = calc_id(r1(:,2), r2(:,1), dr_3d)
                    id22 = calc_id(r1(:,2), r2(:,2), dr_3d)
                    
                    if (id11 <= gr_3d_len .and. id12 <= gr_3d_len) then
                        cnt_3d(id11,id12)=cnt_3d(id11,id12)+1
                        cnt_3d(id12,id11)=cnt_3d(id12,id11)+1
                    end if

                    if (id21 <= gr_3d_len .and. id22 <= gr_3d_len) then
                        cnt_3d(id21,id22)=cnt_3d(id21,id22)+1
                        cnt_3d(id22,id21)=cnt_3d(id22,id21)+1
                    end if

                    if (id11 <= gr_3d_len .and. id21 <= gr_3d_len) then
                        cnt_3d(id11,id21)=cnt_3d(id11,id21)+1
                        cnt_3d(id21,id11)=cnt_3d(id21,id11)+1
                    end if

                    if (id12 <= gr_3d_len .and. id22 <= gr_3d_len) then
                        cnt_3d(id12,id22)=cnt_3d(id12,id22)+1
                        cnt_3d(id22,id12)=cnt_3d(id22,id12)+1
                    end if
                end do
            end do
            
            ! 重心-重心間gr
            gr_gg(:)=gr_gg(:) + cnt(:,3,3)

            ! 重心-粒子間gr
            gr_gp(:) = gr_gp(:) + cnt(:,1,3)
            gr_gp(:) = gr_gp(:) + cnt(:,2,3)
            gr_gp(:) = gr_gp(:) + cnt(:,3,1)
            gr_gp(:) = gr_gp(:) + cnt(:,3,2)
            
            ! 粒子-粒子間gr
            gr_pp(:) = gr_pp(:) + cnt(:,1,1)
            gr_pp(:) = gr_pp(:) + cnt(:,2,1)
            gr_pp(:) = gr_pp(:) + cnt(:,1,2)
            gr_pp(:) = gr_pp(:) + cnt(:,2,2)
            
            ! 3dgr
            gr_3d(:,:) = gr_3d(:,:) + cnt_3d(:,:)
        end do
    end subroutine

    
    function calc_id(r1,r2,dr) result(id)
        real(real64),intent(in):: r1(3), r2(3)
        real(real64),intent(in):: dr
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
        factor = 4d0/3d0*pi * dr**3
        do i=1,size(gr)
            v = factor * dble(3*i*(i-1)+1)
            gr(i) = (gr(i) / v) / num_dens  
        end do
    end subroutine
    

    subroutine normalize_gr_3d(gr)
        real(real64),intent(inout):: gr(:,:)
        integer(int32):: ix, iy
        real(real64):: vx, vy, num_dens, factor

        num_dens = dble(np) / (cell**3)
        factor = 4d0/3d0*pi * dr_3d**3

        do iy=1,gr_3d_len
            do ix=1,gr_3d_len 
                vx = factor * dble(3*ix*(ix-1)+1)
                vy = factor * dble(3*iy*(iy-1)+1)
                gr(ix,iy) = (gr(ix,iy) / (vx+vy))/ num_dens * dble(gr_3d_len)
            end do
        end do
    end subroutine
end program main