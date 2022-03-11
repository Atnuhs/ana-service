program main
    use,intrinsic :: iso_fortran_env
    use io_file_mod
    use md_condition_for_ana_mod
    use const_mod
    implicit none
    include 'mpif.h'
    integer(int32),parameter:: gr_len=100
    integer(int32):: procs, rank, ierr
    integer(int32):: np, ndata
    real(real64):: cell, dr
    real(real64),allocatable:: rg(:,:,:)
    real(real64),allocatable:: arrow(:,:,:)
    real(real64),allocatable:: mo_pp(:,:), mo_gp(:,:)
    
    
    call mpi_init(ierr)
    call mpi_comm_size(mpi_comm_world, procs, ierr)
    call mpi_comm_rank(mpi_comm_world, rank, ierr)
    call main_routine()
    call mpi_finalize(ierr)
contains
    subroutine main_routine
        integer(int32):: fst_run, lst_run, all_run
        integer(int32):: ix, iy, irun
        character(100):: file_sxyz
        real(real64), allocatable:: mo_pp_global(:,:), mo_gp_global(:,:)
        real(real64), allocatable:: x(:), y(:)
        

        if (rank==0) read*, fst_run, lst_run
        call mpi_bcast(fst_run, 1, mpi_integer, 0, mpi_comm_world, ierr)
        call mpi_bcast(lst_run, 1, mpi_integer, 0, mpi_comm_world, ierr)

        call load_condition_for_molecular_orientation_ana(ndata, np, cell)
        all_run = lst_run - fst_run + 1
        dr = 2d-11 ! 球殻の厚さ
        allocate(mo_pp(90,gr_len), mo_gp(90,gr_len))
        allocate(rg(3,np,ndata), arrow(3,np,ndata))
        
        do irun=fst_run+rank, lst_run, procs
            file_sxyz = '../calculation/' // rundir(irun) // '/sxyz.dat'
            print*, 'reading ... ', trim(file_sxyz), rank
            call load_rg_and_arrow(file_sxyz, ndata, np, rg, arrow)
            call calc_molecular_orientation()
        end do 

        allocate(mo_gp_global(90,gr_len))
        allocate(mo_pp_global(90,gr_len))
        call mpi_reduce(mo_gp, mo_gp_global, gr_len*90, mpi_double_precision, mpi_sum, 0, mpi_comm_world, ierr)
        call mpi_reduce(mo_pp, mo_pp_global, gr_len*90, mpi_double_precision, mpi_sum, 0, mpi_comm_world, ierr)

        if (rank==0) then 
            mo_gp_global(:,:) = mo_gp_global(:,:) / dble(ndata*all_run)
            mo_pp_global(:,:) = mo_pp_global(:,:) / dble(ndata*all_run)
            
            call normalize_molecular_orientation(mo_gp_global)
            call normalize_molecular_orientation(mo_pp_global)
            
            allocate(x(gr_len), source=[(ix*dr, ix=1,gr_len)])
            allocate(y(90), source=[(iy-0.5d0, iy=1,90)])
            
            call write_arx_ary_arz('molecular_orientation/mo_gp.txt', gr_len, 90, x, y, mo_gp_global)
            call write_arx_ary_arz('molecular_orientation/mo_pp.txt', gr_len, 90, x, y, mo_pp_global)
        end if
    end subroutine


    subroutine load_rg_and_arrow(file_sxyz, ndata, np, rg, arrow)
        character(*),intent(in):: file_sxyz
        integer(int32),intent(in):: ndata, np
        real(real64), intent(out):: rg(:,:,:), arrow(:,:,:)
        real(real64):: sxyz(3,3,np,ndata)

        call read_sxyz(file_sxyz, ndata, np, sxyz)
        rg(:,:,:) = sxyz(:,3,:,:)
        arrow(:,:,:) = sxyz(:,1,:,:) - sxyz(:,2,:,:)
    end subroutine
    

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


    function calc_angle(v1, v2) result(angle)
        real(real64),intent(in):: v1(3), v2(3)
        real(real64):: angle
        real(real64):: costheta, rad

        costheta = dot_product(v1,v2) / (norm2(v1)*norm2(v2))

        if (costheta > 1d0) costheta = 1d0
        if (costheta < -1d0) costheta = -1d0 
        rad = acos(abs(costheta))
        angle = rad * 180d0 / pi
    end function


    subroutine calc_molecular_orientation()
        integer(int32):: idata, i1, i2, id, iangle, i
        real(real64):: gg(3), pp1(3), pp2(3)

        mo_pp(:,:) = 0
        mo_gp(:,:) = 0
        ! calculate ----------------------------------------------------
        do idata=1,ndata
            if (mod(idata,100) == 0) print*, idata, ndata
            do i1=1,np-1
                pp1(:) = arrow(:,i1,idata) 
                do i2=i1+1,np
                    pp2(:) = arrow(:,i2,idata)
                    gg(:) = rg(:,i2,idata) - rg(:,i1,idata)
                    
                    call adjust_periodic(gg)
                    
                    id = ceiling(norm2(gg)/ dr)
                    if (id > gr_len) cycle

                    ! 軸1-軸2角度
                    iangle = ceiling(calc_angle(pp1, pp2))
                    mo_pp(iangle, id) = mo_pp(iangle, id) + 2d0

                    ! 軸1-重心12角度
                    iangle = ceiling(calc_angle(pp1, gg))
                    mo_gp(iangle, id) = mo_gp(iangle, id) + 1d0

                    ! 軸2-重心21角度
                    iangle = ceiling(calc_angle(pp2, gg))
                    mo_gp(iangle, id) = mo_gp(iangle, id) + 1d0
                end do
            end do
        end do

    end subroutine
    

    subroutine normalize_molecular_orientation(mo)
        real(real64),intent(inout):: mo(90,gr_len)
        real(real64):: this_v, factor, nd, all_inv_v
        integer(int32):: i

        ! normalize -------------------------------------------------------
        ! 球殻に対して数密度の規格化
        factor = 4d0/3d0*pi * dr*dr*dr ! 球殻の体積を求めるときの係数部分
        nd = dble(np) / (cell**3) ! Number Density
        
        do i=1,gr_len
            this_v = factor * dble(3*i*(i-1)+1) ! i番目の球殻の体積
            mo(:,i) = (mo(:,i) / this_v) / nd ! i番目の球殻の数密度 / 全体の数密度
        end do
        
        ! 角度に対して数密度の規格化
        all_inv_v = 0d0
        do i=1,90
            ! x軸からの角度がi-1°~i°の扇形をx軸を回転軸として回転させた時の体積
            ! (規格化のため、半球の体積が1となるような半径を用いる)
            ! 参考: http://ralfbalt.blog66.fc2.com/blog-entry-857.html
            this_v = cos(dble(i-1)*pi/180d0) - cos(dble(i)*pi/180d0)
            all_inv_v = all_inv_v + 1d0 / this_v
            mo(i,:) = mo(i,:) / this_v
        end do

        mo(:,:) = mo(:,:) / all_inv_v
    end subroutine
end program main