program main
    use,intrinsic :: iso_fortran_env
    use md_condition_for_ana_mod
    implicit none
    integer(int32), parameter:: np=500, gr_len=1000
    real(real64), parameter:: pi=acos(-1d0)
    integer(int32):: i, ndata
    
    real(real64):: gr_gp(gr_len), gr_pp(gr_len)
    real(real64):: cell, dr
    real(real64), allocatable:: rg(:,:,:), rp1(:,:,:), rp2(:,:,:)

    call load_condition_for_ext_gr_ana(ndata=ndata, cell=cell)
    dr = cell/dble(gr_len)
    allocate(rg(3,np,ndata), rp1(3,np,ndata), rp2(3,np,ndata))
    call load_rg_and_rp12(rg, rp1, rp2, ndata, np)
    call calc_gr(np, ndata, gr_len, cell, dr, rg, rp1, rp2, gr_gp, gr_pp)

    open(unit=11,file='ext_gr/gr_gp.dat', status='replace')
        write(11,'(2e14.5)') (i*dr, gr_gp(i), i=1,gr_len)
    close(11)

    open(unit=12,file='ext_gr/gr_pp.dat', status='replace')
        write(12,'(2e14.5)') (i*dr, gr_pp(i), i=1,gr_len)
    close(12)
contains
    subroutine load_rg_and_rp12(rg, rp1, rp2, ndata, np)
        real(real64),intent(out):: rg(:,:,:), rp1(:,:,:), rp2(:,:,:)
        real(real64),allocatable:: sxyz(:,:,:,:)
        integer(int32),intent(in):: ndata, np
        integer(int32):: i, j, k, u_sxyz

        allocate(sxyz(3,3,np,ndata))
        open (newunit=u_sxyz, file='../sxyz.dat', status='old')
            do i=1,ndata
                read(u_sxyz, *)
                read(u_sxyz, *) ((sxyz(:,k,j,i), k=1,3), j=1,np)
            end do
        close(u_sxyz)

        rg(:,:,:) = sxyz(:,3,:,:)
        rp1(:,:,:) = sxyz(:,1,:,:)
        rp2(:,:,:) = sxyz(:,2,:,:)
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


    subroutine calc_gr(np, ndata, gr_len, cell, dr, rg, rp1, rp2, gr_gp, gr_pp)
        integer(int32),intent(in):: ndata, np, gr_len
        real(real64),intent(in):: cell, dr, rg(:,:,:), rp1(:,:,:), rp2(:,:,:)
        real(real64),intent(out):: gr_gp(:), gr_pp(:)
        real(real64):: this_rg(3,np), this_rp1(3,np), this_rp2(3,np)
        real(real64):: rgp(3,4), rpp(3,4)
        integer(int32):: idata,id, i1, i2, j

        gr_gp(:) = 0d0
        gr_pp(:) = 0d0

        do idata=1,ndata
            if (mod(idata,100)==0) print*, idata, ndata
            this_rg(:,:) = rg(:,:,idata)
            this_rp1(:,:) = rp1(:,:,idata)
            this_rp2(:,:) = rp2(:,:,idata)
            do i1=1,np-1
                do i2=i1+1,np
                    rgp(:,1) = this_rg(:,i1) - this_rp1(:,i2)
                    rgp(:,2) = this_rg(:,i1) - this_rp2(:,i2)
                    rgp(:,3) = this_rg(:,i2) - this_rp1(:,i1)
                    rgp(:,4) = this_rg(:,i2) - this_rp2(:,i1)

                    rpp(:,1) = this_rp1(:,i1) - this_rp1(:,i2)
                    rpp(:,2) = this_rp1(:,i1) - this_rp2(:,i2)
                    rpp(:,3) = this_rp2(:,i1) - this_rp1(:,i2)
                    rpp(:,4) = this_rp2(:,i1) - this_rp2(:,i2)

                    do j=1,4
                        call adjust_periodic(rgp(:,j), cell)
                        id = ceiling(norm2(rgp(:,j)) / dr)
                        if (id <= gr_len) gr_gp(id) = gr_gp(id) + 1
                        
                        call adjust_periodic(rpp(:,j), cell)
                        id = ceiling(norm2(rpp(:,j)) / dr)
                        if (id <= gr_len) gr_pp(id) = gr_pp(id) + 1
                    end do
                end do
            end do
        end do

        call normalize_gr(gr_gp, ndata, np, cell, dr)
        call normalize_gr(gr_pp, ndata, np, cell, dr)
    end subroutine


    subroutine normalize_gr(gr,ndata,np,cell,dr)
        real(real64), parameter:: pi = acos(-1d0) 
        real(real64), intent(in):: cell, dr
        real(real64), intent(inout):: gr(:)
        integer(int32), intent(in)::ndata, np
        integer(int32):: i
        real(real64):: v,num_dens,factor

        gr(:) = gr(:) / dble(ndata*np*2d0)
        num_dens = dble(np) / (cell**3)

        factor = 4d0/3d0*pi * dr*dr*dr
        do i=lbound(gr,1), ubound(gr,1)
            v = factor * dble(3*i*(i-1)+1)
            gr(i) = (gr(i) / v) / num_dens  
        end do
    end subroutine
end program main