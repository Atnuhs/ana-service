program main
    use,intrinsic :: iso_fortran_env
    use md_condition_for_ana_mod
    implicit none
    integer(int32),parameter:: np=500, nlen=100
    integer(int32):: ndata, i
    real(real64):: rc, cell, dr, max_r
    real(real64),allocatable:: rg(:,:,:), arrow(:,:,:), mo1(:,:), mo2(:,:)

    call load_condition_for_molecular_orientation_ana(ndata, rc, cell)
    max_r = cell / 4d0 ! 半径どこまで見るか
    dr = max_r/dble(nlen) ! 0~max_rをnlen刻み
    allocate(mo1(90,nlen), mo2(90,nlen))
    allocate(rg(3,np,ndata), arrow(3,np,ndata))
    call load_rg_and_arrow(rg, arrow, ndata, np)
    call calc_molecular_orientation(rg, arrow, mo1, mo2, np, ndata, cell)

    open(unit=11, file='molecular_orientation/molecular_orientation.dat', status='replace')
        do i=1,90
            write(11,*) dble(i)-0.5d0, mo1(i, :)
        end do
    close(11)


    open(unit=12, file='molecular_orientation/molecular_orientation2.dat', status='replace')
        do i=1,90
            write(12,*) dble(i)-0.5d0, mo2(i, :)
        end do
    close(12)
contains
    subroutine load_rg_and_arrow(rg, arrow, ndata, np)
        real(real64), intent(out):: rg(:,:,:), arrow(:,:,:)
        real(real64), allocatable:: sxyz(:,:,:,:)
        integer(int32),intent(in):: ndata, np
        integer(int32):: i,j,k,u_sxyz

        allocate(sxyz(3,3,np,ndata))
        open (newunit=u_sxyz, file='../sxyz.dat', status='old')
            do i=1,ndata
                read(u_sxyz, *)
                read(u_sxyz, *) ((sxyz(:,k,j,i), k=1,3), j=1,np)
            end do
        close(u_sxyz)
        rg(:,:,:) = sxyz(:,3,:,:)
        arrow(:,:,:) = sxyz(:,1,:,:) - sxyz(:,2,:,:)
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


    subroutine calc_angle(v1, v2, angle)
        real(real64),parameter:: pi = acos(-1d0)
        real(real64),intent(in):: v1(3), v2(3)
        real(real64),intent(out):: angle
        real(real64):: costheta, rad

        costheta = dot_product(v1,v2) / (norm2(v1)*norm2(v2))

        if (abs(costheta) > 1d0) then
            print*, '変な値でた'
            print*, costheta
            print*, v1
            print*, v2
            error stop
        end if

        rad = acos(abs(costheta))
        angle = rad * 180d0 / pi
    end subroutine


    subroutine  make_pair_list(rg, cell, pair_list, pl_len)
        real(real64), intent(in):: rg(:,:), cell
        integer(int32),intent(out):: pair_list(:,:), pl_len
        integer(int32):: i, j
        real(real64):: rg1(3), rg2(3), rg12(3)

        pl_len=0
        do i=1,np-1
            rg1 = rg(:,i)
            do j=i+1,np
                rg2 = rg(:,j)
                rg12 = rg2 - rg1
                call adjust_periodic(rg12, cell)
                if (norm2(rg12) > cell*0.5d0) cycle
                pl_len = pl_len + 1
                pair_list(1, pl_len) = i
                pair_list(2, pl_len) = j
            end do
        end do
    end subroutine


    subroutine calc_molecular_orientation(rg, arrow, mo1, mo2, np, ndata, cell)
        real(real64), parameter:: pi = acos(-1d0) 
        real(real64),intent(out):: mo1(:,:), mo2(:,:)
        integer(int32),intent(in):: np, ndata
        real(real64),intent(in):: cell, rg(:,:,:), arrow(:,:,:)
        integer(int32):: idata, i1, i2, idistance, iangle, i
        integer(int32):: pair_list(2, np*np), pl_len
        real(real64):: this_rg(3,np), this_arrow(3,np)
        real(real64):: rg12(3), distance
        real(real64):: angle
        real(real64):: this_v, factor, nd

        mo1(:,:) = 0
        mo2(:,:) = 0
        do idata=1,ndata
            this_rg(:,:) = rg(:,:,idata)
            this_arrow(:,:) = arrow(:,:,idata)
            call make_pair_list(this_rg(:,:), cell, pair_list, pl_len)

            do i=1,pl_len
                i1 = pair_list(1,i)
                i2 = pair_list(2,i)
                rg12(:) = this_rg(:,i2) - this_rg(:,i1)
                
                call adjust_periodic(rg12, cell)
                distance = norm2(rg12)
                idistance = ceiling(distance / dr)
                if (idistance > nlen) cycle

                ! 軸-軸角度
                call calc_angle(this_arrow(:,i1), this_arrow(:,i2), angle)
                iangle = ceiling(angle)
                mo1(iangle, idistance) = mo1(iangle, idistance) + 2d0

                ! 軸-重心12角度
                call calc_angle(this_arrow(:,i1), rg12(:), angle)
                iangle = ceiling(angle)
                mo2(iangle, idistance) = mo2(iangle, idistance) + 1d0

                call calc_angle(this_arrow(:,i2), -rg12(:), angle)
                iangle = ceiling(angle)
                mo2(iangle, idistance) = mo2(iangle, idistance) + 1d0
            end do
        end do

        ! normalize
        mo1(:,:) = mo1(:,:) / dble(ndata)
        mo2(:,:) = mo2(:,:) / dble(ndata)

        ! 球殻に対して数密度の規格化
        factor = 4d0/3d0*pi * dr*dr*dr ! 球殻の体積を求めるときの係数部分
        nd = dble(np) / (cell**3) ! Number Density
        
        do i=1,nlen
            this_v = factor * dble(3*i*(i-1)+1) ! i番目の球殻の体積
            mo1(:,i) = (mo1(:,i) / this_v) / nd ! i番目の球殻の数密度 / 全体の数密度
            mo2(:,i) = (mo2(:,i) / this_v) / nd
        end do
        
        ! 角度に対して数密度の規格化
        do i=1,90
            ! x軸からの角度がi-1°~i°の扇形をx軸を回転軸として回転させた時の体積
            ! 規格化のため、半球の体積が1となるような半径を用いる
            this_v = cos(dble(i-1)*pi/180d0) - cos(dble(i)*pi/180d0)
            mo1(i,:) = mo1(i,:) / this_v
            mo2(i,:) = mo2(i,:) / this_v
        end do

    end subroutine
end program main