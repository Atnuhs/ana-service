program main
    use,intrinsic :: iso_fortran_env
    use md_condition_for_ana_mod
    implicit none
    integer(int32),parameter:: np=500, nlen=30
    integer(int32):: ndata, i
    real(real64):: rc, cell, dr, max_r
    real(real64),allocatable:: rg(:,:,:), arrow(:,:,:), mo(:,:)

    call load_condition_for_molecular_orientation_ana(ndata, rc, cell)
    max_r = rc / 3d0 ! 半径どこまで見るか
    dr = max_r/dble(nlen) ! 0~max_rをnlen刻み
    allocate(mo(90,nlen))
    allocate(rg(3,np,ndata), arrow(3,np,ndata))
    call load_rg_and_arrow(rg, arrow, ndata, np)
    call calc_molecular_orientation(rg, arrow, mo, np, ndata, rc, cell)

    open(unit=11, file='molecular_orientation/molecular_orientation.dat', status='replace')
        do i=1,90
            write(11,*) dble(i)-0.5d0, mo(i, :)
        end do
    close(11)
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


    subroutine adjust_periodic(vec, cell, rc)
        real(real64),intent(in):: cell,rc
        real(real64),intent(inout):: vec(3)

        where(vec > rc)
            vec=vec-cell
        else where(vec < -rc)
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


    subroutine calc_molecular_orientation(rg, arrow, mo, np, ndata, rc, cell)
        real(real64), parameter:: pi = acos(-1d0) 
        real(real64),intent(out):: mo(:,:)
        integer(int32),intent(in):: np, ndata
        real(real64),intent(in):: rc, cell, rg(:,:,:), arrow(:,:,:)
        integer(int32):: idata, i1, i2, idistance, iangle, i
        real(real64):: rgi1(3),rgi1i2(3), arrowi1(3), distance, angle
        real(real64):: this_v, factor, nd

        mo(:,:) = 0
        do idata=1,ndata
            do i1=1,np-1
                rgi1(:) = rg(:,i1,idata)
                arrowi1(:) = arrow(:,i1,idata)
                do i2=i1+1,np
                    rgi1i2(:) = rg(:,i2,idata) - rgi1(:)
                    call adjust_periodic(rgi1i2, cell, rc)

                    ! 距離の計算
                    distance = norm2(rgi1i2)
                    idistance = ceiling(distance / dr)
                    if (idistance > nlen) cycle

                    ! 角度の計算
                    call calc_angle(arrowi1, arrow(:,i2,idata), angle)
                    iangle = ceiling(angle)
                    if (iangle < 1 .or. iangle > 90) then
                        print*, 'iangle 値が変'
                        print*, i1, i2, angle, iangle
                        error stop
                    end if

                    mo(iangle, idistance) = mo(iangle, idistance) + 2d0
                end do
            end do
        end do

        ! normalize
        mo(:,:) = mo(:,:) / dble(ndata)

        ! 球殻に対して数密度の規格化
        factor = 4d0/3d0*pi * dr*dr*dr ! 球殻の体積を求めるときの係数部分
        nd = dble(np) / (cell**3) ! Number Density
        
        do i=1,nlen
            this_v = factor * dble(3*i*(i-1)+1) ! i番目の球殻の体積
            mo(:,i) = (mo(:,i) / this_v) / nd ! i番目の球殻の数密度 / 全体の数密度
        end do
        
        ! 角度に対して数密度の規格化
        do i=1,90
            ! x軸からの角度がi-1°~i°の扇形をx軸を回転軸として回転させた時の体積
            ! 規格化のため、半球の体積が1となるような半径を用いる
            this_v = cos(dble(i-1)*pi/180d0) - cos(dble(i)*pi/180d0)
            mo(i,:) = mo(i,:) / this_v
        end do

    end subroutine
end program main