program main
    use,intrinsic :: iso_fortran_env
    use const_mod
    use md_condition_for_ana_mod
    use io_file_mod
    use read_md_results_mod
    implicit none
    integer(int32), parameter:: np=500, n_gr_x=1000, nsep=10 ! sxyz.datをnsep分割して読み込む。
    integer(int32):: ndata, n_read_row
    real(real64), allocatable:: sxyz(:,:,:,:)
    real(real64), allocatable:: gr_gp(:), gr_pp(:), gr_gg(:)
    real(real64), allocatable:: rg(:,:,:), rp1(:,:,:), rp2(:,:,:)
    real(real64):: cell, dr

    call init_ext_gr()
    call  calculate_ext_gr()
contains
    subroutine init_ext_gr()
        call load_condition_for_ext_gr_ana(ndata=ndata, cell=cell)
        ndata=500
        n_read_row = ndata / nsep
        print*, n_read_row
        dr = cell/dble(n_gr_x)
        allocate(gr_gp(n_gr_x), gr_pp(n_gr_x), gr_gg(n_gr_x), source=0d0)
        allocate(rg(3,np,n_read_row), rp1(3,np,n_read_row), rp2(3,np,n_read_row))
        allocate(sxyz(3,3,np,n_read_row))
    end subroutine

    subroutine calculate_ext_gr()
        integer(int32):: i, isep
        real(real64),allocatable:: x(:)

        call open_sxyz()
        do isep=1, nsep
            if (mod(isep*100,nsep)==0) print*, isep, nsep
            ! 読み込み
            call load_rg_and_rp12()
            ! 粒子の距離を計算してカウント
            call count_gr()
        end do
        call close_sxyz()

        ! 正規化
        call normalize_gr(gr_gg)
        call normalize_gr(gr_gp)
        call normalize_gr(gr_pp)

        ! 出力
        allocate(x(n_gr_x), source=[(dble(i-1)*dr, i=1,n_gr_x)])
        call write_arx_ary(n_gr_x, 'ext_gr/gr_gg.dat', x, gr_gg)
        call write_arx_ary(n_gr_x, 'ext_gr/gr_gp.dat', x, gr_gp)
        call write_arx_ary(n_gr_x, 'ext_gr/gr_pp.dat', x, gr_pp)
    end subroutine

    subroutine load_rg_and_rp12()
        call read_sxyz(read_len=n_read_row, sxyz=sxyz)
        rg(:,:,:) = sxyz(:,3,:,:)
        rp1(:,:,:) = sxyz(:,1,:,:)
        rp2(:,:,:) = sxyz(:,2,:,:)
    end subroutine


    subroutine adjust_periodic(vec, cell)
        real(real64),intent(in):: cell
        real(real64),intent(inout):: vec(3)
        real(real64):: hcell

        hcell = cell * 0.5d0

        if (vec(1) > hcell) vec(1)=vec(1)-cell
        if (vec(1) < -hcell) vec(1)=vec(1)+cell

        if (vec(2) > hcell) vec(2)=vec(2)-cell
        if (vec(2) < -hcell) vec(2)=vec(2)+cell

        if (vec(3) > hcell) vec(3)=vec(3)-cell
        if (vec(3) < -hcell) vec(3)=vec(3)+cell
    end subroutine


    subroutine count_gr()
        real(real64):: this_rg(3,np), this_rp1(3,np), this_rp2(3,np)
        real(real64):: rg_i1(3), rp1_i1(3), rp2_i1(3)
        real(real64):: rg_i2(3), rp1_i2(3), rp2_i2(3)
        real(real64):: rgp(3,4), rpp(3,4), rgg(3)
        integer(int32):: idata,id, i1, i2, j

        do idata=1,n_read_row
            this_rg(:,:) = rg(:,:,idata)
            this_rp1(:,:) = rp1(:,:,idata)
            this_rp2(:,:) = rp2(:,:,idata)
            do i1=1,np-1
                rg_i1(:) = this_rg(:,i1)
                rp1_i1(:) = this_rp1(:,i1)
                rp2_i1(:) = this_rp2(:,i1)
                do i2=i1+1,np
                    rg_i2(:) = this_rg(:,i2)
                    rp1_i2(:) = this_rp1(:,i2)
                    rp2_i2(:) = this_rp2(:,i2)

                    ! 重心-重心ベクトル
                    rgg(:) = rg_i1(:) - rg_i2(:)

                    ! 重心-原子ベクトル
                    rgp(:,1) = rg_i1(:) - rp1_i2(:)
                    rgp(:,2) = rg_i1(:) - rp2_i2(:)
                    rgp(:,3) = rg_i2(:) - rp1_i1(:)
                    rgp(:,4) = rg_i2(:) - rp2_i1(:)

                    ! 原子-原子ベクトル
                    rpp(:,1) = rp1_i1(:) - rp1_i2(:)
                    rpp(:,2) = rp1_i1(:) - rp2_i2(:)
                    rpp(:,3) = rp2_i1(:) - rp1_i2(:)
                    rpp(:,4) = rp2_i1(:) - rp2_i2(:)

                    call adjust_periodic(rgg, cell)
                    id = ceiling(norm2(rgg) / dr)
                    if (id <= n_gr_x) gr_gg(id) = gr_gg(id) + 1
                    
                    do j=1,4
                        call adjust_periodic(rgp(:,j), cell)
                        id = ceiling(norm2(rgp(:,j)) / dr)
                        if (id <= n_gr_x) gr_gp(id) = gr_gp(id) + 0.5
                        
                        call adjust_periodic(rpp(:,j), cell)
                        id = ceiling(norm2(rpp(:,j)) / dr)
                        if (id <= n_gr_x) gr_pp(id) = gr_pp(id) + 0.5
                    end do
                end do
            end do
        end do
    end subroutine


    subroutine normalize_gr(gr)
        real(real64), intent(inout):: gr(:)
        integer(int32):: i
        real(real64):: v,num_dens,factor

        gr(:) = gr(:) / dble(ndata*np)
        num_dens = dble(np) / (cell**3)

        factor = 4d0/3d0*pi * dr*dr*dr
        do i=lbound(gr,1), ubound(gr,1)
            v = factor * dble(3*i*(i-1)+1)
            gr(i) = (gr(i) / v) / num_dens  
        end do
    end subroutine
end program main