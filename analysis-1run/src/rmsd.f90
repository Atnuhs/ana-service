
program main
    use,intrinsic :: iso_fortran_env
    use md_condition_for_ana_mod
    use representative_value_mod
    use mean_square_displacement_mod
    implicit none
    integer(int32),parameter:: np = 500
    integer(int32):: ndata, intd
    real(real64):: dt, rdc, temp0, temp
    real(real64), allocatable:: tdphi(:,:,:)
    real(real64), allocatable:: rmsd(:,:,:)
    real(real64), allocatable:: mean_rmsd(:), se_rmsd(:)

    ! インプットの読み込みなどの準備
    call load_condition_for_rmsd_ana(ndata, dt, intd, temp0)
    allocate(tdphi(ndata, np, 3))
    allocate(rmsd(ndata, np, 3))
    allocate(mean_rmsd(ndata), se_rmsd(ndata))
    call init_msd(ndata)
    call read_temp_mean(temp=temp)

    ! 計算
    call read_tdphi(tdphi=tdphi, ndata=ndata, np=np)
    call calc_rmsd(rmsd=rmsd, tdphi=tdphi, ndata=ndata, np=np)
    call calc_mean_rmsd(mean_rmsd=mean_rmsd, se_rmsd=se_rmsd, rmsd=rmsd, ndata=ndata, np=np)
    call calc_rdc(ndata=ndata, dt=dt, intd=intd, temp=temp, temp0=temp0, &
    & mean_rmsd=mean_rmsd, rdc=rdc)

    ! 出力
    call output_mean_rmsd(mean_rmsd=mean_rmsd, se_rmsd=se_rmsd, ndata=ndata, dt=dt, intd=intd)
    call output_diffusion_coefficient(rdc=rdc)
contains
    subroutine read_temp_mean(temp)
        character(100),parameter:: file_temp_mean='temp/temp_mean.dat'
        real(real64):: temp
        integer(int32):: u_temp_mean
        
        open(newunit=u_temp_mean, file=file_temp_mean, status='old')
            read(u_temp_mean, *) temp
        close(u_temp_mean)
    end subroutine

    
    subroutine read_tdphi(tdphi, ndata, np)
        ! tdphiのデータ読み込み
        character(100),parameter:: file_tdphi='../tdphi.dat'
        real(real64),intent(out):: tdphi(:,:,:)
        integer(int32),intent(in):: np, ndata
        integer(int32):: u_tdphi, i, j, j1, j2, c

        open(newunit=u_tdphi, file=file_tdphi, status='old')
            c=0
            do i=1,ndata
                read(u_tdphi,*)
                c=c+1
                do j=1,np/2
                    j1 = 2*j-1
                    j2 = 2*j
                    read(u_tdphi,*) tdphi(i,j1,1), tdphi(i,j1,2), &
                        & tdphi(i,j1,3), tdphi(i,j2,1), tdphi(i,j2,2), tdphi(i,j2,3)
                    c=c+1
                    ! print*, c
                end do
            end do
        close(u_tdphi)
    end subroutine


    subroutine calc_rmsd(rmsd, tdphi, ndata, np)
        ! MSDを自己相関関数、累積和を用いて計算O(N)
        real(real64),intent(out):: rmsd(:,:,:)
        real(real64),intent(in):: tdphi(:,:,:)
        integer(int32),intent(in):: ndata, np
        integer(int32):: i,j

        do i=1,3
            do j=1,np
                call calc_mean_square_displacement(rmsd(:,j,i), tdphi(:,j,i), ndata)
            end do
        end do
    end subroutine


    subroutine calc_mean_rmsd(mean_rmsd, se_rmsd, rmsd, ndata, np)
        ! 各時間でのmsdの
        real(real64),intent(out):: mean_rmsd(:), se_rmsd(:)
        integer(int32),intent(in):: ndata, np
        real(real64),intent(in):: rmsd(:,:,:)
        integer(int32):: i
        real(real64),allocatable:: tmp_ar(:)
        real(real64):: se

        allocate(tmp_ar(np))
        do i=1,ndata
            tmp_ar(:) = sum(rmsd(i,:,:), dim=2)
            mean_rmsd(i) = mean(arr=tmp_ar, se=se)
            se_rmsd(i) = se
        end do
    end subroutine


    subroutine calc_rdc(ndata, dt, intd, temp, temp0, mean_rmsd, rdc)
        real(real64),parameter:: kb = 1.38064852e-23
        real(real64),parameter:: pi = acos(-1d0)
        real(real64),parameter:: zeta = 2.837297d0

        real(real64),intent(out):: rdc
        integer(int32),intent(in):: ndata, intd
        real(real64),intent(in):: mean_rmsd(ndata), dt, temp0, temp

        integer(int32):: l,r,n,i
        real(real64):: a, x(ndata), rdc_temp

        x(:) = [(dt*dble(intd*(i-1)), i=1,ndata)]
        l = ndata/10
        r = ndata/4
        n = r-l+1
        call least_squares_method(n=n, x=x(l:r), y=mean_rmsd(l:r), a=a)
        rdc_temp = a / 4d0
        rdc = rdc_temp * (temp0/temp) ! 温度補正
    end subroutine


    subroutine output_mean_rmsd(mean_rmsd, se_rmsd, ndata, dt, intd)
        character(100),parameter:: file_mean_msd='rmsd/mean_rmsd.dat'
        integer(int32),intent(in):: ndata, intd
        real(real64),intent(in):: mean_rmsd(:), se_rmsd(:), dt
        integer(int32):: u_mean_msd, i

        open(newunit=u_mean_msd, file=file_mean_msd, status='replace')
            write(u_mean_msd,'(3(e13.5))')(dt*dble((i-1)*intd), mean_rmsd(i), se_rmsd(i), i=1,ndata)
        close(u_mean_msd)
    end subroutine


    subroutine output_diffusion_coefficient(rdc)
        character(100),parameter:: file_rdc='rmsd/rotational_diffusion_coefficient.dat'
        real(real64),intent(in):: rdc
        integer(int32):: u_rdc

        open(newunit=u_rdc, file=file_rdc, status='replace')
            write(u_rdc,'(e13.5)') rdc
        close(u_rdc)
    end subroutine
end program
