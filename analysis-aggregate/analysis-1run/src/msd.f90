
program main
    use,intrinsic :: iso_fortran_env
    use md_condition_for_ana_mod
    use representative_value_mod
    use mean_square_displacement_mod
    implicit none
    integer(int32),parameter:: np = 500
    integer(int32):: ndata, intd
    real(real64):: dt, tdc, temp0, temp, cell, viscousity
    real(real64), allocatable:: tdr(:,:,:)
    real(real64), allocatable:: msd(:,:,:)
    real(real64), allocatable:: msd_mean(:), msd_se(:)

    ! インプットの読み込みなどの準備
    call load_condition_for_msd_ana(ndata, dt, intd, temp0, cell)
    allocate(tdr(ndata, np, 3))
    allocate(msd(ndata, np, 3))
    allocate(msd_mean(ndata), msd_se(ndata))
    call init_msd(ndata)
    call read_temp_mean(temp=temp)
    call read_viscousity(viscousity=viscousity)
    
    ! 計算
    call read_dxyz(tdr=tdr, ndata=ndata, np=np)
    call calc_msd(msd=msd, tdr=tdr, ndata=ndata, np=np)
    call calc_msd_mean(msd_mean=msd_mean, msd_se=msd_se, msd=msd, ndata=ndata, np=np)
    call calc_tdc(ndata=ndata, dt=dt, intd=intd, temp=temp, temp0=temp0, &
    & cell=cell, viscousity=viscousity, msd_mean=msd_mean, tdc=tdc)

    ! 出力
    call output_msd_mean(msd_mean=msd_mean, msd_se=msd_se, ndata=ndata, dt=dt, intd=intd)
    call output_diffusion_coefficient(tdc=tdc)
contains
    subroutine read_temp_mean(temp)
        character(100),parameter:: file_temp_mean='temp/temp_mean.dat'
        real(real64):: temp
        integer(int32):: u_temp_mean
        
        open(newunit=u_temp_mean, file=file_temp_mean, status='old')
            read(u_temp_mean, *) temp
        close(u_temp_mean)
    end subroutine


    subroutine read_viscousity(viscousity)
        character(100),parameter:: file_viscousity='viscousity/viscousity.dat'
        real(real64):: viscousity
        integer(int32):: u_viscousity
        
        open(newunit=u_viscousity, file=file_viscousity, status='old')
            read(u_viscousity, *) viscousity
        close(u_viscousity)
    end subroutine


    subroutine read_dxyz(tdr, ndata, np)
        ! dxyzのデータ読み込み
        character(100),parameter:: file_dxyz='../dxyz.dat'
        real(real64),intent(out):: tdr(:,:,:)
        integer(int32),intent(in):: np, ndata
        integer(int32):: u_dxyz, i, j, k

        open(newunit=u_dxyz, file=file_dxyz, status='old')
        read(u_dxyz,*)
            do i=1,ndata
                read(u_dxyz,*)
                read(u_dxyz,*) ((tdr(i,j,k), k=1,3), j=1,np)
            end do
        close(u_dxyz)
    end subroutine


    subroutine calc_msd(msd, tdr, ndata, np)
        ! MSDを自己相関関数、累積和を用いて計算O(N)
        real(real64),intent(out):: msd(:,:,:)
        real(real64),intent(in):: tdr(:,:,:)
        integer(int32),intent(in):: ndata, np
        integer(int32):: i,j

        do i=1,3
            do j=1,np
                call calc_mean_square_displacement(msd(:,j,i), tdr(:,j,i), ndata)
            end do
        end do
    end subroutine


    subroutine calc_msd_mean(msd_mean, msd_se, msd, ndata, np)
        ! 各時間でのmsdの
        real(real64),intent(out):: msd_mean(:), msd_se(:)
        integer(int32),intent(in):: ndata, np
        real(real64),intent(in):: msd(:,:,:)
        integer(int32):: i
        real(real64),allocatable:: tmp_ar(:)
        real(real64):: se

        allocate(tmp_ar(np))
        do i=1,ndata
            tmp_ar(:) = sum(msd(i,:,:), dim=2)
            msd_mean(i) = mean(arr=tmp_ar, se=se)
            msd_se(i) = se
        end do
    end subroutine


    subroutine calc_tdc(ndata, dt, intd, temp, temp0, cell, viscousity, msd_mean, tdc)
        real(real64),parameter:: kb = 1.38064852e-23
        real(real64),parameter:: pi = acos(-1d0)
        real(real64),parameter:: zeta = 2.837297d0

        real(real64),intent(out):: tdc
        integer(int32),intent(in):: ndata, intd
        real(real64),intent(in):: msd_mean(ndata), dt, temp0, temp, cell, viscousity

        integer(int32):: l,r,n,i
        real(real64):: a, x(ndata), tdc_pbc, tdc_temp

        x(:) = [(dt*dble(intd*(i-1)), i=1,ndata)]
        l = ndata/10
        r = ndata/4
        n = r-l+1
        call least_squares_method(n=n, x=x(l:r), y=msd_mean(l:r), a=a)
        tdc_pbc = a / 6d0
        tdc_temp = tdc_pbc + (kb*temp*zeta)/(6d0*pi*cell*viscousity) ! セルサイズ補正
        tdc = tdc_temp * (temp0/temp) ! 温度補正
    end subroutine


    subroutine output_msd_mean(msd_mean, msd_se, ndata, dt, intd)
        character(100),parameter:: file_msd_mean='msd/msd_mean.dat'
        integer(int32),intent(in):: ndata, intd
        real(real64),intent(in):: msd_mean(:), msd_se(:), dt
        integer(int32):: u_msd_mean, i

        open(newunit=u_msd_mean, file=file_msd_mean, status='replace')
            write(u_msd_mean,'(3(e13.5))')(dt*dble((i-1)*intd), msd_mean(i), msd_se(i), i=1,ndata)
        close(u_msd_mean)
    end subroutine


    subroutine output_diffusion_coefficient(tdc)
        character(100),parameter:: file_tdc='msd/translational_diffusion_coefficient.dat'
        real(real64),intent(in):: tdc
        integer(int32):: u_tdc

        open(newunit=u_tdc, file=file_tdc, status='replace')
            write(u_tdc,'(e13.5)') tdc
        close(u_tdc)
    end subroutine
end program
