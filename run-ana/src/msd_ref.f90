
program main
    use,intrinsic :: iso_fortran_env
    use read_condition_mod
    use fft_mod
    use representative_value_mod
    implicit none
    integer(int32),parameter:: np = 500
    integer(int32):: ndata, intd
    real(real64):: dt
    real(real64), allocatable:: tdr(:,:,:)
    real(real64), allocatable:: acf_tdr(:,:,:)
    real(real64), allocatable:: cs_sq_tdr(:,:,:)
    real(real64), allocatable:: msd(:,:,:)
    real(real64), allocatable:: mean_msd(:), std_msd(:)

    ! インプットの読み込みなどの準備
    call input_condition(ndata=ndata, dt=dt, intd=intd)
    allocate(tdr(ndata, np, 3))
    allocate(acf_tdr(ndata, np, 3))
    allocate(cs_sq_tdr(0:ndata, np, 3))
    allocate(msd(ndata, np, 3))
    allocate(mean_msd(ndata), std_msd(ndata))

    ! 計算と出力
    call read_dxyz(tdr=tdr, ndata=ndata, np=np)
    call calc_acf_tdr(acf_tdr=acf_tdr, tdr=tdr, ndata=ndata, np=np)
    call calc_cumlutive_sum_square(cs_sq_tdr=cs_sq_tdr, tdr=tdr, ndata=ndata, np=np)
    call calc_mean_square_displacement(msd=msd, cs_sq_tdr=cs_sq_tdr, acf_tdr=acf_tdr, ndata=ndata, np=np)
    call calc_mean_msd(mean_msd=mean_msd, std_msd=std_msd, msd=msd, ndata=ndata, np=np)
    call output_mean_msd(mean_msd=mean_msd, std_msd=std_msd, ndata=ndata, dt=dt, intd=intd)
contains
    subroutine input_condition(ndata, dt, intd)
        integer(int32),intent(out):: ndata, intd
        real(real64),intent(out):: dt
        type(mdda_type):: mdda

        call read_mdda(mdda)
        ndata = mdda%nstep/mdda%intd
        dt = mdda%dt
        intd = mdda%intd
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


    subroutine calc_acf_tdr(acf_tdr, tdr, ndata, np)
        ! tdrの自己相関関数を計算O(N log N)
        real(real64),intent(out):: acf_tdr(:,:,:)
        integer(int32),intent(in):: ndata, np
        real(real64),intent(in):: tdr(:,:,:)
        integer(int32):: i,j

        call init_acf(array_size=ndata)
        do i=1,3
            do j=1,np
                acf_tdr(:,j,i) = auto_correlation_function(tdr(:,j,i))
            end do
        end do
    end subroutine


    subroutine calc_cumlutive_sum_square(cs_sq_tdr, tdr, ndata, np)
        ! 「tdrの自乗」の累積和を計算O(N)
        real(real64),intent(out):: cs_sq_tdr(0:,:,:)
        integer(int32),intent(in):: ndata, np
        real(real64),intent(in):: tdr(:,:,:)
        integer(int32):: i,j,k

        cs_sq_tdr(0,:,:) = 0d0
        do concurrent(i=1:3, j=1:np)
            do k=1,ndata
                cs_sq_tdr(k,j,i) = cs_sq_tdr(k-1,j,i) + tdr(k,j,i)*tdr(k,j,i)
            end do
        end do
    end subroutine


    subroutine calc_mean_square_displacement(msd, cs_sq_tdr, acf_tdr, ndata, np)
        ! MSDを自己相関関数、累積和を用いて計算O(N)
        real(real64),intent(out):: msd(:,:,:)
        real(real64),intent(in):: cs_sq_tdr(0:,:,:), acf_tdr(:,:,:)
        integer(int32),intent(in):: ndata, np
        real(real64):: cs1, cs2
        integer(int32):: i,j,k

        do concurrent(i=1:3, j=1:np, k=0:ndata-1)
            cs1 = cs_sq_tdr(ndata-k,j,i)
            cs2 = cs_sq_tdr(ndata,j,i) - cs_sq_tdr(k,j,i)
            msd(k+1,j,i) = (cs1+cs2-2*acf_tdr(k+1,j,i))/dble(ndata-k)
        end do
    end subroutine


    subroutine calc_mean_msd(mean_msd, std_msd, msd, ndata, np)
        ! 各時間でのmsdの
        real(real64),intent(out):: mean_msd(:), std_msd(:)
        integer(int32),intent(in):: ndata, np
        real(real64),intent(in):: msd(:,:,:)
        integer(int32):: i,j,k
        real(real64),allocatable:: tmp_ar(:)
        real(real64):: std

        allocate(tmp_ar(np*3))
        do i=1,ndata
            tmp_ar(:) = [((msd(i,j,k), j=1,np), k=1,3)]
            mean_msd(i) = mean(arr=tmp_ar, se=std)
            std_msd(i) = std
        end do
    end subroutine


    subroutine output_mean_msd(mean_msd, std_msd, ndata, dt, intd)
        character(100),parameter:: file_mean_msd='msd/mean_msd.dat'
        integer(int32),intent(in):: ndata, intd
        real(real64),intent(in):: mean_msd(:), std_msd(:), dt
        integer(int32):: u_mean_msd, i

        open(newunit=u_mean_msd, file=file_mean_msd, status='replace')
            write(u_mean_msd,'(3(e13.5))')(dt*dble((i-1)*intd), mean_msd(i), std_msd(i), i=1,ndata)
        close(u_mean_msd)
    end subroutine
end program
