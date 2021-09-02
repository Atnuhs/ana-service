
program main
    use,intrinsic :: iso_fortran_env
    use md_condition_for_ana_mod
    use representative_value_mod
    use mean_square_displacement_mod
    implicit none
    integer(int32),parameter:: np = 500
    integer(int32):: ndata, intd
    real(real64):: dt
    real(real64), allocatable:: tdr(:,:,:)
    real(real64), allocatable:: msd(:,:,:)
    real(real64), allocatable:: mean_msd(:), std_msd(:)

    ! インプットの読み込みなどの準備
    call load_condition_for_msd_ana(ndata, dt, intd)
    allocate(tdr(ndata, np, 3))
    allocate(msd(ndata, np, 3))
    allocate(mean_msd(ndata), std_msd(ndata))
    call init_msd(ndata)
    
    ! 計算
    call read_dxyz(tdr=tdr, ndata=ndata, np=np)
    call calc_msd(msd=msd, tdr=tdr, ndata=ndata, np=np)
    call calc_mean_msd(mean_msd=mean_msd, std_msd=std_msd, msd=msd, ndata=ndata, np=np)

    ! 出力
    call output_mean_msd(mean_msd=mean_msd, std_msd=std_msd, ndata=ndata, dt=dt, intd=intd)
contains
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
