program main
    use, intrinsic:: iso_fortran_env
    use fft_mod
    use md_condition_for_ana_mod
    use numerical_integration_mod
    implicit none
    integer(int32),parameter:: np=500
    real(real64),allocatable:: ej(:,:)
    real(real64),allocatable:: acf_ej(:,:)
    real(real64),allocatable:: integ_acf_ej(:,:)
    integer(int32):: ndata, fst_calc=1000, lst_calc=2000
    real(real64):: temp, dt, vol, thcd


    ! データの読み込み
    call load_condition_for_thcd_ana(ndata=ndata, dt=dt, vol=vol)
    allocate(ej(ndata, 3))
    allocate(acf_ej(ndata, 3))
    allocate(integ_acf_ej(ndata, 3))
    call read_temp_mean(temp)
    call read_tdc(ej=ej, ndata=ndata)

    ! 計算
    call calc_acf_ej(acf_ej=acf_ej, ej=ej, ndata=ndata, vol=vol, temp=temp)
    call calc_integ_acf_ej(integ_acf_ej=integ_acf_ej, acf_ej=acf_ej, ndata=ndata, dt=dt)
    call calc_thcd(integ_acf_ej=integ_acf_ej, thcd=thcd)

    ! 出力
    call output_acf_ej(acf_ej=acf_ej, ndata=ndata, dt=dt)
    call output_integ_acf_ej(integ_acf_ej=integ_acf_ej, ndata=ndata, dt=dt)
    call output_thcd(thcd=thcd)
contains
    subroutine read_temp_mean(temp)
        character(100),parameter:: file_temp_mean='temp/temp_mean.dat'
        real(real64):: temp
        integer(int32):: u_temp_mean
        
        open(newunit=u_temp_mean, file=file_temp_mean, status='old')
            read(u_temp_mean, *) temp
        close(u_temp_mean)
    end subroutine


    subroutine read_tdc(ej, ndata)
        character(100),parameter:: file_tdc='../tdc.dat'
        real(real64),intent(out):: ej(:,:)
        integer(int32),intent(in):: ndata
        integer(int32):: i,u_tdc

        open(newunit=u_tdc,file=file_tdc,status='old')
            read(u_tdc,*) (ej(i,:), i=1,ndata)
        close(u_tdc)
    end subroutine


    subroutine calc_acf_ej(ej, acf_ej, ndata, vol, temp)
        real(real64),parameter:: kbt = 1.380658d-23
        real(real64),intent(out):: acf_ej(:,:)
        integer(int32),intent(in):: ndata
        real(real64),intent(in):: ej(:,:), vol, temp
        integer(int32):: i
        
        call init_acf(ndata)
        do i=1,3
            acf_ej(:,i) = auto_correlation_function(ej(:,i))
        end do
        acf_ej(:,:) = acf_ej(:,:)/(dble(ndata) * vol*kbt*temp*temp)
    end subroutine


    subroutine calc_integ_acf_ej(integ_acf_ej, acf_ej, ndata, dt)
        real(real64),intent(out):: integ_acf_ej(:,:)
        integer(int32),intent(in):: ndata
        real(real64),intent(in):: acf_ej(:,:), dt
        integer(int32):: i

        do concurrent(i=1:3)
            integ_acf_ej(:,i) = trapezoidal_integration(f=acf_ej(:,i),dx=dt,n=ndata)
        end do
    end subroutine


    subroutine calc_thcd(integ_acf_ej, thcd)
        real(real64),intent(in):: integ_acf_ej(:,:)
        real(real64),intent(out):: thcd

        thcd = sum(integ_acf_ej(fst_calc:lst_calc, :)) / dble(lst_calc-fst_calc+1) / 3d0
    end subroutine


    subroutine output_acf_ej(acf_ej, ndata, dt)
        character(100),parameter:: file_acf_ej = 'thcd/acf_ej.dat'
        real(real64),parameter:: inv3=1d0/3d0
        integer(int32),intent(in):: ndata
        real(real64),intent(in):: acf_ej(:,:), dt
        integer(int32):: i, u_acf_ej

        open(newunit=u_acf_ej, file=file_acf_ej, status='replace')
            write(u_acf_ej,'(2e20.10)') (dt*dble(i-1), sum(acf_ej(i,:))*inv3, i=1,ndata)
        close(u_acf_ej)
    end subroutine


    subroutine output_integ_acf_ej(integ_acf_ej, ndata, dt)
        character(100),parameter:: file_integ_acf_ej = 'thcd/integ_acf_ej.dat'
        real(real64),parameter:: inv3=1d0/3d0
        integer(int32),intent(in):: ndata
        real(real64),intent(in):: integ_acf_ej(:,:), dt
        integer(int32):: i, u_integ_acf_ej

        open(newunit=u_integ_acf_ej, file=file_integ_acf_ej, status='replace')
            write(u_integ_acf_ej,'(2e20.10)') (dt*dble(i-1), sum(integ_acf_ej(i,:))*inv3, i=1,ndata)
        close(u_integ_acf_ej)
    end subroutine


    subroutine output_thcd(thcd)
        character(100),parameter:: file_thcd = 'thcd/thcd.dat'
        real(real64),intent(in):: thcd
        integer(int32):: u_thcd

        open(newunit=u_thcd, file=file_thcd, status='replace')
            write(u_thcd,'(e20.10)') thcd
        close(u_thcd)
    end subroutine
end program