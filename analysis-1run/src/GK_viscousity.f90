program main
    use,intrinsic :: iso_fortran_env
    use md_condition_for_ana_mod
    use numerical_integration_mod
    use fft_mod
    implicit none
    integer(int32):: ndata
    real(real64), allocatable:: stress(:,:)
    real(real64), allocatable:: acf_stress(:,:)
    real(real64), allocatable:: integ_acf_stress(:,:)
    real(real64):: temp, dt, vol

    call load_condition_for_viscousity_ana(ndata=ndata, dt=dt, vol=vol)
    call read_temp_mean(temp)
    allocate(stress(ndata,3))
    allocate(acf_stress(ndata,3))
    allocate(integ_acf_stress(ndata,3))

    ! 読み込み
    call read_stress(stress, ndata)
    
    ! 計算
    call calc_acf_stress(acf_stress=acf_stress, stress=stress, ndata=ndata, vol=vol, temp=temp)
    call calc_integ_acf_stress(integ_acf_stress=integ_acf_stress, acf_stress=acf_stress, ndata=ndata, dt=dt)

    ! 出力
    call output_acf_stress(acf_stress=acf_stress, ndata=ndata, dt=dt)
    call output_integ_acf_stress(integ_acf_stress=integ_acf_stress, ndata=ndata, dt=dt)
contains
    subroutine read_temp_mean(temp)
        character(100),parameter:: file_temp_mean='temp/temp_mean.dat'
        real(real64):: temp
        integer(int32):: u_temp_mean
        
        open(newunit=u_temp_mean, file=file_temp_mean, status='old')
            read(u_temp_mean, *) temp
        close(u_temp_mean)
    end subroutine


    subroutine read_stress(stress, ndata)
        character(100),parameter:: file_name='../stress.dat'
        real(real64),intent(out):: stress(:,:)
        integer(int32),intent(in):: ndata
        integer(int32):: i,u_stress

        open(newunit=u_stress,file=file_name,status='old')
            read(u_stress,*) (stress(i,:), i=1,ndata)
        close(u_stress)
    end subroutine


    subroutine calc_acf_stress(stress, acf_stress, ndata, vol, temp)
        real(real64),parameter:: kbt = 1.380658d-23
        real(real64),intent(out):: acf_stress(:,:)
        integer(int32),intent(in):: ndata
        real(real64),intent(in):: stress(:,:), vol, temp
        integer(int32):: i
        
        call init_acf(ndata)
        do i=1,3
            acf_stress(:,i) = auto_correlation_function(stress(:,i))
        end do
        acf_stress(:,:) = acf_stress(:,:) / (dble(ndata)*vol*kbt*temp)

    end subroutine


    subroutine calc_integ_acf_stress(integ_acf_stress, acf_stress, ndata, dt)
        real(real64),intent(out):: integ_acf_stress(:,:)
        integer(int32),intent(in):: ndata
        real(real64),intent(in):: acf_stress(:,:), dt
        integer(int32):: i

        do concurrent(i=1:3)
            integ_acf_stress(:,i) = trapezoidal_integration(f=acf_stress(:,i),dx=dt,n=ndata)
        end do
    end subroutine


    subroutine output_acf_stress(acf_stress, ndata, dt)
        character(100),parameter:: file_acf_stress = 'GK_viscousity/acf_stress.dat'
        real(real64),parameter:: inv3=1d0/3d0
        integer(int32),intent(in):: ndata
        real(real64),intent(in):: acf_stress(:,:), dt
        integer(int32):: i, u_acf_stress

        open(newunit=u_acf_stress, file=file_acf_stress, status='replace')
            write(u_acf_stress,'(2e20.10)') (dt*dble(i-1), sum(acf_stress(i,:))*inv3, i=1,ndata)
        close(u_acf_stress)
    end subroutine


    subroutine output_integ_acf_stress(integ_acf_stress, ndata, dt)
        character(100),parameter:: file_integ_acf_stress = 'GK_viscousity/integ_acf_stress.dat'
        real(real64),parameter:: inv3=1d0/3d0
        integer(int32),intent(in):: ndata
        real(real64),intent(in):: integ_acf_stress(:,:), dt
        integer(int32):: i, u_integ_acf_stress

        open(newunit=u_integ_acf_stress, file=file_integ_acf_stress, status='replace')
            write(u_integ_acf_stress,'(2e20.10)') (dt*dble(i-1), sum(integ_acf_stress(i,:))*inv3, i=1,ndata)
        close(u_integ_acf_stress)
    end subroutine
end program