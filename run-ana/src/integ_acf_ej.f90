program main
    use, intrinsic:: iso_fortran_env
    use fft_mod
    use read_condition_mod
    implicit none
    integer(int32),parameter:: np=500
    real(real64),allocatable:: ej(:,:)
    real(real64),allocatable:: acf_ej(:,:)
    real(real64),allocatable:: integ_acf_ej(:,:)
    integer(int32):: ndata
    real(real64):: temp, dt, vol


    ! データの読み込み
    call input_condition(ndata=ndata, dt=dt, vol=vol)
    call read_temp_mean(temp)
    allocate(ej(ndata, 3))
    allocate(acf_ej(ndata, 3))
    allocate(integ_acf_ej(ndata, 3))

    call read_tdc(ej=ej, ndata=ndata)
    call calc_acf_ej(acf_ej=acf_ej, ej=ej, ndata=ndata, vol=vol, temp=temp)
    call calc_integ_acf_ej(integ_acf_ej=integ_acf_ej, acf_ej=acf_ej, ndata=ndata, dt=dt)
    call output_acf_ej(acf_ej=acf_ej, ndata=ndata, dt=dt)
    call output_integ_acf_ej(integ_acf_ej=integ_acf_ej, ndata=ndata, dt=dt)
contains
    subroutine input_condition(ndata,dt,vol)
        real(real64),parameter:: an=6.0221367d+23
        integer(int32),intent(out):: ndata
        real(real64),intent(out):: dt, vol
        type(mdda_type):: mdda
        type(rate_type):: rate
        type(molecular_type):: molecular
        real(real64):: dens, tmass

        call read_condition(mdda, rate, molecular)

        ndata = mdda%nstep
        dens = mdda%dens*rate%nd
        dt = mdda%dt
        tmass = sum(molecular%mass(:))!*r_mass
        tmass = tmass/an/1000d0 ! g/mol => g => kg
        vol = np*tmass/dens
    end subroutine


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


    pure function trapezoidal_integration(f,dx,n) result(g)
        real(real64),intent(in):: f(:), dx
        integer(int32),intent(in):: n
        real(real64):: g(n)
        integer(int32):: i

        g(1)=0d0
        do i=2,n
            g(i) = g(i-1) + f(i-1) + f(i)
        end do 
        g(:)=g(:)*5d-1*dx
    end function


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
end program