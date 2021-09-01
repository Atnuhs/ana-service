program main
    use,intrinsic :: iso_fortran_env
    use fft_mod
    use read_condition_mod
    implicit none
    integer(int32),parameter:: np=500
    integer(int32):: fst_calc, lst_calc, all_calc, ndata
    real(real64), allocatable:: stress(:,:)
    real(real64), allocatable:: acf_stress(:,:)
    real(real64), allocatable:: integ_acf_stress(:,:)
    real(real64):: temp, dt, vol, viscousity(3)

    fst_calc = 1000
    lst_calc = 1500
    all_calc = lst_calc-fst_calc+1
    call input_condition(ndata=ndata, dt=dt, vol=vol)
    call read_temp_mean(temp)
    allocate(stress(ndata,3))
    allocate(acf_stress(ndata,3))
    allocate(integ_acf_stress(ndata,3))

    ! reading
    call read_stress(stress, ndata)
    
    ! calc
    call calc_acf_stress(acf_stress=acf_stress, stress=stress, ndata=ndata, vol=vol, temp=temp)
    call calc_integ_acf_stress(integ_acf_stress=integ_acf_stress, acf_stress=acf_stress, ndata=ndata, dt=dt)
    call calc_viscousity(integ_acf_stress=integ_acf_stress, viscousity=viscousity, fst_calc=fst_calc, lst_calc=lst_calc)

    ! output
    call output_acf_stress(acf_stress=acf_stress, ndata=ndata, dt=dt)
    call output_integ_acf_stress(integ_acf_stress=integ_acf_stress, ndata=ndata, dt=dt)
    call output_viscousity(viscousity=viscousity)

    ! end
contains
    subroutine input_condition(ndata, dt, vol)
        real(real64),parameter:: an=6.0221367d+23
        integer(int32),intent(out):: ndata
        real(real64),intent(out):: dt, vol
        type(condition_type):: condition
        type(rate_type):: rate
        type(molecular_type):: molecular
        real(real64):: dens, tmass
        
        call read_input(condition, rate, molecular)

        ndata = condition%nstep
        dens = condition%dens*rate%nd
        dt = condition%dt
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


    subroutine calc_viscousity(integ_acf_stress, viscousity, fst_calc, lst_calc)
        integer(int32),intent(in):: fst_calc, lst_calc
        real(real64),intent(in):: integ_acf_stress(:,:)
        real(real64),intent(out):: viscousity(:)
        integer(int32):: all_calc

        all_calc=lst_calc-fst_calc+1
        viscousity(:) = sum(integ_acf_stress(fst_calc:lst_calc, :), dim=1) / dble(all_calc)
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


    subroutine output_acf_stress(acf_stress, ndata, dt)
        character(100),parameter:: file_acf_stress = 'viscousity/acf_stress.dat'
        real(real64),parameter:: inv3=1d0/3d0
        integer(int32),intent(in):: ndata
        real(real64),intent(in):: acf_stress(:,:), dt
        integer(int32):: i, u_acf_stress

        open(newunit=u_acf_stress, file=file_acf_stress, status='replace')
            write(u_acf_stress,'(2e20.10)') (dt*dble(i-1), sum(acf_stress(i,:))*inv3, i=1,ndata)
        close(u_acf_stress)
    end subroutine


    subroutine output_integ_acf_stress(integ_acf_stress, ndata, dt)
        character(100),parameter:: file_integ_acf_stress = 'viscousity/integ_acf_stress.dat'
        real(real64),parameter:: inv3=1d0/3d0
        integer(int32),intent(in):: ndata
        real(real64),intent(in):: integ_acf_stress(:,:), dt
        integer(int32):: i, u_integ_acf_stress

        open(newunit=u_integ_acf_stress, file=file_integ_acf_stress, status='replace')
            write(u_integ_acf_stress,'(2e20.10)') (dt*dble(i-1), sum(integ_acf_stress(i,:))*inv3, i=1,ndata)
        close(u_integ_acf_stress)
    end subroutine


    subroutine output_viscousity(viscousity)
        character(100),parameter:: file_viscousity = 'viscousity/viscousity.dat'
        real(real64),intent(in):: viscousity(:)
        integer(int32):: u_viscousity

        open(newunit=u_viscousity, file=file_viscousity, status='replace')
            write(u_viscousity,'(e20.10)') sum(viscousity(:)) / 3d0
        close(u_viscousity)
    end subroutine
end program