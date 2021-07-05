program main
    use,intrinsic :: iso_fortran_env
    use fft_mod
    implicit none
    integer(int32),parameter:: npt=500

    real(real64),parameter:: an =6.0221367d+23
    real(real64),parameter:: kbt=1.380658d-23
    integer(int32):: fst_run, lst_run, all_run
    integer(int32):: fst_calc, lst_calc, all_calc
    integer(int32):: nstep,inte
    integer(int32):: i,run
    real(real64), allocatable:: stress(:,:)
    real(real64), allocatable:: corr_stress(:,:,:)
    real(real64), allocatable:: integ_stress(:,:,:)
    real(real64), allocatable:: corr_stress_mean(:,:)
    real(real64), allocatable:: viscousity(:,:), visc_std(:,:)
    real(real64), allocatable:: run_temp(:)
    real(real64):: eta_mean(4)
    real(real64):: sd(4), se(4), sdr(4), ser(4)
    real(real64):: temp, dens, dt, tmass, vol
    
    call input_condition()

    vol=npt*tmass/dens

    fst_run = 2
    lst_run = 11
    all_run = lst_run-fst_run+1
    fst_calc = 1000
    lst_calc = 1500
    all_calc = lst_calc-fst_calc+1

    allocate(stress(nstep,3))
    allocate(corr_stress(0:nstep-1, 4, fst_run:lst_run), source=0d0)

    do run=fst_run,lst_run
        call read_stress_dat(stress, run)
        do i=1,3
            call liner_correlation(stress(:,i),stress(:,i),corr_stress(:,i,run))
        end do

        do i=0,nstep-1
            corr_stress(i,1:3,run)=corr_stress(i,1:3,run)/dble(nstep-i)
        end do
    enddo

    allocate(corr_stress_mean(0:nstep-1,4))
    corr_stress(:,4,:) = sum(corr_stress(:,1:3,:),dim=2)/3d0
    corr_stress_mean(:,:) = sum(corr_stress(:,:,:), dim=3)/dble(all_run)
    open(UNIT=3,FILE='acfstress_totl.dat',STATUS='replace')
        do i=0,nstep-1
            write(3,'(e13.4,4e13.5)') dble(i)*dt, corr_stress_mean(i,:)
        end do
    close(3)
    deallocate(corr_stress_mean)
    
    allocate(integ_stress(nstep,4,fst_run:lst_run), source=0d0)
    do run=fst_run,lst_run
        do i=1,4
            integ_stress(:,i,run) = trapezoidal_integration(corr_stress(:,i,run),dt,nstep)
        end do
    end do

    allocate(run_temp(fst_run:lst_run))
    do run=fst_run, lst_run
        run_temp(run) = read_temp(run,nstep,inte)
    end do

    do run=fst_run, lst_run
        integ_stress(:,:,run) = integ_stress(:,:,run)/(vol*kbt*run_temp(run))
    end do
    deallocate(run_temp)

    open(UNIT=10,FILE='integvisc_totl.dat',STATUS='replace')
    do i=1,lst_calc
        block
            real(real64):: integ_stress_mean(4)
            integ_stress_mean(:) = sum(integ_stress(i,:,:),dim=2)/dble(all_run)
            write(10,'(e13.4,4e13.5)') dble(i-1)*dt, integ_stress_mean(:)
        end block
    end do
    close(10)

    allocate(viscousity(4,fst_run:lst_run), source=0d0)
    allocate(visc_std(4,fst_run:lst_run), source=0d0)

    viscousity(:,:) = sum(integ_stress(fst_calc:lst_calc,:,:),dim=1)/dble(all_calc)
    visc_std(:,:) = sum(integ_stress(fst_calc:lst_calc,:,:)**2,dim=1)/dble(all_calc) - viscousity(:,:)**2

    open(unit=11, file='viscousity_run_mean.dat')
    open(unit=12, file='viscousity_run_total.dat')
        do run=fst_run,lst_run
            block
                real(real64):: visc_mean, visc_mean_std, visc_mean_stdr
                visc_mean = viscousity(4,run)
                visc_mean_std = visc_std(4,run)
                visc_mean_stdr = abs(visc_mean_std/visc_mean*1d2)
                write(11,'(i0,100(e15.6))') run, viscousity(:,run), visc_std(:,run)
                write(12,'(i0, 2e15.6, f6.2)') run, visc_mean, visc_mean_std, visc_mean_stdr 
            end block
        end do
    close(11)
    close(12)
    
    do i=1,4
        call calc_std(viscousity(i,:), eta_mean(i), sd(i), se(i), sdr(i), ser(i))
    end do

    open(UNIT=9,FILE='viscosity_totl.dat',STATUS='replace')
    block
        character(35):: fmt = '(A,F10.5,3X,A,F10.5,A,A,F10.2,A)'
        write(9,'(a)')' --- --- vis_result --- ---'
        write(9,*)' '
        write(9,*)' MD infomation .....'
        write(9,'(A20,F8.2,A7)')'density = ',dens,' kg tmass-3'
        write(9,'(A20,F8.2,A2)')'temperature = ',temp,' run'
        write(9,'(A20,I8)')  'nstep = ',nstep
        write(9,'(A20,F8.2,A3)')'time step = ',dt*1e15,' fs'
        write(9,'(A20,F8.2,A3)')'summation time = ',lst_calc*dt*1e12,' ps'
        write(9,'(A20,F8.2,A3)')'cutoff time = ',fst_calc*dt*1e12,' ps'
        write(9,'(A20,I4)')'# of Run = ',all_run
        write(9,*)' '
        write(9,'(A40)')'viscosity, eta [unit: 10^-4 kg/(tmass s)]'
        write(9,fmt) 'eta(xy) = ',eta_mean(1)*1d4,'(SE = ',se(1)*1d4,',','SER = ',ser(1)*1d2,' %)'
        write(9,fmt) 'eta(yz) = ',eta_mean(2)*1d4,'(SE = ',se(2)*1d4,',','SER = ',ser(2)*1d2,' %)'
        write(9,fmt) 'eta(zx) = ',eta_mean(3)*1d4,'(SE = ',se(3)*1d4,',','SER = ',ser(3)*1d2,' %)'
        write(9,fmt)'eta(all) = ',eta_mean(4)*1d4,'(SE = ',se(4)*1d4,',','SER = ',ser(4)*1d2,' %)'
        write(9,'(A)') '---------------------- (reference) ----------------------'
        write(9,fmt) 'eta(xy) = ',eta_mean(1)*1d4,'(SD = ',sd(1)*1d4,',','SDR = ',sdr(1)*1d2,' %)'
        write(9,fmt) 'eta(yz) = ',eta_mean(2)*1d4,'(SD = ',sd(2)*1d4,',','SDR = ',sdr(2)*1d2,' %)'
        write(9,fmt) 'eta(zx) = ',eta_mean(3)*1d4,'(SD = ',sd(3)*1d4,',','SDR = ',sdr(3)*1d2,' %)'
        write(9,fmt)'eta(all) = ',eta_mean(4)*1d4,'(SD = ',sd(4)*1d4,',','SDR = ',sdr(4)*1d2,' %)'
    end block
contains
    subroutine input_condition()
        real(real64):: r_temp, coex_temp, atomic_mass(3)
        real(real64):: r_mass, r_nd

        open(unit=100,file='../mdda.inpt',status='old')
        read(100,*) nstep
        read(100,*) temp
        read(100,*) dens
        read(100,*) dt
        read(100,*) !rc
        read(100,*) !intr
        read(100,*) !intv
        read(100,*) !intd
        read(100,*) inte
        close(100)

        open(unit=101,file='../inpt2.dat',status='old')
            read(101,*) ! r_elongation
            read(101,*) ! r_bond_length
            read(101,*) ! r_moment_of_inertia
            read(101,*) ! r_packing_fraction
            read(101,*) r_temp
            read(101,*) coex_temp
            read(101,*) ! r_eps
            read(101,*) r_mass
            read(101,*) r_nd
        close(101)

        open(unit=102,file='../molecular.inpt',status='old')
            read(102,*) atomic_mass(:)
        close(102)

        print'(a)', '--- from ../mdda.inpt'
        print'(a,5x,i0)','nstep: ',   nstep 
        print'(a,5x,f0.3)', 'temp: ', temp
        print'(a,5x,f0.3)', 'dens: ', dens
        print'(a,5x,e10.3)', 'dt: ',  dt
        print*, ''

        print'(a)', '--- from ../inpt2.dat'
        print'(a,5x,f0.2)','coex_temp: ', coex_temp 
        print'(a,5x,f0.2)','r_temp: ', r_temp
        print'(a,5x,f0.2)','coex_temp*r_temp: ', coex_temp * r_temp
        ! 古いタイプのcoex_tempの仕様にも適用出来るはずだがやや自信なし
        if (coex_temp /= 1) temp=coex_temp
        temp=temp*r_temp
        dens=dens*r_nd
        print'(a,5x,f0.2)','use_temp: ', temp
        print*, ''

        print'(a)', '--- from ../molecular.inpt'
        print'(a,5x,3(f0.5,1x))','atomic_mass: ', atomic_mass(:)
        tmass = sum(atomic_mass) ! *r_mass
        print'(a,5x,f0.3)','tmass: ', tmass
        tmass=tmass/an/1000d0 ! g/mol => g => kg
    end subroutine

    subroutine read_stress_dat(stress,current_run)
        real(real64),intent(out):: stress(:,:)
        integer(int32):: current_run, i
        character(2):: c_run
        character(19):: file_name

        write(c_run,'(i2.2)') current_run
        file_name = '../run'//c_run//'/stress.dat'
        print'(2a)', "Let's load ", file_name
        
        open(unit=1,file=file_name,status='old')
        do i=1,nstep
            read(1,*) stress(i,1), stress(i,2), stress(i,3)
        end do
        close(1)
    end subroutine

    subroutine calc_std(ar, ave, std,se,stdr,ser)
        real(real64),intent(in):: ar(:)
        real(real64),intent(out):: ave,std,se,stdr,ser
        real(real64):: ave2

        ave = sum(ar)/dble(all_run)
        ave2 = sum(ar**2)/dble(all_run)
        std=dsqrt(ave2-ave**2)
        stdr=std/ave
        se=std/dsqrt(dble(all_run-1))
        ser=se/ave
    end subroutine

    function trapezoidal_integration(f,dx,n) result(g)
        real(real64),intent(in):: f(n), dx
        integer(int32),intent(in):: n
        real(real64):: g(n)
        integer(int32):: i

        g(1)=0d0
        do i=2,n
            g(i) = g(i-1) + sum(f(i-1:i))
        end do 
        g(:)=g(:)*5d-1*dx
    end function

    function read_temp(run, ndata, inte) result(ret)
        integer(int32),intent(in):: run, ndata, inte
        real(real64):: ret
        real(real64), allocatable:: temps(:)
        integer(int32):: num_temp
        character(2):: c_run
        character(:),allocatable:: file_name

        num_temp = ndata/inte
        allocate(temps(num_temp))
        write(c_run,'(i2.2)') run
        file_name = '../run'//c_run//'/enrg.dat'
        open(unit=1,file=file_name,status='old')
        do i=1,num_temp
            block
                integer(int32):: t1
                real(real64):: t2,t3,t4,t5
                read(1,*) t1, temps(i),t2,t3,t4,t5
            end block
        end do
        close(1)
        ret = sum(temps(:))/dble(num_temp)
    end function
end program