program main
    use, intrinsic:: iso_fortran_env
    use fft_mod
    implicit none
    real(real64),parameter:: an=6.0221367d+23
    real(real64),parameter:: kbt = 1.380658d-23
    real(real64),parameter:: inv_3 = 1d0/3d0
    real(real64),allocatable:: ejx_run(:),ejy_run(:),ejz_run(:) ! array size -> ndata
    real(real64),allocatable:: ac_ej_run(:,:), tc_run(:), tc_sd_run(:), integ_run(:,:)
    real(real64),allocatable:: run_temp(:)
    integer(int32):: ndata, npt, inte
    integer(int32):: num_calc, fst_c, lst_c
    integer(int32):: num_run, fst_run, lst_run
    integer(int32):: i,run
    real(real64):: dens, temp, dt
    real(real64):: tmass, vol
    real(real64):: tcsq, tc, tcsd, tcse
    real(real64)::tcsdr, tcser 

    fst_c=1000 ! step
    lst_c=1500 ! step
    num_calc=lst_c-fst_c+1
    fst_run = 2
    lst_run = 11
    num_run=lst_run-fst_run+1
    npt=500
    call input_condition()
    vol=npt*tmass/dens




    ! time correlation function ----------------------------------------------------

    allocate(ejx_run(ndata), ejy_run(ndata), ejz_run(ndata))
    allocate(ac_ej_run(0:ndata-1, fst_run:lst_run))

    do run=fst_run,lst_run
        call read_tcd_dat(ejx_run, ejy_run, ejz_run, run)
        block
            real(real64):: cx(ndata), cy(ndata), cz(ndata)
            call liner_correlation(ejx_run, ejx_run,cx)
            call liner_correlation(ejy_run, ejy_run,cy)
            call liner_correlation(ejz_run, ejz_run,cz)
            ac_ej_run(0:ndata-1,run) = (cx(:)+cy(:)+cz(:))/3d0
        end block

        do i=0,ndata-1
            ac_ej_run(i,run)=ac_ej_run(i,run)/dble(ndata-i)
        end do
    end do
    ! read temp at enrg.dat by run
    allocate(run_temp(fst_run:lst_run))
    do run=fst_run, lst_run
        run_temp(run) = read_temp(run,ndata,inte)
        ! print*, run, run_temp(run)
    end do
    ! run_temp(:) = temp

    do run=fst_run,lst_run
        ac_ej_run(:,run) = ac_ej_run(:,run)/(vol*kbt*run_temp(run)**2)
    end do
    
    open(unit=11,file='result_tc/cfee.dat',status='replace')
        do i=0,lst_c-1
            write(11,'(100(e15.6))') dble(i)*dt, ac_ej_run(i,:)
        end do
    close(11)

    ! integration ----------------------------------------------------------------

    allocate(integ_run(ndata, fst_run:lst_run), source=0d0)
    allocate(tc_run(fst_run:lst_run),tc_sd_run(fst_run:lst_run))

    do run=fst_run,lst_run
        integ_run(1,run) = 0
        do i=2,ndata-1
                integ_run(i,run)=integ_run(i-1,run)+sum(ac_ej_run(i-2:i-1,run))
        end do
        integ_run(:,run)=integ_run(:,run)*dt*5d-1
        tc_run(run)=sum(integ_run(fst_c:lst_c,run))/dble(num_calc)
        tc_sd_run(run) = sum(integ_run(fst_c:lst_c,run)**2)/dble(num_calc) - tc_run(run)**2
    end do
    deallocate(ac_ej_run)

    open(unit=12, file='result_tc/integ.dat',status='replace')
        do i=1,lst_c
            write(12,'(100(e15.6))') dble(i-1)*dt, integ_run(i,:)
        end do
    close(12)
    deallocate(integ_run)




    open(unit=13, file='result_tc/thcd_run.dat', status='replace')
        do run=fst_run,lst_run
            write(13,'(i0,1x,2e15.6, f8.2)') run, tc_run(run),tc_sd_run(run), tc_sd_run(run)/tc_run(run)*1d2
        end do
    close(13)


    tc = sum(tc_run(:))/dble(num_run)
    tcsq = sum(tc_run(:)**2)/dble(num_run)
    tcsd = sqrt(tcsq-tc**2)
    tcse = tcsd/sqrt(dble(num_run-1))
    tcsdr = tcsd/tc
    tcser = tcse/tc
    deallocate(tc_run,tc_sd_run)


    open(unit=14,file='result_tc/thcd.dat',status='replace')
    write(14,'(a)')'thermal conductivity of fluid' 
    write(14,'(a,f15.8)')'tc(w m-1 run-1),',tc
    write(14,'(a,f15.8)')'tc(se),',tcse
    write(14,'(a,f15.8)')'tc(se)%,',tcser*1d2
    write(14,'(a)')'reference' 
    write(14,'(a,f15.8)')'tc(sd),',tcsd
    write(14,'(a,f15.8)')'tc(sd)%,',tcsdr*1d2
    close(14)
contains
    subroutine input_condition()
        real(real64):: r_temp, coex_temp, atomic_mass(3)
        real(real64):: r_mass, r_nd
        open(unit=100,file='../mdda.inpt',status='old')
        read(100,*) ndata
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
        print'(a,5x,i0)','ndata: ', ndata 
        print'(a,5x,i0)','ndata: ', ndata 
        print'(a,5x,f0.3)', 'temp: ', temp
        print'(a,5x,f0.3)', 'dens: ', dens
        print'(a,5x,e10.3)', 'dt: ', dt
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


    subroutine read_tcd_dat(x,y,z,current_run)
        real(real64),intent(out):: x(:),y(:),z(:)
        integer(int32):: current_run, i
        character(2):: c_run
        character(16):: file_name

        write(c_run,'(i2.2)') current_run
        file_name = '../run'//c_run//'/tdc.dat'
        print'(2a)', "Let's load ", file_name

        x(:)=0d0; y(:)=0d0; z(:)=0d0
        open(unit=1,file=file_name,status='old')
        do i=1,ndata
            read(1,*) x(i), y(i), z(i)
        end do
        close(1)
    end subroutine

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