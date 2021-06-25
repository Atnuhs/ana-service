
program main
    use,intrinsic :: iso_fortran_env
    use fft_mod
    implicit none
    integer(int32):: i,j,k,run
    integer(int32):: fst_run, lst_run, num_run
    integer(int32):: np, ndata, intd, n2
    ! integer(int32):: omp_get_thread_num
    real(real64):: dt
    real(real64), allocatable:: tdr(:,:,:,:), cs_sq(:,:,:,:)
    real(real64), allocatable:: acf(:,:,:,:)
    real(real64), allocatable:: msd(:,:,:,:), mean_msd(:,:)

    ! call omp_set_num_threads(4)
    np = 500
    fst_run=2
    lst_run=11
    num_run=lst_run-fst_run+1
    call input_condition(ndata, dt, intd)

    allocate(tdr(ndata, 3, np, fst_run:lst_run))
    
    ! input ----------------------------------------------
    print'(a)', '### input'
    do run=fst_run, lst_run
        print'(i0)', run
        tdr(:,:,:,run) = read_dxyz(np, ndata, run)
    end do

    n2 = 1
    do while(n2 < ndata)
        n2=n2*2
    end do


    allocate(acf(0:ndata-1, 3, np, fst_run:lst_run))
    print'(a)', '### correlation'
    

    do run=fst_run,lst_run
        print'(i0)', run
        !$omp parallel do
            do i=1,np
                do j=1,3
                    call liner_correlation(tdr(:,j,i,run), tdr(:,j,i,run), acf(:,j,i,run),n2)
                end do
            end do
        !$omp end parallel do
    end do


    print'(a)', '### cumlutive sum'
    allocate(cs_sq(0:ndata,3,np,run))
    cs_sq(0,:,:,:) = 0d0
    cumlutive_sum :  do run=fst_run,lst_run
        do i=1,np
            do j=1,3
                do k=1,ndata
                    cs_sq(k,j,i,run) = cs_sq(k-1,j,i,run) + tdr(k,j,i,run)*tdr(k,j,i,run)
                end do
            end do
        end do
    end do cumlutive_sum

    allocate(msd(0:ndata-1,3,np,run), source=0d0)
    print'(a)', '### calc_msd'
    do run=fst_run,lst_run
        print'(i0)', run
        do i=1,np
            do j=1,3
                msd(:,j,i,run) = calc_mean_square_displacement(cs_sq(:,j,i,run), acf(:,j,i,run), ndata)
            end do
        end do
    end do

    do i=0,ndata-1
        msd(i,:,:,:) = msd(i,:,:,:)/dble(ndata-i)
    end do

    allocate(mean_msd(0:ndata-1,fst_run:lst_run))

    print'(a)', '### calc_average'
    do run = fst_run, lst_run
        do i=0,ndata-1
            mean_msd(i, run) = sum(msd(i,:,:,run))/dble(np)
        end do
    end do

    print'(a)', '### output'
    open(unit=12, file='msd_all.dat', status='replace')
        do i=0,ndata-1
            write(12,'(100(e13.5))')dt*dble(i*intd), mean_msd(i,:)
        enddo
    close(12)
    print'(a)', '### end'
contains
    function read_dxyz(np,ndata,run) result(run_tdr)
        integer(int32),intent(in):: np, ndata, run
        real(real64):: run_tdr(ndata,3,np)
        character(2):: c_run
        character(:),allocatable:: dxyz_file_path

        write(c_run,'(i2.2)') run
        dxyz_file_path = '../run' // c_run // '/dxyz.dat'
        ! print'(a)', 'read: '//dxyz_file_path
        open(unit=11, file=dxyz_file_path, status='old')
        read(11,*)
            do i=1,ndata
                read(11,*)
                do j=1,np
                    block; real(real64):: x,y,z
                    read(11,*) x,y,z
                    run_tdr(i,1,j) = x
                    run_tdr(i,2,j) = y
                    run_tdr(i,3,j) = z
                    end block
                end do
            end do
        close(11)
    end function


    subroutine input_condition(ndata,dt,intd)
        integer(int32),intent(out):: ndata,intd
        real(real64),intent(out):: dt
        integer(int32):: nstep

        open(unit=100,file='../mdda.inpt',status='old')
            read(100,*) nstep
            read(100,*)
            read(100,*)
            read(100,*) dt
            read(100,*)
            read(100,*)
            read(100,*)
            read(100,*) intd
        close(100)
        ndata=nstep/intd
    end subroutine

    function calc_mean_square_displacement(cs_sq, acf, n) result(msd)
        real(real64),intent(in):: cs_sq(0:n), acf(0:n-1)
        integer(int32),intent(in):: n
        real(real64):: msd(0:n-1), cs1, cs2
        integer(int32):: i

        do i=0,n-1
            cs1 = cs_sq(n-i)
            cs2 = cs_sq(n) - cs_sq(i)
            msd(i) = cs1+cs2-2*acf(i)
        end do
    end function
end program
