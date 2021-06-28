
program main
    use,intrinsic :: iso_fortran_env
    use fft_mod
    implicit none
    integer(int32),parameter:: np = 500
    integer(int32):: ndata, intd
    real(real64):: dt
    real(real64), allocatable:: tdr(:,:,:)
    real(real64), allocatable:: acf_tdr(:,:,:)
    real(real64), allocatable:: cs_sq_tdr(:,:,:)
    real(real64), allocatable:: msd(:,:,:), mean_msd(:)

    ! call omp_set_num_threads(4)
    call input_condition(ndata=ndata, dt=dt, intd=intd)
    allocate(tdr(ndata, np, 3))
    allocate(acf_tdr(ndata, np, 3))
    allocate(cs_sq_tdr(ndata, np, 3))
    allocate(msd(ndata, np, 3))
    call read_dxyz(tdr=tdr, ndata=ndata, np=np)
    call calc_acf(acf_tdr=acf_tdr, tdr=tdr, ndata=ndata, np=np)
    call calc_cumlutive_sum_square(cs_sq_tdr=cs_sq_tdr, tdr=tdr, nadata=ndata, np=np)
    



    print'(a)', '### cumlutive sum'
    

    print'(a)', '### calc_msd'
    do run=fst_run,lst_run
        print'(i0)', run
        do i=1,np
            do j=1,3
                msd(:,j,i,run) = calc_mean_square_displacement(cs_sq_tdr(:,j,i,run), acf_tdr(:,j,i,run), ndata)
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
    subroutine input_condition(ndata, dt, intd)
        integer(int32),intent(out):: ndata, intd
        real(real64),intent(out):: dt
        type(mdda_type):: mdda
        real(real64):: dens, vol, tmass

        call read_mdda(mdda)
        ndata = mdda%nstep/mdda%intd
        dt = mdda%dt
        intd = mdda%intd
    end subroutine


    subroutine read_dxyz(tdr, ndata, np)
        real(real64),intent(out):: tdr(:,:,:)
        integer(int32),intent(in):: np, ndata
        integer(int32):: u_dxyz, i, j, k
        character(100),parameter:: file_dxyz='../dxyz.dat'

        open(newunit=u_dxyz, file=file_dxyz, status='old')
        read(u_dxyz,*)
            do i=1,ndata
                read(u_dxyz,*)
                read(u_dxyz,*) ((tdr(i,j,k), k=1,3), j=1,np)
            end do
        close(u_dxyz)
    end subroutine


    subroutine calc_acf_tdr(acf_tdr, tdr, ndata, np)
        real(real64),intent(out):: acf_tdr(:,:,:)
        integer(int32),intent(in):: ndata, np
        real(real64),intent(in):: tdr(:,:,:)
        integer(int32):: i,j

        call init_acf(array_size=np)
        do concurrent(i=1,3, j=1,np)
            acf_tdr(:,j,i) = auto_correlation_function(tdr(:,j,i))
        end do
    end subroutine


    subroutine calc_cumlutive_sum_square(cs_sq_tdr, tdr, ndata, np)
        real(real64),intent(out):: cs_sq_tdr(:,:,:)
        integer(int32),intent(in):: ndata, np
        real(real64),intent(in);: tdr(:,:,:)

        cs_sq_tdr(1,:,:) = 0d0
        do concurrent(i=1,3, j=1,np)
            do k=2,ndata
                cs_sq_tdr(k,j,i) = cs_sq_tdr(k-1,j,i) + tdr(k,j,i)*tdr(k,j,i)
            end do
        end do
    end subroutine




    function calc_mean_square_displacement(cs_sq_tdr, acf_tdr, n) result(msd)
        real(real64),intent(in):: cs_sq_tdr(0:n), acf_tdr(0:n-1)
        integer(int32),intent(in):: n
        real(real64):: msd(0:n-1), cs1, cs2
        integer(int32):: i

        do i=0,n-1
            cs1 = cs_sq_tdr(n-i)
            cs2 = cs_sq_tdr(n) - cs_sq_tdr(i)
            msd(i) = cs1+cs2-2*acf_tdr(i)
        end do
    end function
end program
