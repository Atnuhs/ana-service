
program main
    use,intrinsic :: iso_fortran_env
    use md_condition_for_ana_mod
    use representative_value_mod
    use mean_square_displacement_mod
    use io_file_mod
    implicit none
    include 'mpif.h'
    integer(int32):: procs, rank, ierr 
    ! prcos:: プロセス数 rank:: 自分のプロセスid(0-indexed)

    call mpi_init(ierr)
    call mpi_comm_size(mpi_comm_world, procs, ierr)
    call mpi_comm_rank(mpi_comm_world, rank, ierr)
    call main_routine()
    call mpi_finalize(ierr)
contains
    subroutine main_routine()
        integer(int32):: ndata, np, intd, i
        integer(int32):: fst_run, lst_run, all_run
        real(real64):: dt, rdc, temp0, temp
        real(real64), allocatable:: tdphi(:,:,:)
        real(real64), allocatable:: rmsd(:), rmsd_sum(:), rmsd2_sum(:)
        real(real64), allocatable:: rmsd_sum_global(:), rmsd2_sum_global(:)
        real(real64), allocatable:: x(:), rmsd_mean(:), rmsd_stdev(:)
        real(real64):: rdc_sum, rdc2_sum, rdc_sum_global, rdc2_sum_global
        real(real64):: rdc_mean, rdc_stdev
    
        if (rank==0) read*, fst_run, lst_run
        all_run = lst_run-fst_run+1

        call mpi_bcast(fst_run, 1, mpi_integer, 0, mpi_comm_world, ierr)
        call mpi_bcast(lst_run, 1, mpi_integer, 0, mpi_comm_world, ierr)

        ! インプットの読み込みなどの準備
        call load_condition_for_rmsd_ana(ndata, np, dt, intd, temp0)
        allocate(tdphi(ndata, np, 3))
        allocate(rmsd(ndata), rmsd_sum(ndata), rmsd2_sum(ndata))
        allocate(rmsd_sum_global(ndata), rmsd2_sum_global(ndata))
        allocate(rmsd_mean(ndata), rmsd_stdev(ndata))
        allocate(x(ndata))
        call init_msd(ndata)

        rmsd_sum(:) = 0d0
        rmsd2_sum(:) = 0d0
        rdc_sum = 0d0
        rdc2_sum = 0d0
        x = [((i-1)*(dt*intd), i=1,ndata)]


        do i=fst_run+rank, lst_run, procs
            ! rmsd
            print*, 'reading ... ', '../calculation/'//rundir(i)//'/tdphi.dat', rank
            call read_tdphi('../calculation/'//rundir(i)//'/tdphi.dat', ndata, np, tdphi)
            call calc_rmsd(rmsd, tdphi, ndata, np)
            rmsd_sum(:) = rmsd_sum(:) + rmsd(:)
            rmsd2_sum(:) = rmsd2_sum(:) + rmsd(:)*rmsd(:)

            ! translational_diffusion_coefficient
            call read_x('./temp/temp_'//rundir(i)//'.dat', temp)
            call calc_rdc(ndata, x, rmsd, rdc)
            rdc = rdc * (temp/temp0)    
            rdc_sum = rdc_sum + rdc
            rdc2_sum = rdc2_sum + rdc*rdc
        end do
        
        call mpi_reduce(rmsd_sum, rmsd_sum_global, ndata, mpi_double_precision, mpi_sum, 0, mpi_comm_world, ierr)
        call mpi_reduce(rmsd2_sum, rmsd2_sum_global, ndata, mpi_double_precision, mpi_sum, 0, mpi_comm_world, ierr)
        call mpi_reduce(rdc_sum, rdc_sum_global, 1, mpi_double_precision, mpi_sum, 0, mpi_comm_world, ierr)
        call mpi_reduce(rdc2_sum, rdc2_sum_global, 1, mpi_double_precision, mpi_sum, 0, mpi_comm_world, ierr)

        if (rank==0) then
            ! rmsd
            do i=1,ndata
                rmsd_mean(i) = mean_from_sum(rmsd_sum_global(i), rmsd2_sum_global(i), dble(all_run), stdev=rmsd_stdev(i))
            end do
            call write_arx_ary_arysd('rmsd/rmsd.txt', ndata, x, rmsd_mean, rmsd_stdev)


            ! translational_diffusion_coefficient
            rdc_mean = mean_from_sum(rdc_sum_global, rdc2_sum_global, dble(all_run), stdev=rdc_stdev)
            call write_x_xsd('rmsd/rdc.txt', rdc_mean, rdc_stdev)
        end if
        
    end subroutine

    
    subroutine calc_rmsd(rmsd, tdphi, ndata, np)
        ! rmsdを自己相関関数、累積和を用いて計算O(N)
        real(real64),intent(out):: rmsd(:)
        real(real64),intent(in):: tdphi(:,:,:)
        integer(int32),intent(in):: ndata, np
        integer(int32):: i,j

        rmsd(:) = 0d0
        do i=1,3
            do j=1,np
                rmsd(:) = rmsd(:) + calc_mean_square_displacement(ndata, tdphi(:,j,i))
            end do
        end do
        rmsd(:) = rmsd(:) / dble(np)
    end subroutine


    subroutine calc_rdc(ndata, x, rmsd_mean, rdc)
        real(real64),parameter:: zeta = 2.837297d0
        integer(int32),intent(in):: ndata
        real(real64),intent(in):: x(ndata), rmsd_mean(ndata)
        real(real64),intent(out):: rdc
        integer(int32):: l,r,n,i
        real(real64):: a

        l = ndata/10
        r = ndata/4
        n = r-l+1
        call least_squares_method(len_arr=n, arx=x(l:r), ary=rmsd_mean(l:r), a=a)
        rdc = a / 4d0
    end subroutine
end program
