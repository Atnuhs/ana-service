program main
    use,intrinsic :: iso_fortran_env
    use const_mod
    use fft_mod
    use read_md_results_mod
    use io_file_mod
    use md_condition_for_ana_mod
    use numerical_integration_mod
    implicit none
    integer(int32):: i, ndata
    real(real64), allocatable:: stress(:,:)
    real(real64), allocatable:: acf_stress(:)
    real(real64), allocatable:: integ_acf_stress(:)
    real(real64), allocatable:: x(:)
    real(real64):: temp, dt, vol

    ! 読み込み
    call load_condition_for_GK_viscousity_ana(ndata=ndata, dt=dt, vol=vol)
    call read_temp_mean(temp)
    allocate(stress(ndata,3))
    allocate(acf_stress(ndata))
    allocate(integ_acf_stress(ndata))
    call read_stress(ndata, stress)
    
    ! 計算
    call calc_acf_stress()
    call calc_integ_acf_stress()

    ! 出力
    allocate(x(ndata), source=[(dt*dble(i-1), i=1,ndata)])
    call write_arx_ary(ndata, 'GK_viscousity/acf_stress.dat', x, acf_stress)
    call write_arx_ary(ndata, 'GK_viscousity/integ_acf_stress.dat', x, integ_acf_stress)
contains
    subroutine read_temp_mean(temp)
        real(real64),intent(out):: temp

        call read_x('temp/temp_mean.dat', temp)
    end subroutine


    subroutine calc_acf_stress()
        integer(int32):: i

        acf_stress(:) = 0d0
        call init_acf(ndata)
        do i=1,3
            acf_stress(:) = acf_stress(:) + auto_correlation_function(stress(:,i))
        end do
        acf_stress(:) = acf_stress(:) / (dble(ndata)*vol*kbt*temp*3d0)
    end subroutine


    subroutine calc_integ_acf_stress()
        integer(int32):: i

        integ_acf_stress(:) = trapezoidal_integration(f=acf_stress,dx=dt,n=ndata)
    end subroutine
end program