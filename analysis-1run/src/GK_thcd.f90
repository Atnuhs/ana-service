program main
    use, intrinsic:: iso_fortran_env
    use const_mod
    use fft_mod
    use read_md_results_mod
    use io_file_mod
    use md_condition_for_ana_mod
    use numerical_integration_mod
    implicit none
    integer(int32):: ndata, i
    real(real64),allocatable:: ej(:,:)
    real(real64),allocatable:: acf_ej(:)
    real(real64),allocatable:: integ_acf_ej(:)
    real(real64),allocatable:: x(:)
    real(real64):: temp, dt, vol


    ! データの読み込み
    call load_condition_for_GK_thcd_ana(ndata=ndata, dt=dt, vol=vol)
    call read_temp_mean(temp=temp)
    allocate(ej(ndata, 3))
    allocate(acf_ej(ndata))
    allocate(integ_acf_ej(ndata))
    call read_tdc(ndata, ej)

    ! 計算
    call calc_acf_ej()
    call calc_integ_acf_ej()

    ! 出力
    allocate(x(ndata), source=[(dt*dble(i-1), i=1,ndata)])
    call write_arx_ary(ndata, 'GK_thcd/acf_ej.dat', x, acf_ej)
    call write_arx_ary(ndata, 'GK_thcd/integ_acf_ej.dat', x, integ_acf_ej)
contains
    subroutine read_temp_mean(temp)
        real(real64),intent(out):: temp

        call read_x('temp/temp_mean.dat', temp)
    end subroutine


    subroutine calc_acf_ej()
        integer(int32):: i
        
        call init_acf(ndata)
        acf_ej(:) = 0d0
        do i=1,3
            acf_ej(:) = acf_ej(:) + auto_correlation_function(ej(:,i))
        end do
        acf_ej(:) = acf_ej(:) / (dble(ndata) * vol*kbt*temp*temp * 3d0)
    end subroutine


    subroutine calc_integ_acf_ej()
        integer(int32):: i

            integ_acf_ej(:) = trapezoidal_integration(f=acf_ej, dx=dt, n=ndata)
    end subroutine
end program