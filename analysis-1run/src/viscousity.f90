program main
    use,intrinsic :: iso_fortran_env
    use md_condition_for_ana_mod
    implicit none
    integer(int32):: fst_calc, lst_calc, ndata
    real(real64), allocatable:: integ_acf_stress(:)
    real(real64):: temp, dt, vol, viscousity

    fst_calc = 4000
    lst_calc = 7000
    call load_condition_for_viscousity_ana(ndata=ndata, dt=dt, vol=vol)
    call read_temp_mean(temp)
    allocate(integ_acf_stress(ndata))

    ! 読み込み
    call read_integ_acf_stress(integ_acf_stress, ndata)
    
    ! 計算
    call calc_viscousity(integ_acf_stress=integ_acf_stress, viscousity=viscousity, fst_calc=fst_calc, lst_calc=lst_calc)

    ! 出力
    call output_viscousity(viscousity=viscousity)
contains
    subroutine read_temp_mean(temp)
        character(100),parameter:: file_temp_mean='temp/temp_mean.dat'
        real(real64):: temp
        integer(int32):: u_temp_mean
        
        open(newunit=u_temp_mean, file=file_temp_mean, status='old')
            read(u_temp_mean, *) temp
        close(u_temp_mean)
    end subroutine


    subroutine read_integ_acf_stress(integ_acf_stress, ndata)
        character(100),parameter:: file_name='GK_viscousity/integ_acf_stress.dat'
        real(real64),intent(out):: integ_acf_stress(:)
        integer(int32),intent(in):: ndata
        integer(int32):: i, u_stress

        open(newunit=u_stress, file=file_name, status='old')
            read(u_stress,*) (integ_acf_stress(i), i=1,ndata)
        close(u_stress)
    end subroutine


    subroutine calc_viscousity(integ_acf_stress, viscousity, fst_calc, lst_calc)
        integer(int32),intent(in):: fst_calc, lst_calc
        real(real64),intent(in):: integ_acf_stress(:)
        real(real64),intent(out):: viscousity
        integer(int32):: all_calc

        all_calc=lst_calc-fst_calc+1
        viscousity = sum(integ_acf_stress(fst_calc:lst_calc)) / dble(all_calc)
    end subroutine


    subroutine output_viscousity(viscousity)
        character(100),parameter:: file_viscousity = 'viscousity/viscousity.dat'
        real(real64),intent(in):: viscousity
        integer(int32):: u_viscousity

        open(newunit=u_viscousity, file=file_viscousity, status='replace')
            write(u_viscousity,'(e20.10)') viscousity
        close(u_viscousity)
    end subroutine
end program