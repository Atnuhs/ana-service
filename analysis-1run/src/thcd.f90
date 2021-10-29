program main
    use, intrinsic:: iso_fortran_env
    use md_condition_for_ana_mod
    ! 熱流の自己相関関数の積分値を読み込んでthcdを計算
    implicit none
    real(real64),allocatable:: integ_acf_ej(:)
    integer(int32):: ndata, fst_calc=3000, lst_calc=6000
    real(real64):: temp, temp0, dt, vol, thcd


    ! データの読み込み
    call load_condition_for_thcd_ana(ndata=ndata, dt=dt, vol=vol, temp0=temp0)
    call read_temp_mean(temp)
    allocate(integ_acf_ej(ndata))

    ! 計算
    call read_integ_acf_ej(integ_acf_ej=integ_acf_ej, ndata=ndata)
    call calc_thcd(integ_acf_ej=integ_acf_ej, thcd=thcd, temp=temp, temp0=temp0)

    ! 出力
    call output_thcd(thcd=thcd)
contains
    subroutine read_temp_mean(temp)
        character(100),parameter:: file_temp_mean='temp/temp_mean.dat'
        real(real64):: temp
        integer(int32):: u_temp_mean
        
        open(newunit=u_temp_mean, file=file_temp_mean, status='old')
            read(u_temp_mean, *) temp
        close(u_temp_mean)
    end subroutine


    subroutine read_integ_acf_ej(integ_acf_ej, ndata)
        character(100),parameter:: file_integ_acf_ej = 'GK_thcd/integ_acf_ej.dat'
        real(real64),intent(out):: integ_acf_ej(:)
        integer(int32),intent(in):: ndata
        integer(int32):: i, u_integ_acf_ej
        real(real64):: x

        open(newunit=u_integ_acf_ej,file=file_integ_acf_ej,status='old')
            read(u_integ_acf_ej,*) (x, integ_acf_ej(i), i=1,ndata)
        close(u_integ_acf_ej)
    end subroutine


    subroutine calc_thcd(integ_acf_ej, thcd, temp, temp0)
        real(real64),intent(in):: integ_acf_ej(:), temp, temp0
        real(real64),intent(out):: thcd
        real(real64):: thcd_temp

        thcd_temp = sum(integ_acf_ej(fst_calc:lst_calc)) / dble(lst_calc-fst_calc+1)
        thcd = thcd_temp * (temp0/temp)**0.25
    end subroutine


    subroutine output_thcd(thcd)
        character(100),parameter:: file_thcd = 'thcd/thcd.dat'
        real(real64),intent(in):: thcd
        integer(int32):: u_thcd

        open(newunit=u_thcd, file=file_thcd, status='replace')
            write(u_thcd,'(e20.10)') thcd
        close(u_thcd)
    end subroutine
end program