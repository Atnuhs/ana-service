module representative_value_mod
    use,intrinsic :: iso_fortran_env
    implicit none
    integer(int32),parameter:: prec=real64
contains
    function mean(arr, var, stdev, stder) result(m)
        ! solve:: mean of arr
        ! input:: arr
        ! output:: m, (var, stdev, stder) <- optional
        real(prec),intent(in):: arr(:)
        real(prec),optional,intent(out):: var, stdev, stder
        real(prec):: m, m2, len_arr, cv, csd

        len_arr = dble(size(arr))
        m = sum(arr)/len_arr
        m2 = sum(arr*arr)/len_arr
        cv = m2 - m*m
        csd = sqrt(cv)
        if (present(var)) var = cv
        if (present(stdev)) stdev = csd
        if (present(stder)) stder = csd/sqrt(len_arr)
    end function


    function mean_from_sum(val_sum, val2_sum, size_val, var, stdev, stder) result(m)
        ! solve:: mean of arr
        ! output:: m, (var, stdev, stder) <- optional
        real(prec),intent(in):: val_sum, val2_sum, size_val
        real(prec),optional,intent(out):: var, stdev, stder
        real(prec):: m, cv, csd

        m = val_sum / size_val
        cv = val2_sum / (size_val-1) - val_sum*val_sum / size_val / (size_val-1)
        csd = sqrt(cv)
        if (present(var)) var = cv
        if (present(stdev)) stdev = csd
        if (present(stder)) stder = csd/sqrt(size_val)
    end function


    subroutine least_squares_method(len_arr,arx,ary,a,b)
        ! 最小二乗法
        integer(int32),intent(in):: len_arr
        real(prec),intent(in):: arx(len_arr),ary(len_arr)
        real(prec),optional,intent(out):: a,b
        real(prec):: mean_x, mean_y, mean_xy
        real(prec):: var_x, cov_xy
        real(prec):: ca, cb, size_arr
        
        mean_x = mean(arr=arx, var=var_x)
        mean_y = mean(arr=ary)
        size_arr = dble(len_arr)
        mean_xy = sum(arx(:)*ary(:)) / size_arr
        cov_xy = mean_xy - mean_x*mean_y

        ca = cov_xy / var_x
        cb = mean_y - ca*mean_x
        
        if (present(a)) a = ca
        if (present(b)) b = cb
    end subroutine
end module