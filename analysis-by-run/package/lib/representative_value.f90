module representative_value_mod
    use,intrinsic :: iso_fortran_env
    implicit none
    integer(int32),parameter:: prec=real64
contains
    function mean(arr, v, sd, se) result(m)
        ! solve:: mean of arr
        ! input:: arr
        ! output:: m, (v, sd, se) <- optional
        real(prec),intent(in):: arr(:)
        real(prec),optional,intent(out):: v, sd, se
        real(prec):: m, m2, len_arr, cv, csd

        len_arr = dble(size(arr))
        m = sum(arr)/len_arr
        m2 = sum(arr*arr)/len_arr
        cv = m2 - m*m
        csd = sqrt(cv)
        if (present(v)) v = cv
        if (present(sd)) sd = csd
        if (present(se)) se = csd/sqrt(len_arr)
    end function


    subroutine least_squares_method(n,x,y,a,b)
        integer(int32),intent(in):: n
        real(prec),intent(in):: x(n),y(n)
        real(prec),optional,intent(out):: a,b
        real(prec):: mean_x, mean_y, mean_xy
        real(prec):: var_x, cov_xy
        real(prec):: ca, cb, len_arr
        
        mean_x = mean(arr=x, v=var_x)
        mean_y = mean(arr=y)
        len_arr = dble(n)
        mean_xy = sum(x(:)*y(:)) / len_arr
        cov_xy = mean_xy - mean_x*mean_y

        ca = cov_xy / var_x
        cb = mean_y - ca*mean_x
        
        if (present(a)) a = ca
        if (present(b)) b = cb
    end subroutine
end module