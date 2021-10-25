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
        cv = abs(m2 - m*m)
        csd = sqrt(cv)
        if (present(v)) v = cv
        if (present(sd)) sd = csd
        if (present(se)) se = csd/sqrt(len_arr)
    end function
end module