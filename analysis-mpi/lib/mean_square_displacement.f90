module mean_square_displacement_mod
    use,intrinsic :: iso_fortran_env
    use fft_mod
    implicit none
contains
    subroutine init_msd(n)
        integer(int32):: n

        call init_acf(n)
    end subroutine


    subroutine calc_cumlutive_sum_square(cs_sq, src, n)
        ! 「自乗」の累積和を計算O(N)
        integer(int32),intent(in):: n
        real(real64),intent(in):: src(:)
        real(real64),intent(out):: cs_sq(0:n)
        integer(int32):: i

        cs_sq(0) = 0d0
        do i=1,n
            cs_sq(i) = cs_sq(i-1) + src(i)*src(i)
        end do
    end subroutine


    function calc_mean_square_displacement(len_src, src) result(msd) 
        ! MSDを自己相関関数、累積和を用いて計算O(N)
        integer(int32),intent(in):: len_src
        real(real64),intent(in):: src(len_src)
        real(real64):: msd(len_src)
        real(real64):: cs_sq(0:len_src), acf(len_src)
        real(real64):: cs1, cs2
        integer(int32):: i

        acf(:) = auto_correlation_function(src)
        call calc_cumlutive_sum_square(cs_sq, src, len_src)
        do concurrent(i=0:len_src-1)
            cs1 = cs_sq(len_src-i)
            cs2 = cs_sq(len_src) - cs_sq(i)
            msd(i+1) = (cs1+cs2 -2*acf(i+1))/dble(len_src-i)
        end do
    end function
end module mean_square_displacement_mod