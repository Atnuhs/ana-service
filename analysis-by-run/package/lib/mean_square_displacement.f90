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


    subroutine calc_mean_square_displacement(msd, src, n)
        ! MSDを自己相関関数、累積和を用いて計算O(N)
        real(real64),intent(out):: msd(:)
        integer(int32),intent(in):: n
        real(real64),intent(in):: src(:)
        real(real64),allocatable:: cs_sq(:), acf(:)
        real(real64):: cs1, cs2
        integer(int32):: i

        allocate(cs_sq(0:n), acf(n))
        acf(:) = auto_correlation_function(src)
        call calc_cumlutive_sum_square(cs_sq, src, n)
        do concurrent(i=0:n-1)
            cs1 = cs_sq(n-i)
            cs2 = cs_sq(n) - cs_sq(i)
            msd(i+1) = (cs1+cs2 -2*acf(i+1))/dble(n-i)
        end do
    end subroutine
end module mean_square_displacement_mod