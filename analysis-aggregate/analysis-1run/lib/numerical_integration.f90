module numerical_integration_mod
    use,intrinsic :: iso_fortran_env
    implicit none
contains
    pure function trapezoidal_integration(f,dx,n) result(g)
        real(real64),intent(in):: f(:), dx
        integer(int32),intent(in):: n
        real(real64):: g(n)
        integer(int32):: i

        g(1)=0d0
        do i=2,n
            g(i) = g(i-1) + f(i-1) + f(i)
        end do 
        g(:)=g(:)*5d-1*dx
    end function


    pure function sympson_integration(f,dx,n) result(g)
        real(real64),parameter:: inv3 = 1d0/3d0
        real(real64),intent(in):: f(:), dx
        integer(int32),intent(in):: n
        real(real64):: g(n)
        integer(int32):: i, i1, i2, i3

        g(1)=0d0
        do i=2,(n+1)/2
            i1 = 2*i-3
            i2 = 2*i-2
            i3 = 2*i-1
            g(i) = g(i-1) + f(i1)+4*f(i2)+f(i3)
        end do 
        g(:)=g(:)*(inv3*dx)
    end function
end module