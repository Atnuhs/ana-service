! use random_mod

! rnd()          => real64,         0 <= v < 1
! randint(n)     => int32 or int64, 0 <= v <= n
! randrange(l,r) => int32 or int64, l <= v <= r

module random01_mod
    use,intrinsic :: iso_fortran_env
    implicit none
contains
    function random01() result(v)
        real(real64):: v

        call random_number(v)
    end function
end module


module random_int32
    use,intrinsic :: iso_fortran_env
    use random01_mod,only: rnd => random01
    implicit none
    private
    public:: randrange, randint
contains
    function randrange(l,r) result(v)
        integer(int32),intent(in):: l,r
        integer(int32):: v

        v = int(rnd()*(r-l+1)) + l
    end function

    function randint(n) result(v)
        integer(int32),intent(in):: n
        integer(int32):: v

        v = randrange(0_int32, n)
    end function
end module


module random_int64
    use,intrinsic :: iso_fortran_env
    use random01_mod,only: rnd => random01
    implicit none
    private
    public:: randrange, randint
contains
    function randrange(l,r) result(v)
        integer(int64),intent(in):: l,r
        integer(int64):: v

        v = int(rnd()*(r-l+1)) + l
    end function

    function randint(n) result(v)
        integer(int64),intent(in):: n
        integer(int64):: v

        v = randrange(0_int64, n)
    end function
end module


module random_real64
    use,intrinsic :: iso_fortran_env
    use random01_mod,only: rnd => random01
    implicit none
    private
    public:: randrange, randint
contains
    function randrange(l,r) result(v)
        real(real64),intent(in):: l,r
        real(real64):: v

        v = rnd()*(r-l) + l
    end function

    function randint(n) result(v)
        real(real64),intent(in):: n
        real(real64):: v

        v = rnd()*n
    end function
end module random_real64


module random_mod
    use,intrinsic :: iso_fortran_env
    use random01_mod, only: random01
    use random_int32, rri32=>randrange, rii32=>randint
    use random_int64, rri64=>randrange, rii64=>randint
    use random_real64, rrr64=>randrange, rir64=>randint
    
    interface randrange
        module procedure rri32, rri64, rrr64
    end interface

    interface randint
        module procedure rii32, rii64, rir64
    end interface

    interface random01
        module procedure random01
    end interface

    public:: random01, randrange, randint
end module