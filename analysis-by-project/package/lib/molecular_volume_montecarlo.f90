module molecular_volume_montecarlo_mod
    use,intrinsic :: iso_fortran_env
    use random_mod
    implicit none
contains
    function calc_molecular_volume(n, m, radius, pos) result(ret)
        integer(int32),intent(in):: n, m
        real(real64),intent(in):: radius(n), pos(3,n)
        integer(int32):: i,j,cnt,interval
        real(real64):: pos_maxmin(2,3), ret, p_rand(3), dist, box_vol
        logical:: inside

        ! calc extrema
        print'(a)', 'calc extrema --------------------------------'
        do i=1,3
            pos_maxmin(:,i) = extrema(pos(i,:), radius(:))
            print*, i, pos_maxmin(:,i)
        end do
        box_vol = product((pos_maxmin(1,:) - pos_maxmin(2,:)))
        print*, box_vol

        print'(a)', 'calc montecarlo --------------------------------'
        cnt=0
        interval = m/1000
        do i=1,m
            p_rand(:) = [(randrange(pos_maxmin(2,j), pos_maxmin(1,j)), j=1,3)]
            inside=.false.
            do j=1,n
                dist = norm2((p_rand(:)-pos(:,j)))
                inside = inside .or. dist <= radius(j)
            end do
            if (inside) cnt=cnt+1
            if (mod(i,interval)==0) print*, i , dble(cnt)/dble(i)
        end do
        ret = dble(cnt)/dble(m) * box_vol
    end function


    function extrema(arr, radius) result(ret)
        real(real64),intent(in):: arr(:), radius(:)
        real(real64):: ret(2)
        integer(int32):: i

        ret(:) = [arr(1)+radius(1), arr(1)-radius(1)]

        do i=2,size(arr)
            ret(1) = max(ret(1), arr(i)+radius(i))
            ret(2) = min(ret(2), arr(i)-radius(i))
        end do
    end function
end module molecular_volume_montecarlo_mod


program main
    use,intrinsic :: iso_fortran_env
    use molecular_volume_montecarlo_mod
    implicit none
    integer(int32),parameter:: n=2,m=100000000
    real(real64):: radius(n), pos(3,n)
    
    radius(1) = 2d0
    radius(2) = 2d0
    pos(:,1) = [-1.5d0, 0d0, 0d0]
    pos(:,2) = [ 1.5d0, 0d0, 0d0]

    print*, calc_molecular_volume(n,m, radius, pos)
    
end program main