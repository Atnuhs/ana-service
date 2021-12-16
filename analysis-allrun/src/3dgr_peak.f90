program main
    use,intrinsic :: iso_fortran_env
    implicit none
    integer(int32),parameter:: lim_peak = 10
    integer(int32):: len_x, len_y, ix, iy, ip
    integer(int32):: u_mo1, u_mo2, u_peaks1, u_peaks2
    integer(int32),allocatable:: ipeaks(:)
    real(real64),allocatable:: x(:), y(:), z(:,:), peaks(:,:)
    
    len_x = 100
    len_y = 90
    allocate(x(len_x), y(len_y), z(len_x,len_y))
    allocate(peaks(lim_peak,len_y), ipeaks(len_y))
    ipeaks(:) = 0

    open(newunit=u_mo1, file='./molecular_orientation/molecular_orientation_mean.txt', status='old')
        do ix=1,len_x
            do iy=1,len_y
                read(u_mo1,*) x(ix), y(iy), z(ix,iy)
            end do
        end do
    close(u_mo1)
    call search_peaks()

    open(newunit=u_peaks1, file='./peaks/peaks1.txt', status='replace')
        do iy = 1, len_y
            do ip=1,ipeaks(iy)
                write(u_peaks1, *) y(iy), peaks(ip, iy)
            end do
        end do
    close(u_peaks1)

    ipeaks(:) = 0

    open(newunit=u_mo2, file='./molecular_orientation/molecular_orientation2_mean.txt', status='old')
        do ix=1,len_x
            do iy=1,len_y
                read(u_mo2,*) x(ix), y(iy), z(ix,iy)
            end do
        end do
    close(u_mo2)

    call search_peaks()

    open(newunit=u_peaks2, file='./peaks/peaks2.txt', status='replace')
        do iy = 1, len_y
            do ip=1,ipeaks(iy)
                write(u_peaks2, *) y(iy), peaks(ip, iy)
            end do
        end do
    close(u_peaks2)
    

contains
    subroutine search_peaks()
        integer(int32):: ix, iy
        do iy=1,len_y
            do ix=2,len_x-1
                if (z(ix-1,iy) < z(ix,iy) .and. z(ix+1,iy) < z(ix,iy)) then
                    ! peak発見
                    ipeaks(iy) = ipeaks(iy) + 1
                    if (ipeaks(iy) <= lim_peak) then
                        peaks(ipeaks(iy), iy) = x(ix)
                    end if 
                end if
            end do
        end do
    end subroutine
end program main