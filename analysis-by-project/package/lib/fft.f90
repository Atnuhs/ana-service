module fft_2based_class
    use,intrinsic :: iso_fortran_env
    implicit none
    real(real64),parameter:: pi = acos(-1d0), pi2 = 2d0*pi
    private
    public:: fft_2based, fft, ifft
    type:: fft_2based
        integer(int32):: l, hl, n, bit_len
        real(real64),allocatable:: wr(:),wi(:)
    contains
        procedure,private:: set_w
        procedure,private:: calc
        procedure:: fft,ifft
    end type
    interface fft_2based
        module procedure:: init_fft_calc
    end interface
contains
    function init_fft_calc(array_len) result(fftc)
        integer(int32),intent(in):: array_len
        type(fft_2based):: fftc

        fftc%l = array_len
        fftc%hl = fftc%l/2
        fftc%bit_len = 0
        do while(shiftl(1,fftc%bit_len) < fftc%l)
            fftc%bit_len = fftc%bit_len + 1
        end do

        fftc%n = shiftl(1,fftc%bit_len)
        ! print*, fftc%l, fftc%hl, fftc%n, fftc%bit_len
        call fftc%set_w()
    end function

    subroutine set_w(fftc)
        class(fft_2based):: fftc
        real(real64):: theta
        integer(int32):: i,hn

        if (allocated(fftc%wr)) deallocate(fftc%wr)
        if (allocated(fftc%wi)) deallocate(fftc%wi)
        hn = shiftr(fftc%n,1)
        allocate(fftc%wr(0:hn-1), fftc%wi(0:hn-1))
        
        theta = -pi2/real(fftc%n, kind=real64)
        do i=0,hn-1
            fftc%wr(i) = cos(theta*i)
            fftc%wi(i) = sin(theta*i)
        end do
        call bit_rev(fftc%wr, fftc%wi, hn, fftc%bit_len-1)
    end subroutine


    subroutine bit_rev(ar, ai, n, bit_len)
        integer(int32)::i,j,n,bit_len
        real(real64):: ar(1:n), ai(1:n)

        do i=0,n-1
            j = calc_rev(i, bit_len)
            ! print"(2b16."//char(bit_len+ichar("0"))//")",i,j
            if (i < j)then
                call swap(i+1,j+1)
            end if
        end do
    contains
        subroutine swap(i,j)
            integer(int32),intent(in):: i,j
            real(real64):: t1r,t1i,t2r,t2i
    
            t1r=ar(i)
            t2r=ar(j)
            t1i=ai(i)
            t2i=ai(j)
            ar(j)=t1r
            ar(i)=t2r
            ai(j)=t1i
            ai(i)=t2i
        end subroutine
    end subroutine


    function calc_rev(x, bit_len) result(revx)
        integer(int32),intent(in):: x
        integer(int32):: revx,i,t,bit_len
        revx=0
            do i=0,bit_len-1
                ! print*, k,h-k-1,h,n
                t = iand(1, shiftr(x,i))
                revx = ieor(revx, shiftl(t, bit_len-1-i))
            end do
    end function


    subroutine fft(fftc,ar,ai)
        class(fft_2based),intent(inout):: fftc
        real(real64):: ar(fftc%n),ai(fftc%n)

        call bit_rev(ar,ai,fftc%n,fftc%bit_len)
        call fftc%calc(ar,ai,.false.)
    end subroutine


    subroutine ifft(fftc,ar,ai)
        class(fft_2based),intent(inout):: fftc
        real(real64):: ar(fftc%n),ai(fftc%n)

        call bit_rev(ar,ai,fftc%n,fftc%bit_len)
        call fftc%calc(ar,ai,.true.)
        ar(:) = ar(:)/dble(fftc%n)
        ai(:) = ai(:)/dble(fftc%n)
    end subroutine


    subroutine calc(fftc, ar, ai, inv)
        class(fft_2based),intent(inout):: fftc
        real(real64):: ar(fftc%n), ai(fftc%n)
        real(real64):: xr,xi,wkr,wki
        integer(int32):: m, hm, k, i,hi, j, t
        logical:: inv

        hm = 1
        m = shiftl(hm,1)
        t=0
        do while(m <= fftc%n)
            t=t+1
            do i=0, fftc%n-1, m
                hi = shiftr(i,t)

                wkr = fftc%wr(hi)
                wki = fftc%wi(hi)
                if (inv) wki=-wki
                do j=i+1, hm+i
                    k = j+hm
                    xr = ar(j) - ar(k)
                    xi = ai(j) - ai(k)
                    ar(j) = ar(j) + ar(k)
                    ai(j) = ai(j) + ai(k)
                    ar(k) = wkr*xr - wki*xi
                    ai(k) = wkr*xi + wki*xr
                end do
            end do
            hm = m
            m = shiftl(hm,1)
        end do
    end subroutine
end module



module fft_mod
    use,intrinsic :: iso_fortran_env
    use fft_2based_class
    implicit none
    type(fft_2based):: fftc
    private
    public:: init_fft, init_conv, init_corr, init_acf
    public:: fft2, ifft2
    public:: convolution, liner_convolution
    public:: correlation, liner_correlation
    public:: auto_correlation_function
    public:: get_fft_len
    
contains
    subroutine init_fft(array_size)
        integer(int32),intent(in):: array_size

        fftc = fft_2based(array_size)
    end subroutine


    subroutine init_conv(array_size)
        integer(int32),intent(in):: array_size

        fftc = fft_2based(array_size*2)
    end subroutine


    subroutine init_corr(array_size)
        integer(int32),intent(in):: array_size

        fftc = fft_2based(array_size*2)
    end subroutine


    subroutine init_acf(array_size)
        integer(int32),intent(in):: array_size

        fftc = fft_2based(array_size*2)
    end subroutine


    subroutine fft2(ar,ai)
        real(real64):: ar(fftc%n),ai(fftc%n)

        call fftc%fft(ar,ai)
    end subroutine


    subroutine ifft2(ar,ai)
        real(real64):: ar(fftc%n),ai(fftc%n)

        call fftc%ifft(ar,ai)
    end subroutine


    subroutine convolution(fr,gr,xr)
        real(real64), intent(inout):: fr(fftc%n),gr(fftc%n),xr(fftc%n)
        real(real64):: fi(fftc%n), gi(fftc%n), xi(fftc%n)

        call fftc%fft(fr,fi)
        call fftc%fft(gr,gi)
        xr(1:fftc%n) = fr(1:fftc%n)*gr(1:fftc%n) - fi(1:fftc%n)*gi(1:fftc%n)
        xi(1:fftc%n) = fr(1:fftc%n)*gi(1:fftc%n) + fi(1:fftc%n)*gr(1:fftc%n)
        call fftc%ifft(xr,xi)
    end subroutine


    subroutine liner_convolution(fr,gr,xr)
        real(real64), intent(in):: fr(fftc%hl),gr(fftc%hl)
        real(real64), intent(out):: xr(fftc%hl)
        real(real64):: fr2(fftc%n), gr2(fftc%n), xr2(fftc%n)

        fr2(1:fftc%hl) = fr(1:fftc%hl); fr2(fftc%hl+1:fftc%n) = 0d0
        gr2(1:fftc%hl) = gr(1:fftc%hl); gr2(fftc%hl+1:fftc%n) = 0d0
        call convolution(fr2,gr2,xr2)
        xr(1:fftc%hl) = xr2(1:fftc%hl)
    end subroutine


    subroutine correlation(fr,gr,xr)
        real(real64), intent(inout):: fr(fftc%n),gr(fftc%n)
        real(real64), intent(out):: xr(fftc%n)
        real(real64):: fi(fftc%n),gi(fftc%n),xi(fftc%n)

        call fftc%fft(fr,fi)
        call fftc%fft(gr,gi)
        xr(1:fftc%n) = fr(1:fftc%n)*gr(1:fftc%n) + fi(1:fftc%n)*gi(1:fftc%n)
        xi(1:fftc%n) = fr(1:fftc%n)*gi(1:fftc%n) - fi(1:fftc%n)*gr(1:fftc%n)
        call fftc%ifft(xr,xi)
    end subroutine


    subroutine liner_correlation(fr,gr,xr)
        real(real64), intent(in):: fr(fftc%hl),gr(fftc%hl)
        real(real64), intent(out):: xr(fftc%hl)
        real(real64):: fr2(fftc%n), gr2(fftc%n), xr2(fftc%n)

        fr2(1:fftc%hl) = fr(1:fftc%hl); fr2(fftc%hl+1:fftc%n) = 0d0
        gr2(1:fftc%hl) = gr(1:fftc%hl); gr2(fftc%hl+1:fftc%n) = 0d0
        call correlation(fr2,gr2,xr2)
        xr(1:fftc%hl) = xr2(1:fftc%hl)
    end subroutine


    function auto_correlation_function(ar) result(xr)
        real(real64),intent(in):: ar(fftc%hl)
        real(real64):: xr(fftc%hl), ar2(fftc%n), ai2(fftc%n)

        ar2(1:fftc%hl) = ar(1:fftc%hl); ar2(fftc%hl+1:fftc%n) = 0d0
        ai2(1:fftc%n) = 0d0
        call fftc%fft(ar2, ai2)
        ar2(1:fftc%n) = ar2(1:fftc%n)*ar2(1:fftc%n) + ai2(1:fftc%n)*ai2(1:fftc%n)
        ai2(1:fftc%n) = 0d0
        call fftc%ifft(ar2, ai2)
        xr(:) = ar2(1:fftc%hl)
    end function


    function get_fft_len() result(fft_len)
        integer(int32):: fft_len

        fft_len = fftc%n
    end function
end module fft_mod