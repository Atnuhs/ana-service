program main
    use,intrinsic :: iso_fortran_env
    use md_condition_for_ana_mod
    implicit none
    integer(int32), parameter:: np=500, gr_len=1000
    integer(int32):: i, ndata
    real(real64):: gr(gr_len)
    real(real64):: cell, dr
    real(real64), allocatable:: rxyz(:,:,:)

    call load_condition_for_gr_ana(ndata, cell)
    dr = cell/dble(gr_len)
    allocate(rxyz(3,np,ndata))
    gr(:) = 0d0
    call read_rxyz(rxyz, ndata, np)
    call calc_gr(rxyz, ndata, np, cell, gr_len, dr, gr)
    call normalize_gr(gr,ndata,np,cell,dr)

    open(unit=11,file='gr/gr.dat', status='replace')
        write(11,'(2e14.5)') (i*dr, gr(i), i=1,gr_len)
    close(11)
contains
    subroutine read_rxyz(rxyz, ndata, np)
        real(real64),intent(out):: rxyz(:,:,:)
        integer(int32),intent(in):: ndata,np
        integer(int32):: i,j,u_rxyz

        open(newunit=u_rxyz, file='../rxyz.dat', status='old')
            do i=1,ndata
                read(u_rxyz,*)
                read(u_rxyz,*) (rxyz(:,j,i), j=1,np)
            end do
        close(u_rxyz)
    end subroutine


    subroutine adjust_periodic(vec, cell)
        real(real64),intent(in):: cell
        real(real64),intent(inout):: vec(3)
        real(real64):: hcell

        hcell = cell * 0.5d0

        where(vec > hcell)
            vec=vec-cell
        else where(vec < -hcell)
            vec=vec+cell
        end where
    end subroutine


    subroutine calc_gr(rxyz, ndata, np, cell, gr_len, dr, gr)
        integer(int32),intent(in):: ndata, np, gr_len
        real(real64),intent(in):: cell, dr, rxyz(:,:,:)
        real(real64),intent(inout):: gr(:)
        real(real64):: ri(3), rij(3)
        integer(int32):: idata,id,i,j

        do idata=1,ndata
            do i=1,np
                ri(:) = rxyz(:,i,idata)
                do j=i+1,np
                    rij(:) = rxyz(:,j,idata) - ri(:)
                    call adjust_periodic(rij, cell)
                    id = ceiling(norm2(rij)/dr)
                    if (id <= gr_len) gr(id) = gr(id) + 2
                end do
            end do
        end do
    end subroutine


    subroutine normalize_gr(gr, ndata, np, cell, dr)
        real(real64), parameter:: pi = acos(-1d0) 
        real(real64), intent(in):: cell, dr
        real(real64), intent(inout):: gr(:)
        integer(int32), intent(in)::ndata, np
        integer(int32):: i
        real(real64):: v,num_dens,factor

        num_dens = np / (cell*cell*cell)
        gr(:) = gr(:) / num_dens / dble(np*ndata)

        factor=4d0/3d0*pi * dr*dr*dr
        
        do i=lbound(gr,1), ubound(gr,1)
            v = factor * dble(3*i*(i-1)+1)
            gr(i)=gr(i)/v
        end do
    end subroutine
end program main