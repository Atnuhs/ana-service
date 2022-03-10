module read_md_results_mod
    use,intrinsic :: iso_fortran_env
    implicit none
    character(100),parameter:: file_sxyz='../sxyz.dat'
    character(100),parameter:: file_tdc='../tdc.dat'
    character(100),parameter:: file_stress='../stress.dat'
    character(100),parameter:: file_rxyz='../rxyz.dat'

    integer(int32):: u_sxyz, u_tdc, u_stress, u_rxyz
contains
    subroutine open_sxyz()
        open (newunit=u_sxyz, file=file_sxyz, status='old')
    end subroutine

    subroutine close_sxyz()
        close(u_sxyz)
    end subroutine

    subroutine read_sxyz(len_readlines, sxyz)
        integer(int32), intent(in)::len_readlines
        real(real64),intent(out):: sxyz(:,:,:,:)
        integer(int32):: i, j, k, np

        np = size(sxyz, 3)
        do i=1,len_readlines
            read(u_sxyz, *)
            read(u_sxyz, *) ((sxyz(:,k,j,i), k=1,3), j=1,np)
        end do
    end subroutine


    subroutine open_rxyz()
        open (newunit=u_rxyz, file=file_rxyz, status='old')
    end subroutine

    subroutine close_rxyz()
        close(u_rxyz)
    end subroutine

    subroutine read_rxyz(len_readlines, rxyz)
        integer(int32), intent(in)::len_readlines
        real(real64),intent(out):: rxyz(:,:,:)
        integer(int32):: i, j, np

        np = size(rxyz, 2)
        do i=1,len_readlines
            read(u_rxyz, *)
            read(u_rxyz, *) (rxyz(:,j,i), j=1,np)
        end do
    end subroutine


    subroutine read_tdc(ndata, ej)
        integer(int32),intent(in):: ndata
        real(real64),intent(out):: ej(:,:)
        integer(int32):: i

        open(newunit=u_tdc,file=file_tdc,status='old')
            read(u_tdc,*) (ej(i,:), i=1,ndata)
        close(u_tdc)
    end subroutine


    subroutine read_stress(ndata, stress)
        integer(int32),intent(in):: ndata
        real(real64),intent(out):: stress(:,:)
        integer(int32):: i

        open(newunit=u_stress,file=file_stress,status='old')
            read(u_stress,*) (stress(i,:), i=1,ndata)
        close(u_stress)
    end subroutine
end module read_md_results_mod