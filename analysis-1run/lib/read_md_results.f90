module read_md_results_mod
    use,intrinsic :: iso_fortran_env
    implicit none
    character(100),parameter:: file_sxyz='../sxyz.dat'
    character(100),parameter:: file_tdc='../tdc.dat'
    character(100),parameter:: file_stress='../stress.dat'

    integer(int32):: u_sxyz, u_tdc, u_stress
contains
    subroutine open_sxyz()
        open (newunit=u_sxyz, file=file_sxyz, status='old')
    end subroutine

    subroutine close_sxyz()
        close(u_sxyz)
    end subroutine

    subroutine read_sxyz(read_len, sxyz)
        integer(int32), intent(in)::read_len
        real(real64),intent(out):: sxyz(:,:,:,:)
        integer(int32):: i, j, k, np

        np = size(sxyz, 3)
        do i=1,read_len
            read(u_sxyz, *)
            read(u_sxyz, *) ((sxyz(:,k,j,i), k=1,3), j=1,np)
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