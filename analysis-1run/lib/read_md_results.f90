module read_md_results_mod
    use,intrinsic :: iso_fortran_env
    implicit none
    character(100):: file_sxyz='../sxyz.dat'
    integer(int32):: u_sxyz
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
end module read_md_results_mod