program main
    use,intrinsic :: iso_fortran_env
    use md_condition_for_ana_mod
    use read_md_results_mod
    implicit none
    integer(int32):: nsep=500
    integer(int32):: ndata, np, len_readlines
    integer(int32):: isep
    integer(int32),allocatable:: units_trajectory_files(:)
    character(100),allocatable:: name_trajectory_files(:)
    real(real64),allocatable:: rxyz(:,:,:)
    
    ! 準備
    call load_condition_for_transpose_rxyz(ndata, np)
    len_readlines = ndata / nsep
    allocate(rxyz(3,np,len_readlines))
    allocate(units_trajectory_files(np))
    allocate(name_trajectory_files(np))


    call open_trajectory_files()
    call open_rxyz()
        do isep =1, nsep
            print*, isep, nsep
            call read_rxyz(len_readlines, rxyz)
            call write_trajectory_files()
        end do
    call close_rxyz()
contains
    subroutine open_trajectory_files()
        integer(int32):: i
        character(4):: c_np

        do i=1,np
            write(c_np, '(i3.3)') i
            name_trajectory_files(i) = './transpose_rxyz/trj_' // trim(c_np) // '.txt'
            open(newunit=units_trajectory_files(i), file=name_trajectory_files(i), status='replace')
        end do
    end subroutine


    subroutine close_trajectory_files()
        integer(int32):: i

        do i=1,np
            close(units_trajectory_files(i))
        end do
    end subroutine


    subroutine write_trajectory_files()
        integer(int32):: i, j

        do i=1,len_readlines
            do j=1,np
                write(units_trajectory_files(j), *) rxyz(:,j,i)
            end do
        end do
    end subroutine
end program main