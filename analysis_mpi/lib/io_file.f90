module io_md_result_mod
    use,intrinsic:: iso_fortran_env
    implicit none
contains
    subroutine read_rxyz(file_rxyz, rxyz, ndata, np)
        character(*),intent(in):: file_rxyz
        real(real64),intent(out):: rxyz(:,:,:)
        integer(int32),intent(in):: ndata,np
        integer(int32):: i,j,u_rxyz

        open(newunit=u_rxyz, file=file_rxyz, status='old')
            do i=1,ndata
                read(u_rxyz,*)
                read(u_rxyz,*) (rxyz(:,j,i), j=1,np)
            end do
        close(u_rxyz)
    end subroutine
end module



module io_file_mod
    use,intrinsic :: iso_fortran_env
    use io_md_result_mod
    implicit none
contains
    function rundir(run)
        integer(int32),intent(in):: run
        character(5):: rundir

        write(rundir, "('run', I2.2)") run
    end function


    subroutine read_x(file_data, y)
        character(*),intent(in):: file_data
        real(real64),intent(out):: y
        integer(int32):: u_data

        open(newunit=u_data, file=file_data, status='old')
            read(u_data, *) y
        close(u_data)
    end subroutine


    subroutine read_x_xsd(file_data, y, y_sd)
        character(*),intent(in):: file_data
        real(real64),intent(out):: y, y_sd
        integer(int32):: u_data

        open(newunit=u_data, file=file_data, status='old')
            read(u_data, *) y, y_sd
        close(u_data)
    end subroutine


    subroutine read_x_y(data_len, file_data, x, y)
        integer(int32),intent(in):: data_len
        character(*),intent(in):: file_data
        real(real64),intent(out):: x(:), y(:)
        integer(int32):: u_data, i

        open(newunit=u_data, file=file_data, status='old')
            read(u_data, *) (x(i), y(i), i=1,data_len)
        close(u_data)
    end subroutine


    subroutine read_x_y_ysd(data_len, file_data, x, y, y_sd)
        integer(int32),intent(in):: data_len
        character(*),intent(in):: file_data
        real(real64),intent(out):: x(:), y(:), y_sd(:)
        integer(int32):: u_data, i

        open(newunit=u_data, file=file_data, status='old')
            read(u_data, *) (x(i), y(i), y_sd(i), i=1,data_len)
        close(u_data)
    end subroutine


    subroutine read_x_y_z(x_len, y_len, file_data, x, y, z)
        integer(int32),intent(in):: x_len, y_len
        character(*),intent(in):: file_data
        real(real64),intent(out):: x(x_len), y(y_len), z(y_len, x_len)
        integer(int32):: u_data, ix, iy

        open(newunit=u_data, file=file_data, status='old')
            read(u_data, *) ((x(ix), y(iy), z(iy,ix), iy=1,y_len), ix=1,x_len)
        close(u_data)
    end subroutine
end module io_file_mod