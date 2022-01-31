module read_md_result_mod
    use,intrinsic:: iso_fortran_env
    implicit none
contains
    subroutine read_rxyz(file_rxyz, ndata, np, rxyz)
        character(*),intent(in):: file_rxyz
        real(real64),intent(out):: rxyz(:,:,:)
        integer(int32),intent(in):: ndata, np
        integer(int32):: i,j,u_rxyz

        open(newunit=u_rxyz, file=file_rxyz, status='old')
            do i=1,ndata
                read(u_rxyz,*)
                read(u_rxyz,*) (rxyz(:,j,i), j=1,np)
            end do
        close(u_rxyz)
    end subroutine


    subroutine read_dxyz(file_dxyz, ndata, np, dxyz)
        ! dxyzのデータ読み込み
        character(*),intent(in):: file_dxyz
        real(real64),intent(out):: dxyz(:,:,:)
        integer(int32),intent(in):: np, ndata
        integer(int32):: u_dxyz, i, j, k

        open(newunit=u_dxyz, file=file_dxyz, status='old')
        read(u_dxyz,*)
            do i=1,ndata
                read(u_dxyz,*)
                read(u_dxyz,*) ((dxyz(i,j,k), k=1,3), j=1,np)
            end do
        close(u_dxyz)
    end subroutine


    subroutine read_tdphi(file_tdphi, ndata, np, tdphi)
        ! tdphiのデータ読み込み
        character(*),intent(in):: file_tdphi
        integer(int32),intent(in):: np, ndata
        real(real64),intent(out):: tdphi(:,:,:)
        integer(int32):: u_tdphi, i, j, j1, j2, c

        open(newunit=u_tdphi, file=file_tdphi, status='old')
            c=0
            do i=1,ndata
                read(u_tdphi,*)
                c=c+1
                do j=1,np/2
                    j1 = 2*j-1
                    j2 = 2*j
                    read(u_tdphi,*) tdphi(i,j1,1), tdphi(i,j1,2), &
                        & tdphi(i,j1,3), tdphi(i,j2,1), tdphi(i,j2,2), tdphi(i,j2,3)
                    c=c+1
                    ! print*, c
                end do
            end do
        close(u_tdphi)
    end subroutine


    subroutine read_tdc(file_tdc, ndata, ej)
        character(*),intent(in):: file_tdc
        integer(int32),intent(in):: ndata
        real(real64),intent(out):: ej(:,:)
        integer(int32):: i,u_tdc

        open(newunit=u_tdc,file=file_tdc,status='old')
            read(u_tdc,*) (ej(i,:), i=1,ndata)
        close(u_tdc)
    end subroutine


    subroutine read_temp_from_enrgdat(file_enrg, temp, ndata)
        character(*),intent(in):: file_enrg
        integer(int32),intent(in):: ndata
        real(real64),intent(out):: temp(:)
        integer(int32):: i, u_enrg, r1, r8
        real(real64):: r3_7(5)

        open(newunit=u_enrg, file=file_enrg, status='old')
            do i=1,ndata
                read(u_enrg,*)  r1, temp(i), r3_7(:), r8
            end do
        close(u_enrg)
    end subroutine
end module



module read_file_mod
    use,intrinsic :: iso_fortran_env
    implicit none
contains
    subroutine read_x(file_data, x)
        character(*),intent(in):: file_data
        real(real64),intent(out):: x
        integer(int32):: u_data

        open(newunit=u_data, file=file_data, status='old')
            read(u_data, *) x
        close(u_data)
    end subroutine


    subroutine read_x_xsd(file_data, x, x_sd)
        character(*),intent(in):: file_data
        real(real64),intent(out):: x, x_sd
        integer(int32):: u_data

        open(newunit=u_data, file=file_data, status='old')
            read(u_data, *) x, x_sd
        close(u_data)
    end subroutine


    subroutine read_arx_ary(file_data, data_len, arx, ary)
        integer(int32),intent(in):: data_len
        character(*),intent(in):: file_data
        real(real64),intent(out):: arx(:), ary(:)
        integer(int32):: u_data, i

        open(newunit=u_data, file=file_data, status='old')
            read(u_data, *) (arx(i), ary(i), i=1,data_len)
        close(u_data)
    end subroutine


    subroutine read_arx_ary_arysd(file_data, data_len, arx, ary, ary_sd)
        integer(int32),intent(in):: data_len
        character(*),intent(in):: file_data
        real(real64),intent(out):: arx(:), ary(:), ary_sd(:)
        integer(int32):: u_data, i

        open(newunit=u_data, file=file_data, status='old')
            read(u_data, *) (arx(i), ary(i), ary_sd(i), i=1,data_len)
        close(u_data)
    end subroutine


    subroutine read_arx_ary_arz(file_data, x_len, y_len, arx, ary, arz)
        integer(int32),intent(in):: x_len, y_len
        character(*),intent(in):: file_data
        real(real64),intent(out):: arx(x_len), ary(y_len), arz(y_len, x_len)
        integer(int32):: u_data, ix, iy

        open(newunit=u_data, file=file_data, status='old')
            read(u_data, *) ((arx(ix), ary(iy), arz(iy,ix), iy=1,y_len), ix=1,x_len)
        close(u_data)
    end subroutine
end module read_file_mod


module write_file_mod
    use,intrinsic :: iso_fortran_env
    implicit none
contains
    subroutine write_x(file_data, x)
        character(*),intent(in):: file_data
        real(real64),intent(in):: x
        integer(int32):: u_data

        open(newunit=u_data, file=file_data, status='replace')
            write(u_data, *) x
        close(u_data)
    end subroutine


    subroutine write_x_xsd(file_data, x, x_sd)
        character(*),intent(in):: file_data
        real(real64),intent(in):: x, x_sd
        integer(int32):: u_data

        open(newunit=u_data, file=file_data, status='replace')
            write(u_data, *) x, x_sd
        close(u_data)
    end subroutine


    subroutine write_arx_ary(file_data, data_len, arx, ary)
        integer(int32),intent(in):: data_len
        character(*),intent(in):: file_data
        real(real64),intent(in):: arx(:), ary(:)
        integer(int32):: u_data, i

        open(newunit=u_data, file=file_data, status='replace')
            write(u_data, *) (arx(i), ary(i), i=1,data_len)
        close(u_data)
    end subroutine


    subroutine write_arx_ary_arysd(file_data, data_len, arx, ary, ary_sd)
        integer(int32),intent(in):: data_len
        character(*),intent(in):: file_data
        real(real64),intent(in):: arx(:), ary(:), ary_sd(:)
        integer(int32):: u_data, i

        open(newunit=u_data, file=file_data, status='replace')
            write(u_data, '(3e20.10)') (arx(i), ary(i), ary_sd(i), i=1,data_len)
        close(u_data)
    end subroutine


    subroutine write_arx_ary_arz(file_data, x_len, y_len, arx, ary, arz)
        integer(int32),intent(in):: x_len, y_len
        character(*),intent(in):: file_data
        real(real64),intent(in):: arx(x_len), ary(y_len), arz(y_len, x_len)
        integer(int32):: u_data, ix, iy

        open(newunit=u_data, file=file_data, status='replace')
            write(u_data, *) ((arx(ix), ary(iy), arz(iy,ix), iy=1,y_len), ix=1,x_len)
        close(u_data)
    end subroutine
end module write_file_mod




module io_file_mod
    use read_md_result_mod
    use read_file_mod
    use write_file_mod
contains
    function rundir(run)
        integer(int32),intent(in):: run
        character(5):: rundir

        write(rundir, "('run', I2.2)") run
    end function
end module