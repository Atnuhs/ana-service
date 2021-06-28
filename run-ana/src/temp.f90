program main
    use,intrinsic :: iso_fortran_env
    use read_condition_mod
    use representative_value_mod
    implicit none
    integer(int32):: ndata,i
    real(real64),allocatable:: temp(:)

    call input_condition(ndata=ndata)
    allocate(temp(ndata))
    call read_enrg(temp=temp, ndata=ndata)
    call output_temp_mean(temp=temp, ndata=ndata)
contains
    subroutine input_condition(ndata)
        integer(int32),intent(out):: ndata
        type(mdda_type):: mdda

        call read_mdda(mdda)
        ndata = mdda%nstep/mdda%inte
    end subroutine


    subroutine read_enrg(temp,ndata)
        character(100),parameter:: file_enrg='../enrg.dat'
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


    subroutine output_temp_mean(temp, ndata)
        character(100),parameter:: file_temp_mean='temp/temp_mean.dat'
        integer(int32),intent(in):: ndata
        real(real64),intent(in):: temp(:)
        integer(int32):: i, u_temp_mean
        real(real64):: temp_mean, se

        temp_mean = mean(temp, se)
        open(newunit=u_temp_mean, file=file_temp_mean, status='replace')
            write(u_temp_mean, *) temp_mean, se
        close(u_temp_mean)
    end subroutine
end program main