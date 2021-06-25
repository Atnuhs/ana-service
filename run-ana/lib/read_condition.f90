module read_condition_mod
    use,intrinsic :: iso_fortran_env
    implicit none
    type mdda_type
        integer(int32):: nstep
        integer(int32):: intr, intv, intd, inte
        real(real64):: temp0, dens, dt, rc
    end type
    type rate_type
        real(real64):: elongation
        real(real64):: bond_length
        real(real64):: moment_of_inertia
        real(real64):: packing_fraction
        real(real64):: temperature
        real(real64):: eps
        real(real64):: mass
        real(real64):: nd
    end type
    type molecular_type
        real(real64):: mass(3)
        real(real64):: sigma(3)
        real(real64):: eps(3)
        real(real64):: bond_length
    end type
contains
    subroutine read_mdda(mdda)
        type(mdda_type),intent(out):: mdda
        integer(int32):: u_mdda

        open(newunit=u_mdda,file='../../mdda.inpt',status='old')
            read(u_mdda,*) mdda%nstep
            read(u_mdda,*) mdda%temp0
            read(u_mdda,*) mdda%dens
            read(u_mdda,*) mdda%dt
            read(u_mdda,*) mdda%rc
            read(u_mdda,*) mdda%intr
            read(u_mdda,*) mdda%intv
            read(u_mdda,*) mdda%intd
            read(u_mdda,*) mdda%inte
        close(u_mdda)
    end subroutine

    
    subroutine read_rate(rate)
        type(rate_type),intent(out):: rate
        integer(int32):: u_rate

        open(newunit=u_rate, file='../../inpt2.dat', status='old')
            read(u_rate,*) rate%elongation
            read(u_rate,*) rate%bond_length
            read(u_rate,*) rate%moment_of_inertia
            read(u_rate,*) rate%packing_fraction
            read(u_rate,*) rate%temperature
            read(u_rate,*) 
            read(u_rate,*) rate%eps
            read(u_rate,*) rate%mass
            read(u_rate,*) rate%nd
        close(u_rate)
    end subroutine


    subroutine read_molecular(molecular)
        type(molecular_type):: molecular
        integer(int32):: u_molecular

        open(newunit=u_molecular, file='../../molecular.inpt', status='old')
            read(u_molecular,*) molecular%mass(:) ! g/mol
            read(u_molecular,*) molecular%sigma(:)       ! m
            read(u_molecular,*) molecular%eps(:)         ! K
            read(u_molecular,*) molecular%bond_length    ! m
        close(u_molecular)
    end subroutine


    subroutine read_condition(mdda, rate, molecular)
        type(mdda_type):: mdda
        type(rate_type):: rate
        type(molecular_type):: molecular

        call read_mdda(mdda)
        call read_rate(rate)
        call read_molecular(molecular)
    end subroutine
end module