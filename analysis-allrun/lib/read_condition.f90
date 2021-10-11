module read_condition_mod
    use,intrinsic :: iso_fortran_env
    implicit none
    type condition_type
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
    subroutine read_condition(condition)
        type(condition_type),intent(out):: condition
        integer(int32):: u_mdda

        open(newunit=u_mdda,file='../input/condition_input.txt',status='old')
            read(u_mdda,*) condition%nstep
            read(u_mdda,*) condition%temp0
            read(u_mdda,*) condition%dens
            read(u_mdda,*) condition%dt
            read(u_mdda,*) condition%rc
            read(u_mdda,*) condition%intr
            read(u_mdda,*) condition%intv
            read(u_mdda,*) condition%intd
            read(u_mdda,*) condition%inte
        close(u_mdda)
    end subroutine

    
    subroutine read_rate(rate)
        type(rate_type),intent(out):: rate
        integer(int32):: u_rate

        open(newunit=u_rate, file='../input/rate_input.txt', status='old')
            read(u_rate,*) rate%elongation
            read(u_rate,*) rate%bond_length
            read(u_rate,*) rate%moment_of_inertia
            read(u_rate,*) rate%packing_fraction
            read(u_rate,*) rate%temperature
            read(u_rate,*) rate%eps
            read(u_rate,*) rate%mass
            read(u_rate,*) rate%nd
        close(u_rate)
    end subroutine


    subroutine read_molecular(molecular)
        type(molecular_type):: molecular
        integer(int32):: u_molecular

        open(newunit=u_molecular, file='../input/molecular_input.txt', status='old')
            read(u_molecular,*) molecular%mass(:) ! g/mol
            read(u_molecular,*) molecular%sigma(:)       ! m
            read(u_molecular,*) molecular%eps(:)         ! K
            read(u_molecular,*) molecular%bond_length    ! m
        close(u_molecular)
    end subroutine


    subroutine read_input(condition, rate, molecular)
        type(condition_type):: condition
        type(rate_type):: rate
        type(molecular_type):: molecular

        call read_condition(condition)
        call read_rate(rate)
        call read_molecular(molecular)
    end subroutine
end module