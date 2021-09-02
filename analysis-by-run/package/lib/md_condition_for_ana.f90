module condition_input_mod
    use,intrinsic :: iso_fortran_env
    implicit none
    private
    type,public:: condition_input_class
        character(100),private:: filepath
        integer(int32):: nstep
        real(real64):: temp0
        real(real64):: dens
        real(real64):: dt
        real(real64):: rc
        integer(int32):: intr
        integer(int32):: intv
        integer(int32):: intd
        integer(int32):: inte
    end type

    interface condition_input_class
        module procedure:: set_file_path_str
    end interface

contains
    function set_file_path_str(filepath) result(molecular_input)
        character(*),intent(in):: filepath
        type(condition_input_class):: molecular_input

        molecular_input%filepath = filepath
        call read_condition_input(molecular_input)
    end function


    subroutine read_condition_input(molecular_input)
        class(condition_input_class):: molecular_input
        integer(int32):: unitnum
        ! open(newunit=unitnum,file='../../input/molecular_input.txt',status='old')
        open(newunit=unitnum,file=molecular_input%filepath,status='old')
            read(unitnum,*)molecular_input%nstep
            read(unitnum,*)molecular_input%temp0
            read(unitnum,*)molecular_input%dens
            read(unitnum,*)molecular_input%dt
            read(unitnum,*)molecular_input%rc
            read(unitnum,*)molecular_input%intr
            read(unitnum,*)molecular_input%intv
            read(unitnum,*)molecular_input%intd
            read(unitnum,*)molecular_input%inte
        close(unitnum)
    end subroutine
end module



module rate_input_mod
    use,intrinsic :: iso_fortran_env
    implicit none
    private
    type,public:: rate_input_class
        character(100),private:: filepath
        real(real64):: elongation
        real(real64):: bond_length
        real(real64):: moment_of_inertia
        real(real64):: packing_fraction
        real(real64):: temperature
        real(real64):: epsilon
        real(real64):: mass
        real(real64):: number_density
    contains
        procedure:: molecular_volume => rate_molecular_volume
        procedure:: molecular_length => rate_molecular_length
    end type
    interface rate_input_class
        module procedure:: set_file_path_str
    end interface
contains
    function set_file_path_str(filepath) result(rate_input)
        character(*),intent(in):: filepath
        type(rate_input_class):: rate_input

        rate_input%filepath = filepath
        call read_rate_input(rate_input)
    end function


    subroutine read_rate_input(rate_input)
        class(rate_input_class):: rate_input
        integer(int32):: unitnum
        ! open(newunit=unitnum,file='../../input/rate_input.txt',status='old')
        open(newunit=unitnum,file=rate_input%filepath,status='old')
            read(unitnum,*) rate_input%elongation
            read(unitnum,*) rate_input%bond_length
            read(unitnum,*) rate_input%moment_of_inertia
            read(unitnum,*) rate_input%packing_fraction
            read(unitnum,*) rate_input%temperature
            read(unitnum,*) rate_input%epsilon
            read(unitnum,*) rate_input%mass
            read(unitnum,*) rate_input%number_density
        close(unitnum)
    end subroutine


    function rate_molecular_volume(rate_input)
        class(rate_input_class):: rate_input
        real(real64):: rate_molecular_volume

        rate_molecular_volume = rate_input%packing_fraction / rate_input%number_density
    end function


    function rate_molecular_length(rate_input)
        real(real64),parameter:: inv3 = 1d0/3d0
        class(rate_input_class):: rate_input
        real(real64):: rate_molecular_length

        rate_molecular_length = rate_input%molecular_volume() ** inv3
    end function
end module



module molecular_input_mod
    use,intrinsic :: iso_fortran_env
    implicit none
    private
    type,public:: molecular_input_class
        character(100),private:: filepath
        real(real64):: atomic_mass(3)
        real(real64):: sigma(3)
        real(real64):: eps(3)
        real(real64):: bond_length
    contains
        procedure:: read_input => read_molecular_input
        procedure:: radius
        procedure:: hbl
    end type

    interface molecular_input_class
        module procedure:: set_file_path_str
    end interface
contains
    function set_file_path_str(filepath) result(molecular_input)
        character(*),intent(in):: filepath
        type(molecular_input_class):: molecular_input

        molecular_input%filepath = filepath
        call read_molecular_input(molecular_input)
    end function


    subroutine read_molecular_input(molecular_input)
        class(molecular_input_class):: molecular_input
        integer(int32):: unitnum
        ! open(unit=32, file='../../input/molecular_input.txt', status='old')
        open(newunit=unitnum, file=molecular_input%filepath, status='old')
            read(unitnum,*) molecular_input%atomic_mass(:) ! g/mol
            read(unitnum,*) molecular_input%sigma(:)       ! m
            read(unitnum,*) molecular_input%eps(:)         ! K
            read(unitnum,*) molecular_input%bond_length    ! m
        close(unitnum)
    end subroutine


    function radius(molecular_input)
        class(molecular_input_class):: molecular_input
        real(real64):: radius

        radius = molecular_input%sigma(1) * 0.5d0
    end function


    function hbl(molecular_input)
        class(molecular_input_class):: molecular_input
        real(real64):: hbl

        hbl = molecular_input%bond_length * 0.5d0
    end function
end module


module rate_mod
    use,intrinsic :: iso_fortran_env
    use rate_input_mod
    use molecular_input_mod
    implicit none
    private
    type:: rate_class
        real(real64):: sigma
        real(real64):: density
        real(real64):: edge_mass
        real(real64):: bond_length
        real(real64):: epsilon
        real(real64):: temperature
    end type

    interface rate_class
        module procedure:: generate_rate
    end interface

    public:: rate_class
contains
    function generate_rate(rate_input, molecule) result(rate)
            real(real64),parameter:: inv3 = 1d0/3d0
            type(rate_input_class),intent(in):: rate_input
            type(molecular_input_class),intent(in):: molecule
            type(rate_class):: rate
            real(real64):: r_vol, r_cell
            real(real64):: r, l, nu, dn_sigma, dn_bond_length

            rate%density = rate_input%number_density
            rate%epsilon = rate_input%epsilon
            rate%temperature = rate_input%temperature

            r_cell = rate_input%molecular_length()
            r = molecule%radius()
            l = molecule%hbl()
            
            nu = f(r,l)
            dn_sigma = f(r, rate_input%elongation*l)
            dn_bond_length = f(r/rate_input%elongation, l)

            rate%sigma = nu/dn_sigma * r_cell
            rate%bond_length = nu*r_cell/dn_bond_length * rate_input%bond_length
            rate%edge_mass = 1d0/rate_input%moment_of_inertia * (dn_bond_length/(nu*r_cell))**2
        contains
            function f(r,l) result(ret)
                real(real64),intent(in):: r,l
                real(real64):: ret
        
                ret = 2*r*r*r + 3*r*r*l - l*l*l
            end function
        end function
end module



module md_condition_mod
    use,intrinsic :: iso_fortran_env
    use rate_mod
    use condition_input_mod
    use rate_input_mod
    use molecular_input_mod
    real(real64),parameter:: an=6.0221367d+23
    type md_condition_class
        integer(int32):: np = 500
        integer(int32):: nstep
        real(real64):: temp0
        real(real64):: dens
        real(real64):: dt
        real(real64):: rc
        integer(int32):: intr
        integer(int32):: intv
        integer(int32):: intd
        integer(int32):: inte
        real(real64):: atomic_mass(3)
        real(real64):: sigma(3)
        real(real64):: eps(3)
        real(real64):: bond_length
    contains
        procedure:: tmass => calc_tmass
        procedure:: system_volume => calc_system_volume
        procedure:: system_cell => calc_system_cell
    end type

    interface md_condition_class
        module procedure:: generate_from_inputs 
    end interface
contains
    function generate_from_inputs(condition_input, molecular_input, rate_input) result(md_condition)
        type(condition_input_class),intent(in):: condition_input
        type(molecular_input_class),intent(in):: molecular_input
        type(rate_input_class),intent(in):: rate_input
        type(rate_class):: rate
        type(md_condition_class):: md_condition 

        ! generate rate
        rate = rate_class(molecule=molecular_input, rate_input=rate_input)

        ! copy from condition_input
        md_condition%nstep = condition_input%nstep
        md_condition%temp0 = condition_input%nstep * rate%temperature
        md_condition%dens = condition_input%dens * rate%density
        md_condition%dt = condition_input%dt
        md_condition%rc = condition_input%rc
        md_condition%intr = condition_input%intr
        md_condition%intv = condition_input%intv
        md_condition%intd = condition_input%intd
        md_condition%inte = condition_input%inte

        ! copy from molecular_input
        md_condition%atomic_mass(1:2) = molecular_input%atomic_mass(1:2) * rate%edge_mass
        md_condition%atomic_mass(3) = sum(molecular_input%atomic_mass)*rate_input%mass - sum(md_condition%atomic_mass(1:2))
        md_condition%atomic_mass(:) = md_condition%atomic_mass(:) /an/1000d0 ! g/mol => g => kg

        md_condition%sigma(:) = molecular_input%sigma(:) * rate%sigma
        md_condition%bond_length = molecular_input%bond_length * rate%bond_length
    end function


    real(real64) function calc_tmass(md_condition)
        class(md_condition_class),intent(in):: md_condition
    
        calc_tmass = sum(md_condition%atomic_mass)
    end function

    real(real64) function calc_system_volume(md_condition)
        class(md_condition_class),intent(in):: md_condition

        calc_system_volume = md_condition%np * md_condition%tmass()/md_condition%dens
    end function


    real(real64) function calc_system_cell(md_condition)
        class(md_condition_class),intent(in):: md_condition

        calc_system_cell = md_condition%system_volume() ** (1d0/3d0)
    end function
end module



module md_condition_for_ana_mod
    use md_condition_mod
    use condition_input_mod
    use rate_input_mod
    use molecular_input_mod
contains 
    function generate_md_condition() result(md_condition)
        character(100),parameter:: file_condition_input = '../../../input/condition_input.txt'
        character(100),parameter:: file_molecular_input = '../../../input/molecular_input.txt'
        character(100),parameter:: file_rate_input = '../../../input/rate_input.txt'
        type(condition_input_class):: condition_input
        type(molecular_input_class):: molecular_input
        type(rate_input_class):: rate_input
        type(md_condition_class):: md_condition

        condition_input = condition_input_class(trim(file_condition_input))
        molecular_input = molecular_input_class(trim(file_molecular_input))
        rate_input = rate_input_class(trim(file_rate_input))

        md_condition = md_condition_class( &
            & condition_input=condition_input, &
            & molecular_input=molecular_input, &
            & rate_input=rate_input)
    end function


    subroutine load_condition_for_temp_ana(ndata)
        integer(int32),intent(out):: ndata
        type(md_condition_class):: md_condition

        md_condition = generate_md_condition()
        ndata = md_condition%nstep / md_condition%inte
    end subroutine


    subroutine load_condition_for_gr_ana(ndata, rc, cell)
        integer(int32),intent(out):: ndata
        real(real64),intent(out):: rc, cell
        type(md_condition_class):: md_condition

        md_condition = generate_md_condition()    
        
        ndata = md_condition%nstep / md_condition%intr
        rc = md_condition%rc
        cell = md_condition%system_cell()
    end subroutine


    subroutine load_condition_for_thcd_ana(ndata, dt, vol)
        integer(int32),intent(out):: ndata
        real(real64),intent(out):: dt, vol
        type(md_condition_class):: md_condition

        md_condition = generate_md_condition()

        ndata = md_condition%nstep
        dt = md_condition%dt
        vol = md_condition%system_volume()
    end subroutine


    subroutine load_condition_for_viscousity_ana(ndata, dt, vol)
        integer(int32),intent(out):: ndata
        real(real64),intent(out):: dt, vol
        type(md_condition_class):: md_condition

        md_condition = generate_md_condition()

        ndata = md_condition%nstep
        dt = md_condition%dt
        vol = md_condition%system_volume()
    end subroutine


    subroutine load_condition_for_msd_ana(ndata, dt, intd)
        integer(int32),intent(out):: ndata, intd
        real(real64),intent(out):: dt
        type(md_condition_class):: md_condition

        md_condition = generate_md_condition()

        ndata = md_condition%nstep / md_condition%intd
        dt = md_condition%dt
        intd = md_condition%intd
    end subroutine
end module
