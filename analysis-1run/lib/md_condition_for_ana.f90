module condition_input_mod
    use,intrinsic :: iso_fortran_env
    implicit none
    private
    type,public:: condition_input_class
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
        module procedure:: constructor_condition_input_class
    end interface

contains
    function constructor_condition_input_class(file_condition_input) result(molecular_input)
        ! condition_input.txtのパスを渡すとそれを読み込んだcondition_input_classを返してくれる。
        character(*),intent(in):: file_condition_input
        type(condition_input_class):: molecular_input
        integer(int32):: u_file_rate_input

        open(newunit=u_file_rate_input,file=file_condition_input,status='old')
            read(u_file_rate_input,*)molecular_input%nstep
            read(u_file_rate_input,*)molecular_input%temp0
            read(u_file_rate_input,*)molecular_input%dens
            read(u_file_rate_input,*)molecular_input%dt
            read(u_file_rate_input,*)molecular_input%rc
            read(u_file_rate_input,*)molecular_input%intr
            read(u_file_rate_input,*)molecular_input%intv
            read(u_file_rate_input,*)molecular_input%intd
            read(u_file_rate_input,*)molecular_input%inte
        close(u_file_rate_input)
    end function
end module



module rate_input_mod
    use,intrinsic :: iso_fortran_env
    implicit none
    private
    type,public:: rate_input_class
        real(real64):: elongation
        real(real64):: bond_length
        real(real64):: moment_of_inertia
        real(real64):: packing_fraction
        real(real64):: temperature
        real(real64):: epsilon
        real(real64):: mass
        real(real64):: number_density
    contains
        ! いらないんじゃないかな。
        procedure:: molecular_volume_ratio
        procedure:: molecular_homothetic_ratio
    end type
    interface rate_input_class
        module procedure:: constructor_rate_input_class
    end interface
contains
    function constructor_rate_input_class(filepath) result(rate_input)
        ! rate_input.txtのパスを渡すとrate_input_classを返してくれる。
        character(*),intent(in):: filepath
        type(rate_input_class):: rate_input
        integer(int32):: u_file_rate_input

        open(newunit=u_file_rate_input, file=filepath, status='old')
            read(u_file_rate_input,*) rate_input%elongation
            read(u_file_rate_input,*) rate_input%bond_length
            read(u_file_rate_input,*) rate_input%moment_of_inertia
            read(u_file_rate_input,*) rate_input%packing_fraction
            read(u_file_rate_input,*) rate_input%temperature
            read(u_file_rate_input,*) rate_input%epsilon
            read(u_file_rate_input,*) rate_input%mass
            read(u_file_rate_input,*) rate_input%number_density
        close(u_file_rate_input)
    end function


    function molecular_volume_ratio(rate_input)
        ! 体積比
        class(rate_input_class):: rate_input
        real(real64):: molecular_volume_ratio

        molecular_volume_ratio = rate_input%packing_fraction / rate_input%number_density
    end function


    function molecular_homothetic_ratio(rate_input)
        ! 相似比
        real(real64),parameter:: inv3 = 1d0/3d0
        class(rate_input_class):: rate_input
        real(real64):: molecular_homothetic_ratio

        molecular_homothetic_ratio = rate_input%molecular_volume_ratio() ** inv3
    end function
end module



module molecular_input_mod
    use,intrinsic :: iso_fortran_env
    implicit none
    private
    type,public:: molecular_input_class
        real(real64):: atomic_mass(3)
        real(real64):: sigma(3)
        real(real64):: eps(3)
        real(real64):: bond_length
    contains
        procedure:: atom_radius
        procedure:: half_bond_length
    end type

    interface molecular_input_class
        module procedure:: constructor_molecular_input_class
    end interface
contains
    function constructor_molecular_input_class(filepath) result(molecular_input)
        ! molecular_input.txtのパスを渡渡すとコンストラクトしてくれる。
        character(*),intent(in):: filepath
        type(molecular_input_class):: molecular_input
        integer(int32):: u_file_rate_input


        open(newunit=u_file_rate_input, file=filepath, status='old')
            read(u_file_rate_input,*) molecular_input%atomic_mass(:) ! g/mol
            read(u_file_rate_input,*) molecular_input%sigma(:)       ! m
            read(u_file_rate_input,*) molecular_input%eps(:)         ! K
            read(u_file_rate_input,*) molecular_input%bond_length    ! m
        close(u_file_rate_input)
    end function


    function atom_radius(molecular_input)
        class(molecular_input_class):: molecular_input
        real(real64):: atom_radius

        atom_radius = molecular_input%sigma(1) * 0.5d0
    end function


    function half_bond_length(molecular_input)
        class(molecular_input_class):: molecular_input
        real(real64):: half_bond_length

        half_bond_length = molecular_input%bond_length * 0.5d0
    end function
end module


module rate_mod
    ! rate_input.txtとmolecular_inputクラスから、実際にプログラム上で変化させる値の倍率(rate_class)を計算
    use,intrinsic :: iso_fortran_env
    use rate_input_mod
    use molecular_input_mod
    implicit none
    private
    type:: rate_class
        real(real64):: tmass
        real(real64):: sigma
        real(real64):: density
        real(real64):: edge_mass
        real(real64):: bond_length
        real(real64):: epsilon
        real(real64):: temperature
    end type

    interface rate_class
        module procedure:: generate_rate, generate_rate_reference
    end interface

    public:: rate_class
contains
    function generate_rate(rate_input, molecule) result(rate)
            real(real64),parameter:: inv3 = 1d0/3d0
            type(rate_input_class),intent(in):: rate_input
            type(molecular_input_class),intent(in):: molecule
            type(rate_class):: rate
            real(real64):: homothetic_ratio
            real(real64):: r, l, numerator, denominator_sigma, denominator_bond_length

            ! そのままのもの =============================================================================
            rate%tmass = rate_input%mass
            rate%density = rate_input%number_density * rate_input%mass ! number_densityの変化率はdensityの変化率と等価
            rate%epsilon = rate_input%epsilon
            rate%temperature = rate_input%temperature

            ! 計算が必要なもの ============================================================================
            ! 前計算
            homothetic_ratio = rate_input%molecular_homothetic_ratio() ! 分子を相似的に変化させる際の相似比
            r = molecule%atom_radius()
            l = molecule%half_bond_length()
            
            numerator = f(r,l)
            denominator_sigma = f(r, rate_input%elongation*l)
            denominator_bond_length = f(r/rate_input%elongation, l)

            ! rate計算
            rate%sigma = (numerator/denominator_sigma)**inv3 * homothetic_ratio
            rate%bond_length = (numerator/denominator_bond_length)**inv3 * homothetic_ratio * rate_input%bond_length
            rate%edge_mass = rate_input%moment_of_inertia * ((denominator_bond_length/numerator)**inv3 / homothetic_ratio)**2
        contains
            function f(r,l) result(ret)
                real(real64),intent(in):: r,l
                real(real64):: ret
        
                ret = 2*r*r*r + 3*r*r*l - l*l*l
            end function
        end function


        function generate_rate_reference() result(rate)
            type(rate_class):: rate

            rate%tmass = 1d0
            rate%density = 1d0
            rate%epsilon = 1d0
            rate%temperature = 1d0
            rate%sigma = 1d0
            rate%bond_length = 1d0
            rate%edge_mass = 1d0
        end function
end module



module md_condition_mod
    use,intrinsic :: iso_fortran_env
    use const_mod
    use rate_mod
    use condition_input_mod
    use rate_input_mod
    use molecular_input_mod
    implicit none
    character(100),parameter:: file_condition_input = '../../../input/condition_input.txt'
    character(100),parameter:: file_molecular_input = '../../../input/molecular_input.txt'
    character(100),parameter:: file_rate_input = '../../../input/rate_input.txt'
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
        procedure:: molecular_volume => calc_molecular_volume
        procedure:: number_density => calc_number_density
        procedure:: packing_fraction => calc_packing_fraction
        procedure:: moment_of_inertia => calc_moment_of_inertia
        procedure:: elongation => calc_elongation
        procedure:: print => print_md_condition
    end type


    interface md_condition_class
        module procedure:: generate_from_inputs 
    end interface
contains
    function generate_from_inputs() result(md_condition)
        type(condition_input_class):: condition_input
        type(molecular_input_class):: molecular_input
        type(rate_input_class):: rate_input
        type(rate_class):: rate, rate_reference
        type(md_condition_class):: md_condition, md_condition_reference

        ! load input files
        condition_input = condition_input_class(file_condition_input)
        molecular_input = molecular_input_class(file_molecular_input)
        rate_input = rate_input_class(file_rate_input)

        ! generate rate
        rate = rate_class(molecule=molecular_input, rate_input=rate_input)
        rate_reference = rate_class()

        md_condition = generate_md_condition(condition_input, molecular_input, rate)
        md_condition_reference = generate_md_condition(condition_input, molecular_input, rate_reference)

        ! call md_condition%print()
        ! call compare_with_reference_condition(md_condition, md_condition_reference)
    end function


    function generate_md_condition(condition_input, molecular_input, rate) result(md_condition)
        type(condition_input_class), intent(in):: condition_input
        type(molecular_input_class), intent(in):: molecular_input
        type(rate_class), intent(in):: rate
        type(md_condition_class):: md_condition

        ! copy from condition_input
        md_condition%nstep = condition_input%nstep
        md_condition%temp0 = condition_input%temp0 * rate%temperature
        md_condition%dens = condition_input%dens * rate%density
        md_condition%dt = condition_input%dt
        md_condition%rc = condition_input%rc
        md_condition%intr = condition_input%intr
        md_condition%intv = condition_input%intv
        md_condition%intd = condition_input%intd
        md_condition%inte = condition_input%inte

        ! copy from molecular_input
        md_condition%atomic_mass(1:2) = molecular_input%atomic_mass(1:2) * rate%edge_mass
        md_condition%atomic_mass(3) = sum(molecular_input%atomic_mass)*rate%tmass - sum(md_condition%atomic_mass(1:2))
        md_condition%atomic_mass(:) = md_condition%atomic_mass(:) /an/1000d0 ! g/mol => g => kg

        md_condition%sigma(:) = molecular_input%sigma(:) * rate%sigma
        md_condition%eps(:) = molecular_input%eps(:) * rate%epsilon * kbt ! K => J
        md_condition%bond_length = molecular_input%bond_length * rate%bond_length

        md_condition%rc = min(md_condition%system_cell()*0.5d0, 3*(md_condition%sigma(1)+md_condition%bond_length))
    end function


    subroutine print_md_condition(md_condition)
        class(md_condition_class),intent(in):: md_condition

        print"('@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@')"
        print"('@@@@@ CONDITION_CHECK @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@')"
        print"('NP      :', i10.0)", md_condition%np
        print"('NSTEP   :', i10.0)", md_condition%nstep
        print"('TEMP0   :', f10.2, ' K')", md_condition%temp0
        print"('DENS    :', f10.2, ' kgm^-3')", md_condition%dens
        print"('DT      :', f10.2, ' fs')", md_condition%dt * 1d15
        print"('CUT_OFF :', f10.2, ' A')", md_condition%rc * 1d10
        print"('INTR    :', i10.0)", md_condition%intr
        print"('INTV    :', i10.0)", md_condition%intv
        print"('INTD    :', i10.0)", md_condition%intd
        print"('INTE    :', i10.0)", md_condition%inte
        print"('==========================================================================')"
        print"('ATOMIC_MASS   :', 3(f10.3, ' mol'))", md_condition%atomic_mass(:) * an
        print"('SIGMA         :', 3(f10.3, ' A  '))", md_condition%sigma(:) * 1d10
        print"('EPSILON       :', 3(f10.3, ' K  '))", md_condition%eps(:) / kbt
        print"('BOND_LENGTH   :', f10.3, ' A')", md_condition%bond_length * 1d10
        print"('TOTAL_MASS    :', f10.3, ' mol')", md_condition%tmass() * an
        print"('MOLECULAR_VOL :', f10.3, ' m^3')", md_condition%molecular_volume()
        print"('SYSTEM_VOL    :', e10.3, ' m^3')", md_condition%system_volume()
        print"('SYSTEM_CELL   :', f10.3, ' A')", md_condition%system_cell() * 1d10
        print"('==========================================================================')"
        print"('NUMBER_DENSITY    :', e10.3, ' M^-3')", md_condition%number_density() 
        print"('PACKING_FRACTION  :', f10.3)", md_condition%packing_fraction()
        print"('VOID_FRACTION     :', f10.3)", 1d0-md_condition%packing_fraction()
        print"('MOMENT_OF_INERTIA :', e10.3, ' kgm^2')", md_condition%moment_of_inertia()
        print"('ELONGATION        :', f10.3)", md_condition%elongation()
        print"('@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@')"
    end subroutine print_md_condition


    subroutine compare_with_reference_condition(md_condition, md_condition_ref)
        type(md_condition_class):: md_condition, md_condition_ref

        print"('@@@@@ RATIO_CHECK @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@')"
        print"('TEMP0             :', f12.4)", md_condition%temp0 / md_condition_ref%temp0
        print"('NUMBER_DENSITY    :', f12.4)", md_condition%number_density() / md_condition_ref%number_density()
        print"('PACKING_FRACTION  :', f12.4)", md_condition%packing_fraction() / md_condition_ref%packing_fraction()
        print"('MOLECULAR_MASS    :', f12.4)", md_condition%tmass() / md_condition_ref%tmass()
        print"('MOMENT_OF_INERTIA :', f12.4)", md_condition%moment_of_inertia() / md_condition_ref%moment_of_inertia()
        print"('EPSILON           :', f12.4)", md_condition%eps(1) / md_condition_ref%eps(1)
        print"('ELONGATION        :', f12.4)", md_condition%elongation() / md_condition_ref%elongation()
        print"('==========================================================================')"
        print"('SIGMA             :', f12.4)", md_condition%sigma(1) / md_condition_ref%sigma(1)
        print"('BOND_LENGTH       :', f12.4)", md_condition%bond_length / md_condition_ref%bond_length
        print"('EDGE_MASS         :', f12.4)", md_condition%atomic_mass(1) / md_condition_ref%atomic_mass(1)
        print"('MOLECULAR_VOLUME  :', f12.4)", md_condition%molecular_volume() / md_condition_ref%molecular_volume()
        print"('SYSTEM_VOLUME     :', f12.4)", md_condition%system_volume() / md_condition_ref%system_volume()
        print"('SYSTEM_CELL_SIZE  :', f12.4)", md_condition%system_cell() / md_condition_ref%system_cell()
        print"('CUT_OFF           :', f12.4)", md_condition%rc / md_condition_ref%rc
        print"('@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@')"
    end subroutine


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


    real(real64) function calc_molecular_volume(md_condition)
        class(md_condition_class),intent(in):: md_condition
        real(real64):: sigma ,bond_length
        
        sigma = md_condition%sigma(1)
        bond_length = md_condition%bond_length
        calc_molecular_volume = pi/12d0 * (2*sigma**3 + 3*sigma**2*bond_length - bond_length**3) 
    end function

    real(real64) function calc_number_density(md_condition)
        class(md_condition_class),intent(in):: md_condition

        calc_number_density = dble(md_condition%np) / md_condition%system_volume()
    end function


    real(real64) function calc_packing_fraction(md_condition)
        class(md_condition_class),intent(in):: md_condition

        calc_packing_fraction = md_condition%number_density() * md_condition%molecular_volume()
    end function

    real(real64) function calc_moment_of_inertia(md_condition)
        class(md_condition_class),intent(in):: md_condition

        calc_moment_of_inertia = 2d0 * md_condition%atomic_mass(1) * (md_condition%bond_length/2d0)**2
    end function

    real(real64) function calc_elongation(md_condition)
        class(md_condition_class),intent(in):: md_condition        

        calc_elongation = md_condition%bond_length / md_condition%sigma(1)
    end function
end module



module md_condition_for_ana_mod
    use md_condition_mod
contains 
    subroutine load_condition_for_temp_ana(ndata)
        integer(int32),intent(out):: ndata
        type(md_condition_class):: md_condition

        md_condition = md_condition_class()
        ndata = md_condition%nstep / md_condition%inte
    end subroutine


    subroutine load_condition_for_gr_ana(ndata, cell)
        integer(int32),intent(out):: ndata
        real(real64),intent(out):: cell
        type(md_condition_class):: md_condition

        md_condition = md_condition_class() 
        
        ndata = md_condition%nstep / md_condition%intr
        cell = md_condition%system_cell()
    end subroutine


    subroutine load_condition_for_ext_gr_ana(ndata, cell)
        integer(int32),intent(out):: ndata
        real(real64),intent(out):: cell
        type(md_condition_class):: md_condition

        md_condition = md_condition_class()
        
        ndata = md_condition%nstep / md_condition%intr
        cell = md_condition%system_cell()
    end subroutine


    subroutine load_condition_for_molecular_orientation_ana(ndata, cell)
        integer(int32),intent(out):: ndata
        real(real64),intent(out):: cell
        type(md_condition_class):: md_condition

        md_condition = md_condition_class()
        
        ndata = md_condition%nstep / md_condition%intr
        cell = md_condition%system_cell()
    end subroutine


    subroutine load_condition_for_GK_thcd_ana(ndata, dt, vol)
        integer(int32),intent(out):: ndata
        real(real64),intent(out):: dt, vol
        type(md_condition_class):: md_condition

        md_condition = md_condition_class()

        ndata = md_condition%nstep
        dt = md_condition%dt
        vol = md_condition%system_volume()
    end subroutine


    subroutine load_condition_for_thcd_ana(ndata)
        integer(int32),intent(out):: ndata
        type(md_condition_class):: md_condition

        md_condition = md_condition_class()

        ndata = md_condition%nstep
    end subroutine


    subroutine load_condition_for_viscousity_ana(ndata)
        integer(int32),intent(out):: ndata
        type(md_condition_class):: md_condition

        md_condition = md_condition_class()

        ndata = md_condition%nstep
    end subroutine


    subroutine load_condition_for_GK_viscousity_ana(ndata, dt, vol)
        integer(int32),intent(out):: ndata
        real(real64),intent(out):: dt, vol
        type(md_condition_class):: md_condition

        md_condition = md_condition_class()

        ndata = md_condition%nstep
        dt = md_condition%dt
        vol = md_condition%system_volume()
    end subroutine


    subroutine load_condition_for_msd_ana(ndata, dt, intd, temp0, cell)
        integer(int32),intent(out):: ndata, intd
        real(real64),intent(out):: dt, temp0, cell
        type(md_condition_class):: md_condition

        md_condition = md_condition_class()

        ndata = md_condition%nstep / md_condition%intd
        dt = md_condition%dt
        intd = md_condition%intd
        temp0 = md_condition%temp0
        cell = md_condition%system_cell()
    end subroutine


    subroutine load_condition_for_rmsd_ana(ndata, dt, intd, temp0)
        integer(int32),intent(out):: ndata, intd
        real(real64),intent(out):: dt, temp0
        type(md_condition_class):: md_condition

        md_condition = md_condition_class()

        ndata = md_condition%nstep / md_condition%intd
        dt = md_condition%dt
        intd = md_condition%intd
        temp0 = md_condition%temp0
    end subroutine

    subroutine load_condition_for_transpose_rxyz(ndata, np)
        integer(int32),intent(out):: ndata, np
        type(md_condition_class):: md_condition

        md_condition = md_condition_class()
        ndata = md_condition%nstep / md_condition%intd
        np = md_condition%np
    end subroutine
end module
