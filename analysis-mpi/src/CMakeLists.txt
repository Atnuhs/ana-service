macro(ADDEXE name)

    ### テスト用プログラムのビルド設定
    add_executable(${name}.out ${name}.f90)

    ### 参照するmodファイルのディレクトリとリンクするライブラリの設定
    target_link_libraries(${name}.out 
    ${MPI_Fortran_LIBRARIES}
    ${ARGN}
    )
    set_target_properties(${name}.out
        PROPERTIES
        Fortran_MODULE_DIRECTORY ${LIB_MOD_DIR}
    )


endmacro(ADDEXE)

### テストディレクトリの追加
ADDEXE(gr io_file_mod md_condition_for_ana_mod const_mod)
ADDEXE(ext_gr io_file_mod md_condition_for_ana_mod const_mod)
ADDEXE(molecular_orientation io_file_mod md_condition_for_ana_mod const_mod)
ADDEXE(temp io_file_mod md_condition_for_ana_mod)
ADDEXE(GK_thcd io_file_mod md_condition_for_ana_mod const_mod fft_mod numerical_integration_mod)
ADDEXE(GK_viscousity io_file_mod md_condition_for_ana_mod const_mod fft_mod numerical_integration_mod)
ADDEXE(thcd io_file_mod md_condition_for_ana_mod const_mod representative_value_mod)
ADDEXE(viscousity io_file_mod md_condition_for_ana_mod const_mod representative_value_mod)
ADDEXE(msd io_file_mod md_condition_for_ana_mod const_mod representative_value_mod mean_square_displacement_mod)
ADDEXE(rmsd io_file_mod md_condition_for_ana_mod const_mod representative_value_mod mean_square_displacement_mod)
