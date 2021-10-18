macro(ADDEXE name)

    ### テスト用プログラムのビルド設定
    add_executable(${name}.out ${name}.f90)

    ### 参照するmodファイルのディレクトリとリンクするライブラリの設定
    target_link_libraries(${name}.out ${ARGN})
    set_target_properties(${name}.out
        PROPERTIES
        Fortran_MODULE_DIRECTORY ${LIB_MOD_DIR}
    )


endmacro(ADDEXE)

### テストディレクトリの追加
ADDEXE(gr                    md_condition_for_ana_mod)
ADDEXE(molecular_orientation md_condition_for_ana_mod)
ADDEXE(temp                  md_condition_for_ana_mod representative_value_mod)
ADDEXE(msd                   md_condition_for_ana_mod fft_mod mean_square_displacement_mod representative_value_mod)
ADDEXE(rmsd                  md_condition_for_ana_mod fft_mod mean_square_displacement_mod representative_value_mod)
ADDEXE(GK_thcd               md_condition_for_ana_mod fft_mod numerical_integration_mod)
ADDEXE(thcd                  md_condition_for_ana_mod)
ADDEXE(GK_viscousity            md_condition_for_ana_mod fft_mod numerical_integration_mod)
ADDEXE(viscousity            md_condition_for_ana_mod fft_mod numerical_integration_mod)

