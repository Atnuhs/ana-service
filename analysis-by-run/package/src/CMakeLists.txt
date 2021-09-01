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
ADDEXE(gr read_condition_mod)
ADDEXE(temp read_condition_mod representative_value_mod)
ADDEXE(msd_ref read_condition_mod mean_square_displacement_mod fft_mod representative_value_mod)
ADDEXE(integ_acf_ej read_condition_mod fft_mod)
ADDEXE(vis_ref read_condition_mod fft_mod)

