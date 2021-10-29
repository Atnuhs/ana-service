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
ADDEXE(gr read_condition_mod representative_value_mod)
ADDEXE(molecular_orientation read_condition_mod representative_value_mod)
ADDEXE(msd read_condition_mod representative_value_mod)
ADDEXE(rmsd read_condition_mod representative_value_mod)
ADDEXE(thcd read_condition_mod representative_value_mod)
ADDEXE(GK_thcd read_condition_mod representative_value_mod)
ADDEXE(GK_viscousity read_condition_mod representative_value_mod)
ADDEXE(viscousity read_condition_mod representative_value_mod)

