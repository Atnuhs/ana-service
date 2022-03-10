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
ADDEXE(gr                    aggregate_x_y_mod)
ADDEXE(ext_gr                aggregate_x_y_mod)
ADDEXE(3dgr_peak)
ADDEXE(molecular_orientation aggregate_x_y_z_mod)
ADDEXE(msd  read_condition_mod aggregate_x_y_mod aggregate_x_mod)
ADDEXE(rmsd read_condition_mod aggregate_x_y_mod aggregate_x_mod)
ADDEXE(GK_thcd       read_condition_mod aggregate_x_y_mod)
ADDEXE(GK_viscousity read_condition_mod aggregate_x_y_mod)
ADDEXE(thcd       read_condition_mod aggregate_x_mod convergence_green_kubo_formula_mod)
ADDEXE(viscousity read_condition_mod aggregate_x_mod convergence_green_kubo_formula_mod)

