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
ADDEXE(gr                    aggregate_xy_data_mod)
ADDEXE(ext_gr                aggregate_xy_data_mod)
ADDEXE(molecular_orientation aggregate_xyz_data_mod)
ADDEXE(msd  read_condition_mod aggregate_xy_ysd_data_mod aggregate_y_data_mod)
ADDEXE(rmsd read_condition_mod aggregate_xy_ysd_data_mod aggregate_y_data_mod)
ADDEXE(GK_thcd       read_condition_mod aggregate_xy_data_mod)
ADDEXE(GK_viscousity read_condition_mod aggregate_xy_data_mod)
ADDEXE(thcd       read_condition_mod aggregate_y_ysd_data_mod convergence_green_kubo_formula_mod)
ADDEXE(viscousity read_condition_mod aggregate_y_ysd_data_mod convergence_green_kubo_formula_mod)

