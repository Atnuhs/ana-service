### モジュールライブラリの登録用マクロ
macro(ADDLIB name)

    add_library(${name}_mod ${name}.f90)
    target_link_libraries(${name}_mod ${ARGN})
    set_target_properties(${name}_mod
        PROPERTIES
        Fortran_MODULE_DIRECTORY ${LIB_MOD_DIR}
    )

endmacro(ADDLIB)


###　moduleの登録
ADDLIB(read_condition)
ADDLIB(representative_value)
ADDLIB(io_file)
ADDLIB(aggregate_x io_file_mod representative_value_mod)
ADDLIB(aggregate_x_y io_file_mod representative_value_mod)
ADDLIB(aggregate_x_y_z io_file_mod representative_value_mod)
ADDLIB(convergence_green_kubo_formula representative_value_mod)
