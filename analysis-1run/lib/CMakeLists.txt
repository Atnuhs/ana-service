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
ADDLIB(fft)
ADDLIB(read_condition)
ADDLIB(representative_value)
ADDLIB(mean_square_displacement fft_mod)
ADDLIB(numerical_integration)
ADDLIB(md_condition_for_ana)
ADDLIB(convergence_green_kubo_formula representative_value_mod)
