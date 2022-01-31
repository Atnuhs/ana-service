### モジュールライブラリの登録用マクロ
macro(ADDLIB name)

    add_library(${name}_mod ${name}.f90)
    target_link_libraries(${name}_mod ${ARGN})
    set_target_properties(${name}_mod
        PROPERTIES
        Fortran_MODULE_DIRECTORY ${LIB_MOD_DIR}
    )

endmacro(ADDLIB)


### moduleの登録
ADDLIB(io_file)
ADDLIB(const)
ADDLIB(fft)
ADDLIB(numerical_integration)
ADDLIB(md_condition_for_ana const_mod)
ADDLIB(representative_value)
ADDLIB(mean_square_displacement fft_mod)
