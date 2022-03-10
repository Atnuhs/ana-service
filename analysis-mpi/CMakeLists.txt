### プロジェクト全体設定
cmake_minimum_required(VERSION 2.8)

project(analysis_md_program Fortran)
# project名の設定

enable_language(Fortran)

find_package(MPI REQUIRED COMPONENTS Fortran)
add_definitions(${MPI_Fortran_COMPILE_FLAGS})
include_directories(${MPI_Fortran_INCLUDE_PATH})
link_directories(${MPI_Fortran_LIBRARIES})
### モジュールファイルの出力ディレクトリパスの設定
set(LIB_MOD_DIR ${CMAKE_CURRENT_BINARY_DIR}/mod_files/)

# ifort
if(CMAKE_Fortran_COMPILER_ID MATCHES Intel)
    set(CMAKE_Fortran_FLAGS         "-O3 -xHost")
    set(CMAKE_Fortran_FLAGS_DEBUG   "-O0 -warn all -check all -std -gen_interfaces -fpe0 -ftrapuv -traceback")
    set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -march=native")
endif()
#gfortran
if(CMAKE_Fortran_COMPILER_ID MATCHES GNU)
    set(CMAKE_Fortran_FLAGS         "-O3")
    set(CMAKE_Fortran_FLAGS_DEBUG   "-Wall -pedantic -fbounds-check -O -ffpe-trap=invalid,zero,overflow -fbacktrace")
    set(CMAKE_Fortran_FLAGS_PROF   "-O3 -pg")
    set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -march=native")
endif()
add_subdirectory(src)
add_subdirectory(lib)

