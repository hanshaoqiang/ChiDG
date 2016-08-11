project(chidg_v0.1 Fortran)
cmake_minimum_required(VERSION 3.4) 




#-----------------------------------------------------------------------------------
#                       CMake macros for adding source files
#-----------------------------------------------------------------------------------
include(cmake/macros/add_lib_sources.txt)
include(cmake/macros/add_exe_sources.txt)
include(cmake/macros/add_python_sources.txt)
include(cmake/macros/add_test_target.txt)
include(cmake/macros/add_test_sources.txt)
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/modules/")

# Set output directories
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/lib)



#-----------------------------------------------------------------------------------
#                       Enable testing with -DTESTING=True
#-----------------------------------------------------------------------------------
set(TESTING FALSE CACHE BOOL "Enable building unit/regression testing")





#-----------------------------------------------------------------------------------
#                           Get fortran compiler vendor
#-----------------------------------------------------------------------------------
get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)
if (Fortran_COMPILER_NAME MATCHES "ifort.*")
    set (Fortran_VENDOR "INTEL")
elseif (Fortran_COMPILER_NAME MATCHES "gfortran.*")
    set (Fortran_VENDOR "GNU")
else ()
    message(SEND_ERROR "Fortran compiler not recognized")
endif()




#-----------------------------------------------------------------------------------
#                               Set compiler flags
#----------------------------------------------------------------------------------- 
if (${Fortran_VENDOR} MATCHES "INTEL")
#    set (USER_COMPILE_FLAGS_DEBUG   "-O0 -fpp -assume realloc_lhs -debug -p -g -traceback -check all -check noarg_temp_created -check bounds -fp-stack-check")
    set (USER_COMPILE_FLAGS_DEBUG   "-O0 -fpp -assume realloc_lhs -debug -p -g -traceback -check all -check bounds -fp-stack-check -standard-semantics -assume nostd_mod_proc_name -heap-arrays 1600")
    set (USER_COMPILE_FLAGS_RELEASE "-O3 -fpp -assume realloc_lhs")
    set (CMAKE_Fortran_LINK_FLAGS_DEBUG "")
    set (CMAKE_Fortran_LINK_FLAGS_RELEASE "")

    set (PROFILE_FLAGS "")
    
    #set (CMAKE_AR xiar)        # for -ipo
    #set (CMAKE_LINKER xild)    # for -ipo
    
elseif (${Fortran_VENDOR} MATCHES "GNU")
    # Note: -fcheck=all has issues with passing null pointer in tecio_interface
    set (USER_COMPILE_FLAGS_DEBUG   "-O0 -cpp --coverage -ffree-form -ffree-line-length-none -fbacktrace -fstack-protector -fcheck=bounds -fmax-stack-var-size=1600 -p -g -Wall")    # Note: -fcheck=all has issues with passing null pointer in tecio_interface
    #set (USER_COMPILE_FLAGS_RELEASE "-O3 -cpp -ffree-form -ffree-line-length-none -fexternal-blas -march=native -funroll-loops -m64")
    #set (USER_COMPILE_FLAGS_RELEASE "-O3 -cpp -fPIC -ffree-form -ffree-line-length-none -fexternal-blas -march=native -funroll-loops -m64 -g")
    set (USER_COMPILE_FLAGS_RELEASE "-O3 -cpp -fPIC -ffree-form -ffree-line-length-none -fexternal-blas -funroll-loops -m64 -g")

    set (CMAKE_Fortran_LINK_FLAGS_DEBUG "-O0 --coverage")
    set (CMAKE_Fortran_LINK_FLAGS_RELEASE "-O3")
    
    set (PROFILE_FLAGS "-p -g")


endif()

# Append user flags to debug and release variables
set (CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} ${USER_COMPILE_FLAGS_DEBUG}")
set (CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} ${USER_COMPILE_FLAGS_RELEASE}")

# Append profile flags
if (PROFILE)
    set (CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} ${PROFILE_FLAGS}")
    set (CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} ${PROFILE_FLAGS}")
endif()




#----------------------------------------------------------------------------------- 
#                                  Find HDF5
#----------------------------------------------------------------------------------- 
# NOTE: export HDF5_ROOT path as shell environment variable such that 
#       $HDF5_ROOT contains the bin, lib, include directories for HDF5.
#       The find_package module looks first for $HDF5_ROOT.

set(HDF5_USE_STATIC_LIBRARIES TRUE)
find_package(HDF5 1.10 COMPONENTS Fortran HL Fortran_HL REQUIRED)
#find_package(HDF5 REQUIRED COMPONENTS Fortran HL)
include_directories(${HDF5_INCLUDE_DIRS})

if (HDF5_FOUND)
    message(STATUS "HDF5_LIBRARIES: " ${HDF5_LIBRARIES})
#    message(STATUS "HDF5_HL_LIBRARIES: " ${HDF5_HL_LIBRARIES})
endif()


# Filter just the HDF5 libraries out of the defined variables. We don't want the dynamic libs like libdl or libz etc.
# This is to link in the python interface.
foreach(LIBPATH in LIST ${HDF5_LIBRARIES})
    get_filename_component(LIB ${LIBPATH} NAME)
    if (${LIB} MATCHES "libhdf5.a")
        set(HDF5_INTRINSIC_LIBRARIES ${HDF5_INTRINSIC_LIBRARIES} ${LIBPATH})
    endif()
    if (${LIB} MATCHES "libhdf5_hl.a")
        set(HDF5_INTRINSIC_LIBRARIES ${HDF5_INTRINSIC_LIBRARIES} ${LIBPATH})
    endif()
    if (${LIB} MATCHES "libhdf5_fortran.a")
        set(HDF5_INTRINSIC_LIBRARIES ${HDF5_INTRINSIC_LIBRARIES} ${LIBPATH})
    endif()
    if (${LIB} MATCHES "libhdf5hl_fortran.a")
        set(HDF5_INTRINSIC_LIBRARIES ${HDF5_INTRINSIC_LIBRARIES} ${LIBPATH})
    endif()
endforeach()

message(STATUS "HDF5 Intrinsic Libraries: " ${HDF5_INTRINSIC_LIBRARIES})






#----------------------------------------------------------------------------------- 
#                                 Find Lapack
#----------------------------------------------------------------------------------- 
find_package(LAPACK REQUIRED)

message(STATUS "LAPACK_LIBRARIES: " ${LAPACK_LIBRARIES})
message(STATUS "BLAS_LIBRARIES: " ${BLAS_LIBRARIES})





##----------------------------------------------------------------------------------- 
##                                  Find MPI - STUB
##----------------------------------------------------------------------------------- 
## NOTE: export MPI_ROOT path as shell environment variable such that 
##       $MPI_ROOT/gnu or $MPI_ROOT/intel are valid. Inside the 'gnu' or 'intel'
##       folder one should find lib and include directories.
#
## Select vendor installation
#if (${Fortran_VENDOR} MATCHES "INTEL")
#    set(MPI_VENDOR_ROOT $ENV{MPI_ROOT}/intel)
#elseif (${Fortran_VENDOR} MATCHES "GNU")
#    set(MPI_VENDOR_ROOT $ENV{MPI_ROOT}/gnu)
#endif()
#
#message(STATUS "Fortran_VENDOR: ${Fortran_VENDOR}")
#message(STATUS "MPI_VENDOR_ROOT: ${MPI_VENDOR_ROOT}")
#
#set(MPI_Fortran_COMPILER "${MPI_VENDOR_ROOT}/bin/mpifort")
#set(MPI_Fortran_INCLUDE_PATH "${MPI_VENDOR_ROOT}/include")
#
#
#find_package(MPI REQUIRED)
#
#include_directories(${MPI_Fortran_INCLUDE_PATH})
#








#----------------------------------------------------------------------------------- 
#                               Find Tecplot
#----------------------------------------------------------------------------------- 
# Shouldn't need user-intervention here.
# First define TEC_ROOT path as cmake variable such that
# ${TEC_ROOT}/lib/libtecio.a  ${TEC_ROOT}/include/tecio.f90 are valid.
# ${TEC_ROOT} gets used by the find_package(Tecplot) module.

# Select TecIO library based on detected OS
if ( ${CMAKE_SYSTEM_NAME} MATCHES "Darwin" ) 
    set(TEC_ROOT ${CMAKE_SOURCE_DIR}/tecio/mac)
elseif( ${CMAKE_SYSTEM_NAME} MATCHES "Linux" )
    set(TEC_ROOT ${CMAKE_SOURCE_DIR}/tecio/linux)
else()
    message(SEND_ERROR "Invalid operating system detected for TecIO")
endif()


find_package(Tecplot REQUIRED)
include_directories(${TECPLOT_INCLUDE_DIR})


message(STATUS "TECPLOT_LIBRARIES: " ${TECPLOT_LIBRARIES})
message(STATUS "TECPLOT_INCLUDE_DIR: " ${TECPLOT_INCLUDE_DIR})




#----------------------------------------------------------------------------------- 
#                        Add source tree and includes
#----------------------------------------------------------------------------------- 
add_subdirectory(DNAD)                                                                        
add_subdirectory(src)

include_directories(${CMAKE_SOURCE_DIR}/src/tools)    # Need include here for macro-enhanced message handler messenger.h







#----------------------------------------------------------------------------------- 
#                                   Add core library
#----------------------------------------------------------------------------------- 
add_library(coredg STATIC ${LIB_SRCS})
target_link_libraries(coredg ${HDF5_LIBRARIES} ${LAPACK_LIBRARIES} ${BLAS_LIBRARIES} ${TECPLOT_LIBRARIES})
set_target_properties(coredg PROPERTIES 
                             COMPILE_FLAGS "${CMAKE_Fortran_FLAGS}"
                             LINK_FLAGS    "${CMAKE_Fortran_LINK_FLAGS}")
                             #COMPILE_FLAGS "${CMAKE_Fortran_FLAGS} ${MPI_Fortran_COMPILE_FLAGS}"
                             #LINK_FLAGS    "${CMAKE_Fortran_LINK_FLAGS} ${MPI_Fortran_LINK_FLAGS}")






#-----------------------------------------------------------------------------------
#                                Build ChiDG interfaces
#-----------------------------------------------------------------------------------

#
# Build Fortran driver executable linked to coredg library
#
add_executable(chidg ${EXE_SRCS})
target_link_libraries(chidg coredg)
set_target_properties(chidg PROPERTIES 
                            COMPILE_FLAGS "${CMAKE_Fortran_FLAGS}"
                            LINK_FLAGS    "${CMAKE_Fortran_LINK_FLAGS} -lc++ -lpthread")
                            #LINK_FLAGS    "${CMAKE_Fortran_LINK_FLAGS} -lstdc++ -lpthread")
                            #COMPILE_FLAGS "${CMAKE_Fortran_FLAGS} ${MPI_Fortran_COMPILE_FLAGS}"
                            #LINK_FLAGS    "${CMAKE_Fortran_LINK_FLAGS} ${MPI_Fortran_LINK_FLAGS} -lstdc++ -lpthread")
                            # need to link stdc++ here for tecio


#
# Build python module
#
if (CMAKE_BUILD_TYPE STREQUAL "Release")
    set(QUIET_STATUS --quiet)
endif()

add_custom_command(OUTPUT "_chidg.so"
                   COMMAND f2py 
                           -c ${PYTHON_SRCS}
                           -m _chidg
                           --build-dir "${CMAKE_BINARY_DIR}/lib"
                           ${CMAKE_BINARY_DIR}/lib/libcoredg.a
                           ${TECPLOT_LIBRARIES}
                           ${HDF5_INTRINSIC_LIBRARIES}
                           --link-lapack_opt
                           -I${CMAKE_BINARY_DIR}/lib
                           ${QUIET_STATUS}
                   DEPENDS ${PYTHON_SRCS} coredg
                   WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/lib
                   COMMENT "f2py: Building Python module _chidg")


# Add a custom target <name> to trigger the generation of the python module.
add_custom_target(python ALL DEPENDS "_chidg.so")

# Install the python module
install(FILES "${CMAKE_BINARY_DIR}/lib/_chidg.so" "${CMAKE_SOURCE_DIR}/src/interfaces/chidg.py"
        DESTINATION /usr/local/chidg/lib)






#-----------------------------------------------------------------------------------
#       Install targets
#----------------------------------------------------------------------------------- 
install(TARGETS chidg coredg
        RUNTIME DESTINATION /usr/local/chidg/bin
        LIBRARY DESTINATION /usr/local/chidg/lib
        ARCHIVE DESTINATION /usr/local/chidg/lib)














#----------------------------------------------------------------------------------- 
#                   Testing:   enable with -DTESTING=True
#----------------------------------------------------------------------------------- 
# To build tests, the unit-testing framework pFUnit must be compiled with MPI support.
# The environment variable PFUNIT must be set such that PFUNIT/gnu or PFUNIT/intel
# are valid.

if (TESTING)

    enable_testing()

    #
    # Set pFUnit installation based on compiler vendor
    #
    #if (${Fortran_VENDOR} MATCHES "INTEL")
    #    set(PF_ROOT $ENV{PFUNIT}/intel)
    #elseif (${Fortran_VENDOR} MATCHES "GNU")
    #    set(PF_ROOT $ENV{PFUNIT}/gnu)
    #endif()

    set(PF_ROOT $ENV{PFUNIT})

    #
    # Set location of pFUnit libs
    #
    set(PFU_LIB ${PF_ROOT}/lib)


    #
    # Add unit tests
    #
    add_subdirectory(test)


endif()






#------------------------------------------------------------------
#                    Generate documentation
#------------------------------------------------------------------





# This turns off the deprecated $HDF5_DIR variable that shows up sometimes when running ccmake
# It probably depends on what version of FindHDF5.cmake is being used, but the correct variable
# to set is HDF5_ROOT
unset(HDF5_DIR CACHE)









