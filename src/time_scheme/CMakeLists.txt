#add_subdirectory(forward_euler)                
#add_subdirectory(backward_euler)


add_lib_sources(
                type_time_scheme.f90
                mod_time_scheme.f90
                type_timer.f90
                steady.f90
                backward_euler.f90
                forward_euler.f90
                )

                
