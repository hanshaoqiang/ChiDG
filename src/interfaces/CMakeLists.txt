add_subdirectory(graphics)

# Add driver for executable interface
add_exe_sources(driver.f90)    


## Add wrappers for python interface
#add_python_sources(
#                   f90wrap_type_chidg_python.f90
#                   f90wrap_type_dict_python.f90
#                   f90wrap_type_chidgvis_bc_python.f90
#                   )
    

add_lib_sources()
