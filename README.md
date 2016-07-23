
<p align="center">
    <img src=/doc/figures/chidg_logo_small.png?raw=true />
</p>

# ChiDG

[![][license img]][license]

A Chimera-based, discontinuous Galerkin solver


Overset airfoil grid                              |  NACA 2412 pressure
:------------------------------------------------:|:------------------------------------------------------:
![](doc/figures/naca2412_A4p0_straight_grid.png)  |     ![](doc/figures/naca2412_M0p2_A4p0_cpcontour_P3.png)



4th-order duct grid                               |  Acoustic duct mode radiation. 7th-order accuracy.
:------------------------------------------------:|:------------------------------------------------------:
![](doc/figures/munt_duct_grid.png)               |     ![](doc/figures/mode91_3dview.png)  



Constant pressure outlet boundary condition       |  Fully-implicit nonreflecting outlet boundary condition
:------------------------------------------------:|:-------------------------------------------------------:
<img src="doc/figures/aachen_turbine_reflectingbc.png" hspace="118pt"/> |   <img src="doc/figures/aachen_turbine_nonreflectingbc.png" hspace="118pt"/>





## Documentation

Documentation can be found on the following github page:

[ChiDG Documentation](https://nwukie.github.io/ChiDG_site/ )











## Installation

<B> Up-to-date Fortran(F2008) compiler required. Tested with gfortran 5.3 </B>


ChiDG uses the CMake build system. Out-of-source builds, as described here, are the only supported build method.
- Change to the ChiDG root directory.   
        `cd ChiDG/`

- Create a new build directory.         
        `mkdir build`

- Change to the build directory.        
        `cd build`

- Execute the CMake command with options to configure the build.         
        `cmake -DCMAKE_Fortran_COMPILER=gfortran -DCMAKE_BUILD_TYPE=Release ..`

- Run make to build the ChiDG library and executable.        
        `make`






## Dependencies

HDF5: ChiDG-formatted IO  
CMake: Build system  
TecIO: Visualization IO  





## License
ChiDG is released under the BSD 3-clause license. See LICENSE file.



## Author Acknowledgement:
Nathan A. Wukie   <nwukie@gmail.com>






## Sponsorship Acknowledgement:
This material is based upon work supported by the National Science Foundation Graduate 
Research Fellowship Program under Grant No. 1610397. Any opinions, findings, and 
conclusions or recommendations expressed in this material are those of the author(s) 
and do not necessarily reflect the views of the National Science Foundation.











[license]:LICENSE
[license img]:https://img.shields.io/badge/license-BSD%203--clause-blue.svg

























