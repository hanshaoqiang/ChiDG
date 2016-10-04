module fcn_gaussian
#include <messenger.h>
    use mod_kinds,      only: rk,ik
    use mod_constants,  only: ZERO, ONE, TWO, THREE, FIVE
    use type_point,     only: point_t
    use type_function,  only: function_t
    implicit none





    !>  Gaussian function.
    !!
    !!  Three 1D Gaussian functions are computed; one for each dimension. They are multiplied
    !!  together to create a 3D version of the function.
    !!
    !!  \f$  f_x(t,\vec{x}) = a e^{- \frac{(x-b_x)^2}{2c^2} }    \\
    !!       f_y(t,\vec{x}) = a e^{- \frac{(y-b_y)^2}{2c^2} }    \\
    !!       f_z(t,\vec{x}) = a e^{- \frac{(z-b_z)^2}{2c^2} }    \\
    !!       f(t,\vec{x}) = f_x * f_y * f_z                      \f$
    !!
    !!  Function parameters:
    !!
    !!  \f$ a   -   \text{Amplitude of the distribution}   \\
    !!      b_i -   \text{Offset in coordinate 'i'}        \\
    !!      c   -   \text{Width of the distribution}   \f$
    !!
    !!  @author Nathan A. Wukie
    !!  @date   2/1/2016
    !!
    !-------------------------------------------------------------------------------------
    type, extends(function_t), public :: gaussian_f


    contains

        procedure   :: init
        procedure   :: compute

    end type gaussian_f
    !*************************************************************************************



contains



    !>
    !!
    !!  @author Nathan A. Wukie
    !!  @date   2/2/2016
    !!
    !-------------------------------------------------------------------------
    subroutine init(self)
        class(gaussian_f),  intent(inout)   :: self

        !
        ! Set function name
        !
        call self%add_name("gaussian")


        !
        ! Set function options to default settings
        !
        call self%add_option('a',1._rk)
        call self%add_option('b_x',0._rk)
        call self%add_option('b_y',0._rk)
        call self%add_option('b_z',0._rk)
        call self%add_option('c',1._rk)


    end subroutine init
    !*************************************************************************







    !>
    !!
    !!  @author Nathan A. Wukie
    !!  @date   2/2/2016
    !!
    !----------------------------------------------------------------------------------
    impure elemental function compute(self,time,coord) result(val)
        class(gaussian_f),  intent(inout)  :: self
        real(rk),           intent(in)  :: time
        type(point_t),      intent(in)  :: coord

        real(rk)                        :: val

        real(rk)    :: x,   y,   z, &
                       a,   b_x, b_y, b_z, c, &
                       v_x, v_y, v_z

        !
        ! Get inputs and function parameters
        !
        x = coord%c1_
        y = coord%c2_
        z = coord%c3_

        a   = self%get_option_value('a')
        b_x = self%get_option_value('b_x')
        b_y = self%get_option_value('b_y')
        b_z = self%get_option_value('b_z')
        c   = self%get_option_value('c')


        !
        ! Compute function
        !
        v_x = a * exp( - ((x - b_x)**TWO) / (TWO * c**TWO))
        v_y = a * exp( - ((y - b_y)**TWO) / (TWO * c**TWO))
        v_z = a * exp( - ((z - b_z)**TWO) / (TWO * c**TWO))


        !val = v_x * v_y * v_z
        val = v_x

    end function compute
    !***********************************************************************************


end module fcn_gaussian
