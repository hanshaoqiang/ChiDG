module type_chidg_worker
    use mod_kinds,          only: rk, ik
    use type_mesh,          only: mesh_t
    use type_solverdata,    only: solverdata_t
!    use type_element_info,  only: element_info_t
    use type_face_info,     only: face_info_t
    use type_function_info, only: function_info_t

    use mod_interpolate,    only: interpolate_face_autodiff
    use mod_integrate,      only: integrate_boundary_scalar_flux
    use DNAD_D
    implicit none



    !>
    !!
    !!  @author Nathan A. Wukie
    !!  @date   8/20/2016
    !!
    !----------------------------------------------------------------------------------
    type, public :: chidg_worker_t

        type(mesh_t),           pointer :: mesh(:)
        type(solverdata_t),     pointer :: solverdata

!        type(element_info_t)    :: elem_info
        type(face_info_t)       :: face_info
        type(function_info_t)   :: fcn_info

    contains

        procedure   :: init

!        procedure   :: initialize
        procedure   :: interpolate
        procedure   :: integrate
!    
!        procedure   :: x
!        procedure   :: y
!        procedure   :: z
!   
         procedure  :: normal
         procedure  :: unit_normal
!
!        procedure   :: volume
!        procedure   :: area
!
!        procedure   :: mass
!        procedure   :: invmass

        final   :: destructor

    end type chidg_worker_t
    !**********************************************************************************






contains





    !>
    !!
    !!  @author Nathan A. Wukie
    !!  @date   8/20/2016
    !!
    !!
    !----------------------------------------------------------------------------------------
    subroutine init(self,mesh,solverdata)
        class(chidg_worker_t),      intent(inout)   :: self
        type(mesh_t),       target, intent(in)      :: mesh(:)
        type(solverdata_t), target, intent(in)      :: solverdata


        self%mesh       => mesh
        self%solverdata => solverdata


    end subroutine init
    !*****************************************************************************************







    !>
    !!
    !!
    !!
    !!
    !!
    !-----------------------------------------------------------------------------------------
    function interpolate(self,ieqn,source) result(val_gq)
        class(chidg_worker_t),  intent(in)  :: self
        integer(ik),            intent(in)  :: ieqn
        integer(ik),            intent(in)  :: source

        integer(ik)             :: idom, ielem, iface, nnodes
        type(AD_D), allocatable :: val_gq(:)


        idom  = self%face_info%idomain
        ielem = self%face_info%ielement
        iface = self%face_info%iface

        nnodes = self%mesh(idom)%faces(ielem,iface)%gq%face%nnodes

        allocate(val_gq(nnodes))


        call interpolate_face_autodiff(self%mesh, self%face_info, self%solverdata%q, ieqn, val_gq, source)


    end function interpolate
    !******************************************************************************************







    !>
    !!
    !!  @author Nathan A. Wukie
    !!  @date   8/20/2016
    !!
    !!
    !-------------------------------------------------------------------------------------------
    subroutine integrate(self,ieqn,integrand)
        class(chidg_worker_t),  intent(inout)   :: self
        integer(ik),            intent(in)      :: ieqn
        type(AD_D),             intent(inout)   :: integrand(:)





        call integrate_boundary_scalar_flux(self%mesh,self%solverdata,self%face_info,self%fcn_info,ieqn,integrand)




    end subroutine integrate
    !*********************************************************************************************









    !>
    !!
    !!
    !!
    !!
    !------------------------------------------------------------------------------------------
    function normal(self,coord) result(vals)
        class(chidg_worker_t),  intent(in)  :: self
        integer(ik),            intent(in)  :: coord

        integer(ik)             :: idom, ielem, iface
        real(rk), allocatable   :: vals(:)

        idom  = self%face_info%idomain
        ielem = self%face_info%ielement
        iface = self%face_info%iface


        vals = self%mesh(idom)%faces(ielem,iface)%norm(:,coord)


    end function normal
    !******************************************************************************************









    !>
    !!
    !!
    !!
    !!
    !------------------------------------------------------------------------------------------
    function unit_normal(self,coord) result(vals)
        class(chidg_worker_t),  intent(in)  :: self
        integer(ik),            intent(in)  :: coord

        integer(ik)             :: idom, ielem, iface
        real(rk), allocatable   :: vals(:)

        idom  = self%face_info%idomain
        ielem = self%face_info%ielement
        iface = self%face_info%iface


        vals = self%mesh(idom)%faces(ielem,iface)%unorm(:,coord)


    end function unit_normal
    !******************************************************************************************







    !>
    !!
    !!
    !!
    !-----------------------------------------------------------------------------------------
    subroutine destructor(self)
        type(chidg_worker_t),   intent(inout)   :: self

        if (associated(self%mesh))       nullify(self%mesh)
        if (associated(self%solverdata)) nullify(self%solverdata)

    end subroutine destructor
    !******************************************************************************************



end module type_chidg_worker
