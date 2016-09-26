module type_gather_graphics_bcs
#include <messenger.h>
    use mod_kinds,          only: ik, rk
    use type_bc,            only: bc_t
    use type_graphics_bc,   only: graphics_bc_t
    use type_chidg,         only: chidg_t
    implicit none



    !>  A visitor that gathers bc_t's from chidg and processes an array of graphics_bc_t's 
    !!  that are extensions of bc_t's that contain graphics primitive data.
    !!
    !!  @author Nathan A. Wukie
    !!  @date   9/4/2016
    !!
    !---------------------------------------------------------------------------------------
    type, public :: gather_graphics_bcs_v

        type(graphics_bc_t), allocatable :: graphics_bc(:)

    contains

        procedure   :: pull
        procedure   :: push

        procedure   :: nbcs
        procedure   :: get_bc

        procedure, private :: add_graphics_bc

    end type gather_graphics_bcs_v
    !***************************************************************************************



contains




    !>  Processes all the bc_t instances in chidg into gui_bc_t that contains graphics
    !!  information
    !!
    !!  @author Nathan A. Wukie
    !!  @date   9/4/2016
    !!
    !!
    !---------------------------------------------------------------------------------------
    subroutine pull(self,chidg)
        class(gather_graphics_bcs_v),   intent(inout)   :: self
        type(chidg_t),                  intent(inout)   :: chidg

        integer(ik)         :: idom, ibc, ierr
        type(graphics_bc_t) :: graphics_bc



        !
        ! Loop through boundary conditions. For each, allocated a graphics_bc, process it, and add to the list
        !
        do idom = 1,chidg%data%ndomains()
            if (allocated(chidg%data%bcset(idom)%bcs)) then
                do ibc = 1,size(chidg%data%bcset(idom)%bcs)

                    if (allocated(chidg%data%bcset(idom)%bcs(ibc)%bc)) then
                        ! Copy data from the bc_t to the graphics_bc_t
                        graphics_bc%name          = chidg%data%bcset(idom)%bcs(ibc)%bc%name
                        graphics_bc%isInitialized = chidg%data%bcset(idom)%bcs(ibc)%bc%isInitialized
                        graphics_bc%dom           = chidg%data%bcset(idom)%bcs(ibc)%bc%dom
                        graphics_bc%elems         = chidg%data%bcset(idom)%bcs(ibc)%bc%elems
                        graphics_bc%faces         = chidg%data%bcset(idom)%bcs(ibc)%bc%faces
                        graphics_bc%bcproperties  = chidg%data%bcset(idom)%bcs(ibc)%bc%bcproperties

                        ! Process the graphics_bc 
                        call graphics_bc%graphics_process_faces(chidg%data%mesh)

                        ! Add to the graphics_bc array
                        call self%add_graphics_bc(graphics_bc)
                    end if

                end do
            end if
        end do
        





    end subroutine pull
    !***************************************************************************************










    !>  Processes all the bc_t instances in chidg into gui_bc_t that contains graphics
    !!  information
    !!
    !!  @author Nathan A. Wukie
    !!  @date   9/5/2016
    !!
    !!
    !---------------------------------------------------------------------------------------
    subroutine push(self,chidg)
        class(gather_graphics_bcs_v),   intent(inout)   :: self
        type(chidg_t),                  intent(inout)   :: chidg







    end subroutine push
    !***************************************************************************************







    !>  Add a graphics_bc to the sotrage
    !!
    !!  @author Nathan A. Wukie
    !!  @date   9/4/2016
    !!
    !!
    !----------------------------------------------------------------------------------------
    subroutine add_graphics_bc(self,graphics_bc)
        class(gather_graphics_bcs_v),   intent(inout)   :: self
        class(graphics_bc_t),           intent(in)      :: graphics_bc

        integer(ik) :: nbcs_before, nbcs_after, ierr
        type(graphics_bc_t),    allocatable :: temp_bcs(:)


        nbcs_before = self%nbcs()
        nbcs_after  = nbcs_before + 1

        !
        ! Allocate temporary vector
        !
        allocate(temp_bcs(nbcs_after), stat=ierr)
        if (ierr /= 0) call AllocationError


        !
        ! Copy previously allocated graphics_bcs to temp
        !
        temp_bcs(1:nbcs_before) = self%graphics_bc(1:nbcs_before)


        !
        ! Add the new graphics_bc to the end
        !
        temp_bcs(nbcs_after) = graphics_bc
!        allocate(temp_bcs(nbcs_after)%bc, source=graphics_bc, stat=ierr)
!        if (ierr /= 0) call AllocationError


        !
        ! Move allocation to self
        !
        call move_alloc(temp_bcs,self%graphics_bc)


    end subroutine add_graphics_bc
    !****************************************************************************************





    !>  Return the number of graphics boundary conditions currently stored
    !!
    !!  @author Nathan A. Wukie
    !!  @date   9/4/2016
    !!
    !!
    !----------------------------------------------------------------------------------------
    function nbcs(self) result(nbcs_)
        class(gather_graphics_bcs_v),   intent(in)  :: self

        integer(ik) :: nbcs_

        if (allocated(self%graphics_bc)) then
            nbcs_ = size(self%graphics_bc)
        else
            nbcs_ = 0
        end if

    end function nbcs
    !*****************************************************************************************








    !>
    !!
    !!  @author Nathan A. Wukie
    !!  @date   9/4/2016
    !!
    !!
    !-----------------------------------------------------------------------------------------
    function get_bc(self,ibc) result(bc)
        class(gather_graphics_bcs_v),   intent(in)  :: self
        integer(ik),                    intent(in)  :: ibc

        type(graphics_bc_t) :: bc
        integer(ik)         :: ierr

        !allocate(bc, source=self%graphics_bc(ibc)%bc, stat=ierr)
        !if (ierr /= 0) call AllocationError
        bc = self%graphics_bc(ibc)

    end function get_bc
    !*****************************************************************************************













end module type_gather_graphics_bcs
