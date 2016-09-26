module type_graphics_bc
#include <messenger.h>
    use mod_kinds,      only: ik, rk
    use mod_constants,  only: XI_MIN, XI_MAX, ETA_MIN, ETA_MAX, ZETA_MIN, ZETA_MAX, ONE
    use type_mesh,      only: mesh_t
    use type_bc,        only: bc_t
    implicit none




    !>  Extends a boundary condition to include graphics information.
    !!
    !!  @author Nathan A. Wukie
    !!  @date   9/4/2016
    !!
    !------------------------------------------------------------------------------------------------
    type, extends(bc_t), public :: graphics_bc_t

        real(rk),       allocatable :: graphics_points(:,:)  ! Cartesian (npts, ndim=3)
        integer(ik),    allocatable :: graphics_faces(:,:)   ! Triangles (ntriangles, npts=3)

    contains

        procedure           :: graphics_process_faces
        procedure, private  :: graphics_add_faces
        
        procedure           :: graphics_npoints
        procedure           :: graphics_nfaces

        procedure           :: graphics_get_points
        procedure           :: graphics_get_faces

    end type graphics_bc_t
    !************************************************************************************************



contains




    !>
    !!
    !!  @author Nathan A. Wukie 
    !!  @date   9/4/2016
    !!
    !!
    !------------------------------------------------------------------------------------------------
    subroutine graphics_process_faces(self,mesh)
        class(graphics_bc_t),   intent(inout)   :: self
        type(mesh_t),           intent(in)      :: mesh(:)

        character(len=:),   allocatable :: user_msg, dev_msg
        integer(ik)                     :: idom, ielem, iface, icoord, ipt, ieta, ixi, iface_bc
        real(rk)                        :: xi, eta

        real(rk),       dimension(5)    :: xi_points, eta_points
        real(rk),       dimension(25,3) :: points
        integer(ik),    dimension(32,3) :: faces


        if (.not. self%isInitialized) then 
            user_msg = "It seems like a graphics boundary condition container was not &
                        initialized correctly from a standard boundary condition container."
            dev_msg  = "Check to make sure that all instances of graphics_bc_t's are  &
                        initialized from bc_t's and that allocation or assignment statements  &
                        are copying all geometry and initialization data"
            call chidg_signal_one(FATAL,user_msg,dev_msg=dev_msg)
        end if
        

        !
        ! Clear graphics data if necessary in case the object is being reused
        !
        if (allocated(self%graphics_faces))  deallocate(self%graphics_faces)
        if (allocated(self%graphics_points)) deallocate(self%graphics_points)



        !
        ! Computational coordinates of a bc_t face to be sampled at.
        !
        xi_points  = [-1._rk, -0.5_rk, 0._rk, 0.5_rk, 1._rk]
        eta_points = [-1._rk, -0.5_rk, 0._rk, 0.5_rk, 1._rk]



        !
        ! Process the boundary condition faces and convert them to gui faces
        !
        do iface_bc = 1,size(self%faces)

            idom  = self%dom(iface_bc)
            ielem = self%elems(iface_bc)
            iface = self%faces(iface_bc)


            !
            ! Sample the face
            !
            do icoord = 1,3

                ipt = 1
                do ieta = 1,size(eta_points)
                    do ixi = 1,size(xi_points)

                        xi  = xi_points(ixi)
                        eta = eta_points(ieta)


                        if ( (iface == XI_MIN) ) then
                            points(ipt,icoord) = mesh(idom)%elems(ielem)%grid_point(icoord,-ONE, xi, eta)
                        else if ( (iface == XI_MAX) ) then
                            points(ipt,icoord) = mesh(idom)%elems(ielem)%grid_point(icoord, ONE, xi, eta)
                        else if ( (iface == ETA_MIN) ) then
                            points(ipt,icoord) = mesh(idom)%elems(ielem)%grid_point(icoord, xi,-ONE, eta)
                        else if ( (iface == ETA_MAX) ) then
                            points(ipt,icoord) = mesh(idom)%elems(ielem)%grid_point(icoord, xi, ONE, eta)
                        else if ( (iface == ZETA_MIN) ) then
                            points(ipt,icoord) = mesh(idom)%elems(ielem)%grid_point(icoord, xi, eta,-ONE)
                        else if ( (iface == ZETA_MAX) ) then
                            points(ipt,icoord) = mesh(idom)%elems(ielem)%grid_point(icoord, xi, eta, ONE)
                        end if


                        ipt = ipt + 1
                
                    end do !ixi
                end do !ieta
            
            end do !icoord



            !
            ! Store the face connectivities. This is the connectivity for a triangularly sampled
            ! representation of a single face in a standard bc_t face.
            !
            faces(:,1) = [1,  2,  2,  3,  3,  4,  4,  5,  6,  7, &
                          7,  8,  8,  9,  9,  10, 11, 12, 12, 13, &
                          13, 14, 14, 15, 16, 17, 17, 18, 18, 19, 19, 20]

            faces(:,2) = [2,  7,  3,  8,  4,  9,  5,  10, 7,  12, &
                          8,  13, 9,  14, 10, 15, 12, 17, 13, 18, &
                          14, 19, 15, 20, 17, 22, 18, 23, 19, 24, 20, 25]

            faces(:,3) = [6,  6,  7,  7,  8,  8,  9,  9,  11, 11, &
                          12, 12, 13, 13, 14, 14, 16, 16, 17, 17, &
                          18, 18, 19, 19, 21, 21, 22, 22, 23, 23, 24, 24]


            !
            ! append the primitive face data to graphics_faces and graphics_points
            !
            call self%graphics_add_faces(points,faces)



        end do !iface_bc






    end subroutine graphics_process_faces
    !***************************************************************************************************




















    !>
    !!
    !!  @author Nathan A. Wukie 
    !!  @date   9/4/2016
    !!
    !!
    !!
    !--------------------------------------------------------------------------------------------------
    subroutine graphics_add_faces(self,points,faces)
        class(graphics_bc_t),   intent(inout)   :: self
        real(rk),               intent(inout)   :: points(:,:)
        integer(ik),            intent(inout)   :: faces(:,:)

        integer(ik)                 :: npoints_before, nfaces_before, npoints_after, nfaces_after, ierr
        real(rk),       allocatable :: temp_points(:,:)
        integer(ik),    allocatable :: temp_faces(:,:)


        !
        ! Allocate storage to resize arrays
        !
        npoints_before = self%graphics_npoints()
        nfaces_before  = self%graphics_nfaces()

        npoints_after = npoints_before + size(points,1)
        nfaces_after  = nfaces_before  + size(faces,1)

        allocate(temp_points(npoints_after,3), temp_faces(nfaces_after,3), stat=ierr)
        if (ierr /= 0) call AllocationError

        if (npoints_before > 0) then
            temp_points(1:npoints_before,1:3) = self%graphics_points(1:npoints_before,1:3)
            temp_faces(1:nfaces_before,1:3)   = self%graphics_faces(1:nfaces_before,1:3)
        end if


        !
        ! Increment incoming face connectivities
        !
        faces = faces + npoints_before - 1


        !
        ! Add faces and points to temp arrays
        !
        temp_points(npoints_before+1:, 1:3) = points
        temp_faces(nfaces_before+1:,1:3)    = faces


        !
        ! Move allocation to self
        !
        call move_alloc(temp_points,self%graphics_points)
        call move_alloc(temp_faces,self%graphics_faces)


    end subroutine graphics_add_faces
    !****************************************************************************************










    !>
    !!
    !!  @author Nathan A. Wukie
    !!  @date   9/4/2016
    !!
    !---------------------------------------------------------------------------------
    function graphics_nfaces(self) result(nfaces_)
        class(graphics_bc_t),    intent(in)  :: self

        integer(ik) :: nfaces_

        if (allocated(self%graphics_faces)) then
            nfaces_ = size(self%graphics_faces,1)
        else
            nfaces_ = 0
        end if

    end function graphics_nfaces
    !**********************************************************************************




    !>
    !!
    !!  @author Nathan A. Wukie
    !!  @date   9/4/2016
    !!
    !---------------------------------------------------------------------------------
    function graphics_npoints(self) result(npoints_)
        class(graphics_bc_t),    intent(in)  :: self

        integer(ik) :: npoints_

        if (allocated(self%graphics_points)) then
            npoints_ = size(self%graphics_points,1)
        else
            npoints_ = 0
        end if

    end function graphics_npoints
    !**********************************************************************************





    !>
    !!
    !!  @author Nathan A. Wukie
    !!  @date   9/4/2016
    !!
    !----------------------------------------------------------------------------------
    function graphics_get_points(self,npoints,ndim) result(points)
        class(graphics_bc_t),    intent(in)  :: self
        integer(ik),        intent(in)  :: npoints
        integer(ik),        intent(in)  :: ndim

        real(rk), dimension(npoints,ndim) :: points

        points = self%graphics_points(1:npoints,1:ndim)

    end function graphics_get_points
    !**********************************************************************************





    !>
    !!
    !!  @author Nathan A. Wukie
    !!  @date   9/4/2016
    !!
    !----------------------------------------------------------------------------------
    function graphics_get_faces(self,nfaces,ndim) result(faces)
        class(graphics_bc_t),   intent(in)  :: self
        integer(ik),            intent(in)  :: nfaces
        integer(ik),            intent(in)  :: ndim

        integer(ik), dimension(nfaces,ndim) :: faces

        faces = self%graphics_faces(1:nfaces,1:ndim)

    end function graphics_get_faces
    !**********************************************************************************


end module type_graphics_bc
