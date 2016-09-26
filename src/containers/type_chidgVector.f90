module type_chidgVector
#include <messenger.h>
    use mod_kinds,          only: rk, ik
    use mod_constants,      only: ZERO, TWO
    use type_mesh,          only: mesh_t
    use type_function,      only: function_t
    use mod_project,        only: project_function_xyz
    use type_blockvector
    implicit none





    !> Container stores a blockvector_t for each domain_t
    !!
    !!  @author Nathan A. Wukie
    !!  @date   2/1/2016
    !!
    !!
    !!
    !!
    !----------------------------------------------------------------------------------------------------
    type, public :: chidgVector_t

        type(blockvector_t), allocatable    :: dom(:)

        logical :: initialized = .false.

    contains
        ! Initializers
        generic, public     :: init => initialize

        procedure, private  :: initialize

        ! Modifiers
        procedure, public   :: clear

        ! Interogators
        procedure, public   :: norm
        procedure, public   :: dump
        !procedure, public   :: nentries
        !procedure, public   :: ndomains


        procedure, public   :: project


        !final               :: destructor

    end type chidgVector_t
    !****************************************************************************************************










    !------------------------       OPERATORS       --------------------------------------

    public operator (*)
    interface operator(*)
        module procedure mult_real_chidgVector          ! real * chidgVector
        module procedure mult_chidgVector_real          ! chidgVector * real
    end interface


    public operator (/)
    interface operator (/)
        module procedure div_real_chidgVector           ! real / chidgVector
        module procedure div_chidgVector_real           ! chidgVector / real
    end interface


    public operator (-)
    interface operator (-)
        module procedure sub_chidgVector_chidgVector    ! chidgVector - chidgVector
    end interface

    public operator (+)
    interface operator (+)
        module procedure add_chidgVector_chidgVector    ! chidgVector + chidgVector
    end interface
















contains





    !> Subroutine for initializing chidgVector_t
    !!
    !!  @author Nathan A. Wukie
    !!  @date   2/1/2016
    !!
    !!  @param[in]  mesh    Array of mesh_t instances used to initialize each blockvector_t subcomponent.
    !!
    !----------------------------------------------------------------------------------------------------
    subroutine initialize(self,mesh,init_type)
        class(chidgVector_t),   intent(inout)   :: self
        type(mesh_t),           intent(in)      :: mesh(:)
        character(len=*),       intent(in)      :: init_type

        integer(ik) :: ierr, ndomains, idom, neqns
        character(len=:), allocatable   :: user_msg, dev_msg


        !
        ! Allocate blockvector_t for each mesh
        !
        ndomains = size(mesh)
        allocate(self%dom(ndomains), stat=ierr)
        if (ierr /= 0) call AllocationError



        !
        ! Call initialization procedure for each blockvector_t
        !
        do idom = 1,ndomains

            !
            ! Set the number of equations to initialize in the vector based in the initialization type
            !
            select case (init_type)
                case('equations')
                    neqns = mesh(idom)%neqns
                case('parameter')
                    neqns = 1
                case default
                    user_msg = "A chidgVector was trying to determine if it was supposed to initialize a set of equations &
                                or a single parameter. Unfortunately, the string that was passed in didn't correnspond to either &
                                of those cases. Valid values for init_type are 'equations' and 'parameter'"
                    dev_msg = "chidgVector%initialize: bad value for init_type"
                    call chidg_signal_two(OOPS, user_msg, trim(init_type), dev_msg=dev_msg)
            end select


            !
            ! Call blockvector initialization
            !
            call self%dom(idom)%init(mesh(idom),neqns)

        end do


        ! Set initialization status
        self%initialized = .true.

    end subroutine initialize
    !****************************************************************************************************








    !> Set all floating-point vector entries to zero.
    !!
    !!  @author Nathan A. Wukie
    !!  @date   2/1/2016
    !!
    !!
    !----------------------------------------------------------------------------------------------------
    subroutine clear(self)
        class(chidgVector_t),   intent(inout)   :: self

        integer :: idom


        !
        ! Call clear procedure for each blockvector_t
        !
        do idom = 1,size(self%dom)
            call self%dom(idom)%clear()
        end do


    end subroutine clear
    !*****************************************************************************************************







    !> Compute the L2-Norm of the vector
    !!
    !!  @author Nathan A. Wukie
    !!  @date   2/1/2016
    !!
    !!  @return res L2-norm of the vector
    !!
    !-----------------------------------------------------------------------------------------------------
    function norm(self) result(res)
        class(chidgVector_t),   intent(in)   :: self

        real(rk)    :: res
        integer(ik) :: idom, ielem


        res = ZERO

        !
        ! Loop through domain vectors and compute contribution to vecotr L2-Norm
        !
        do idom = 1,size(self%dom)
            do ielem = 1,size(self%dom(idom)%lvecs)
            
                res = res + sum( self%dom(idom)%lvecs(ielem)%vec ** TWO )

            end do ! ielem
        end do ! idom


        !
        ! Take the square root of the result
        !
        res = sqrt(res)

    end function norm
    !*****************************************************************************************************







    !> Dump contents of the vector
    !!
    !!  @author Nathan A. Wukie
    !!  @date   2/1/2016
    !!
    !!
    !!
    !-----------------------------------------------------------------------------------------------------
    subroutine dump(self)
        class(chidgVector_t),   intent(in)   :: self

        integer(ik) :: idom



        !
        ! Loop through domain vectors and compute contribution to vecotr L2-Norm
        !
        do idom = 1,size(self%dom)
            
            call self%dom(idom)%dump()

        end do ! idom



    end subroutine dump
    !*****************************************************************************************************










    !>  Project functions to element solution variables
    !!
    !!  @author Nathan A. Wukie
    !!  @date   2/1/2016
    !!  @note   Moved from mod_grid_operators
    !!
    !!
    !!  @param[in]  ivar    Integer index of the variable being initialized
    !!  @param[in]  fcn     Function being projected to the solution
    !!
    !--------------------------------------------------------------------------------------------------------
    subroutine project(self,mesh,fcn,ivar)
        class(chidgVector_t),   intent(inout)   :: self
        type(mesh_t),           intent(in)      :: mesh(:)
        class(function_t),      intent(inout)   :: fcn
        integer(ik),            intent(in)      :: ivar

        integer(ik)                         :: ielem, ierr, idom, nterms, spacedim
        real(rk),           allocatable     :: fmodes(:)
        character(len=:),   allocatable     :: user_msg

        ! Check the vector has been initialized
        user_msg = "chidgVector%project: The vector was not initialized before the project routine was called. &
                    Make sure to call chidgVector%init before calling the projection routine"
        if (.not. self%initialized) call chidg_signal(FATAL,user_msg)


        !
        ! Loop through elements in mesh and call function projection
        !
        do idom = 1,size(mesh)

            ! Check that variable index 'ivar' is valid
            if (ivar > self%dom(idom)%lvecs(1)%nvars() ) call chidg_signal(FATAL,'initialize_variable: variable index ivar exceeds the number of equations')

            do ielem = 1,mesh(idom)%nelem

                    !
                    ! Get spacedim
                    !
                    spacedim = mesh(idom)%elems(ielem)%spacedim

                    !
                    ! Initial array allocation
                    !
                    nterms = self%dom(idom)%lvecs(ielem)%nterms()
                    if (.not. allocated(fmodes)) allocate(fmodes(nterms))


                    !
                    ! Reallocate mode storage if necessary. For example, if the order of the expansion was changed
                    !
                    if (size(fmodes) /= nterms) then
                        if (allocated(fmodes)) deallocate(fmodes)
                        allocate(fmodes(nterms), stat=ierr)
                        if (ierr /= 0) call AllocationError
                    end if


                    if (.not. allocated(fmodes)) call chidg_signal(FATAL,"initialize_variable: fmodes not allocated")


                    !
                    ! Call function projection
                    !
                    call project_function_xyz(fcn,spacedim,mesh(idom)%elems(ielem)%nterms_s,mesh(idom)%elems(ielem)%coords,fmodes)


                    !
                    ! Store the projected modes to the solution expansion
                    !
                    call self%dom(idom)%lvecs(ielem)%setvar(ivar,fmodes)

            end do ! ielem

        end do ! idomain

    end subroutine project
    !**************************************************************************************************************















    !---------------------------------------------------------------------------------------------
    !
    !
    !                              Operator Implementations
    !
    !---------------------------------------------------------------------------------------------



    !>
    !!  @author Nathan A. Wukie
    !!  @date   2/1/2016
    !!
    !------------------------------------------------------------------------
    function mult_real_chidgVector(left,right) result(res)
        real(rk),               intent(in)  :: left
        type(chidgVector_t),    intent(in)  :: right

        type(chidgVector_t) :: res
        integer(ik)         :: idom, ndom

        ndom = size(right%dom)

        allocate(res%dom(ndom))

        do idom = 1,size(right%dom)
            res%dom(idom) = left * right%dom(idom)
        end do

    end function mult_real_chidgVector
    !************************************************************************




    !>
    !!  @author Nathan A. Wukie
    !!  @date   2/1/2016
    !!
    !!
    !------------------------------------------------------------------------
    function mult_chidgVector_real(left,right) result(res)
        type(chidgVector_t),    intent(in)  :: left
        real(rk),               intent(in)  :: right

        type(chidgVector_t) :: res
        integer(ik)         :: idom, ndom

        ndom = size(left%dom)

        allocate(res%dom(ndom))

        do idom = 1,size(left%dom)
            res%dom(idom) = left%dom(idom) * right
        end do

    end function mult_chidgVector_real
    !************************************************************************





    !>
    !!  @author Nathan A. Wukie
    !!  @date   2/1/2016
    !!
    !!
    !------------------------------------------------------------------------
    function div_real_chidgVector(left,right) result(res)
        real(rk),               intent(in)  :: left
        type(chidgVector_t),    intent(in)  :: right

        type(chidgVector_t) :: res
        integer(ik)         :: idom, ndom

        ndom = size(right%dom)

        allocate(res%dom(ndom))

        do idom = 1,size(right%dom)
            res%dom(idom) = left / right%dom(idom)
        end do

    end function div_real_chidgVector
    !************************************************************************




    !>
    !!  @author Nathan A. Wukie
    !!  @date   2/1/2016
    !!
    !!
    !------------------------------------------------------------------------
    function div_chidgVector_real(left,right) result(res)
        type(chidgVector_t),    intent(in)  :: left
        real(rk),               intent(in)  :: right

        type(chidgVector_t) :: res
        integer(ik)         :: idom, ndom

        ndom = size(left%dom)

        allocate(res%dom(ndom))

        do idom = 1,size(left%dom)
            res%dom(idom) = left%dom(idom) / right
        end do

    end function div_chidgVector_real
    !*************************************************************************





    !>
    !!  @author Nathan A. Wukie
    !!  @date   2/1/2016
    !!
    !!
    !------------------------------------------------------------------------
    function add_chidgVector_chidgVector(left,right) result(res)
        type(chidgVector_t),    intent(in)  :: left
        type(chidgVector_t),    intent(in)  :: right

        type(chidgVector_t) :: res
        integer(ik)         :: idom, ndom

        ndom = size(right%dom)

        allocate(res%dom(ndom))

        do idom = 1,size(left%dom)
            res%dom(idom) = left%dom(idom) + right%dom(idom)
        end do

    end function add_chidgVector_chidgVector
    !*************************************************************************





    !>
    !!  @author Nathan A. Wukie
    !!  @date   2/1/2016
    !!
    !!
    !------------------------------------------------------------------------
    function sub_chidgVector_chidgVector(left,right) result(res)
        type(chidgVector_t),    intent(in)  :: left
        type(chidgVector_t),    intent(in)  :: right

        type(chidgVector_t) :: res
        integer(ik)         :: idom, ndom

        ndom = size(right%dom)

        allocate(res%dom(ndom))


        do idom = 1,size(left%dom)
            res%dom(idom) = left%dom(idom) - right%dom(idom)
        end do


    end function sub_chidgVector_chidgVector
    !*************************************************************************
























end module type_chidgVector
