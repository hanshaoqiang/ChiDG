!>  Boundary condition module
!!      - contains instantiations of all boundary condition for dynamically creating boundary conditions at run-time
!!      
!!
!!  Registering boundary conditions
!!      - To register a boundary condition:
!!          1st: Import it's definition for use in the current module
!!          2nd: In the 'register_bcs' routine, declare an instance of the boundary condition
!!          3rd: In the 'register_bcs' routine, push an instanace of the bc to the registered_bcs vector 
!!
!--------------------------------------------------------
module mod_bc
#include <messenger.h>
    use mod_kinds,      only: rk,ik
    use type_bc,        only: bc_t
    use type_bcvector,  only: bcvector_t

    !
    ! Import boundary conditions
    !
    use bc_periodic,                        only: periodic_t
    use bc_linearadvection_extrapolate,     only: linearadvection_extrapolate_t
    use bc_euler_wall,                      only: euler_wall_t
    use bc_euler_totalinlet,                only: euler_totalinlet_t
    use bc_euler_totalinlet_characteristic, only: euler_totalinlet_characteristic_t
    use bc_euler_pressureoutlet,            only: euler_pressureoutlet_t
    use bc_euler_extrapolate,               only: euler_extrapolate_t
    use bc_euler_giles_outlet,              only: euler_giles_outlet_t
    use bc_euler_giles_outlet_2D_a,         only: euler_giles_outlet_2D_a_t
    use bc_euler_giles_outlet_2D_b,         only: euler_giles_outlet_2D_b_t

    use bc_lineuler_inlet,                  only: lineuler_inlet_t
    use bc_lineuler_outlet,                 only: lineuler_outlet_t
    use bc_lineuler_extrapolate,            only: lineuler_extrapolate_t
    use bc_lineuler_wall,                   only: lineuler_wall_t

    use bc_primlineuler_inlet,              only: primlineuler_inlet_t
    use bc_primlineuler_outlet,             only: primlineuler_outlet_t
    use bc_primlineuler_extrapolate,        only: primlineuler_extrapolate_t
    use bc_primlineuler_wall,               only: primlineuler_wall_t

    use bc_primlineuleraxi_inlet,           only: primlineuleraxi_inlet_t
    use bc_primlineuleraxi_outlet,          only: primlineuleraxi_outlet_t
    use bc_primlineuleraxi_extrapolate,     only: primlineuleraxi_extrapolate_t
    use bc_primlineuleraxi_wall,            only: primlineuleraxi_wall_t
    use bc_primlineuleraxi_axi,             only: primlineuleraxi_axi_t

    use bc_kirchoff,                        only: kirchoff_t

    implicit none




    !>
    !!
    !!  @author Nathan A. Wukie 
    !!  @date   9/5/2016
    !!
    !!
    !--------------------------------------------------------------------------------
    type, public :: bc_factory_t

        type(bcvector_t)    :: registered_bcs

        logical             :: initialized_ = .false.

    contains
        
        procedure   :: init
        procedure   :: initialized
        procedure   :: check_initialized

        procedure   :: create

        procedure   :: size
        procedure   :: get_name

    end type bc_factory_t
    !********************************************************************************




    type(bc_factory_t)  :: bc_factory



contains


    !>  Register boundary conditions in a module vector.
    !!
    !!  This allows the available boundary conditions to be queried in the same way that they 
    !!  are registered for allocation. 
    !!
    !!  This gets called by chidg%init('env')
    !!
    !!  @author Nathan A. Wukie
    !!  @date   2/8/2016
    !!
    !!
    !--------------------------------------------------------------------------------------------
    subroutine init(self)
        class(bc_factory_t),    intent(inout)   :: self
        integer :: nbcs, ibc

        !
        ! Instantiate bcs
        !
        type(periodic_t)                        :: PERIODIC
        type(linearadvection_extrapolate_t)     :: LINEARADVECTION_EXTRAPOLATE

        type(euler_wall_t)                      :: EULER_WALL
        type(euler_totalinlet_t)                :: EULER_TOTALINLET
        type(euler_totalinlet_characteristic_t) :: EULER_TOTALINLET_CHARCTERISTIC
        type(euler_pressureoutlet_t)            :: EULER_PRESSUREOUTLET
        type(euler_extrapolate_t)               :: EULER_EXTRAPOLATE
        type(euler_giles_outlet_t)              :: EULER_GILES_OUTLET
        type(euler_giles_outlet_2D_a_t)         :: EULER_GILES_OUTLET_2D_A
        type(euler_giles_outlet_2D_b_t)         :: EULER_GILES_OUTLET_2D_B

        type(lineuler_inlet_t)                  :: LINEULER_INLET
        type(lineuler_outlet_t)                 :: LINEULER_OUTLET
        type(lineuler_extrapolate_t)            :: LINEULER_EXTRAPOLATE
        type(lineuler_wall_t)                   :: LINEULER_WALL

        type(primlineuler_inlet_t)              :: PRIMLINEULER_INLET
        type(primlineuler_outlet_t)             :: PRIMLINEULER_OUTLET
        type(primlineuler_extrapolate_t)        :: PRIMLINEULER_EXTRAPOLATE
        type(primlineuler_wall_t)               :: PRIMLINEULER_WALL

        type(primlineuleraxi_inlet_t)           :: PRIMLINEULERAXI_INLET
        type(primlineuleraxi_outlet_t)          :: PRIMLINEULERAXI_OUTLET
        type(primlineuleraxi_extrapolate_t)     :: PRIMLINEULERAXI_EXTRAPOLATE
        type(primlineuleraxi_wall_t)            :: PRIMLINEULERAXI_WALL
        type(primlineuleraxi_axi_t)             :: PRIMLINEULERAXI_AXI


        type(kirchoff_t)                        :: KIRCHOFF

        if ( .not. self%initialized() ) then
            !
            ! Register in global vector
            !
            call self%registered_bcs%push_back(PERIODIC)
            call self%registered_bcs%push_back(LINEARADVECTION_EXTRAPOLATE)


            call self%registered_bcs%push_back(EULER_WALL)
            call self%registered_bcs%push_back(EULER_TOTALINLET)
            call self%registered_bcs%push_back(EULER_TOTALINLET_CHARCTERISTIC)
            call self%registered_bcs%push_back(EULER_PRESSUREOUTLET)
            call self%registered_bcs%push_back(EULER_EXTRAPOLATE)
            call self%registered_bcs%push_back(EULER_GILES_OUTLET)
            call self%registered_bcs%push_back(EULER_GILES_OUTLET_2D_A)
            call self%registered_bcs%push_back(EULER_GILES_OUTLET_2D_B)


            call self%registered_bcs%push_back(LINEULER_INLET)
            call self%registered_bcs%push_back(LINEULER_OUTLET)
            call self%registered_bcs%push_back(LINEULER_EXTRAPOLATE)
            call self%registered_bcs%push_back(LINEULER_WALL)


            call self%registered_bcs%push_back(PRIMLINEULER_INLET)
            call self%registered_bcs%push_back(PRIMLINEULER_OUTLET)
            call self%registered_bcs%push_back(PRIMLINEULER_EXTRAPOLATE)
            call self%registered_bcs%push_back(PRIMLINEULER_WALL)


            call self%registered_bcs%push_back(PRIMLINEULERAXI_INLET)
            call self%registered_bcs%push_back(PRIMLINEULERAXI_OUTLET)
            call self%registered_bcs%push_back(PRIMLINEULERAXI_EXTRAPOLATE)
            call self%registered_bcs%push_back(PRIMLINEULERAXI_WALL)
            call self%registered_bcs%push_back(PRIMLINEULERAXI_AXI)


            call self%registered_bcs%push_back(KIRCHOFF)




            !
            ! Initialize each boundary condition in set. Doesn't need modified.
            !
            nbcs = self%registered_bcs%size()
            do ibc = 1,nbcs
                call self%registered_bcs%data(ibc)%bc%add_options()
            end do

            !
            ! Confirm initialization
            !
            self%initialized_ = .true.

        end if

    end subroutine init
    !********************************************************************************************








    !>  Boundary condition factory
    !!      - Allocate a concrete boundary condition type based on the incoming string specification.
    !!      - Initialize the allocated boundary condition.
    !!
    !!  @author Nathan A. Wukie
    !!  @date   1/31/2016
    !!
    !!  @param[in]      string  Character string used to select the appropriate boundary condition
    !!  @param[inout]   bc      Allocatable boundary condition
    !!
    !-----------------------------------------------------------------------------------------------
    subroutine create(self,bcstring,bc)
        class(bc_factory_t),            intent(inout)   :: self
        character(*),                   intent(in)      :: bcstring
        class(bc_t),    allocatable,    intent(inout)   :: bc

        integer(ik) :: ierr, bcindex


        ! Make sure the object was initialized
        call self%check_initialized()


        if ( allocated(bc) ) then
            deallocate(bc)
        end if



        !
        ! Find equation set in 'registered_bcs' vector
        !
        bcindex = self%registered_bcs%index_by_name(trim(bcstring))



        !
        ! Check equationset was found in 'registered_bcs'
        !
        if (bcindex == 0) call chidg_signal_one(FATAL,"create_bc: boundary condition not recognized", trim(bcstring))



        !
        ! Allocate conrete bc_t instance
        !
        allocate(bc, source=self%registered_bcs%data(bcindex)%bc, stat=ierr)
        if (ierr /= 0) call chidg_signal(FATAL,"create: error allocating boundary condition from global vector.")



        !
        ! Check boundary condition was allocated
        !
        if ( .not. allocated(bc) ) call chidg_signal(FATAL,"create_bc: error allocating concrete boundary condition.")



    end subroutine create
    !******************************************************************************************










    !>  Return the number of boundary conditions registered in the factory
    !!
    !!  @author Nathan A. Wukie 
    !!  @date   9/5/2016
    !!
    !!
    !------------------------------------------------------------------------------------------
    function size(self) result(size_)
        class(bc_factory_t),    intent(in)  :: self

        integer(ik) :: size_

        ! Make sure the object was initialized
        call self%check_initialized()

        size_ = self%registered_bcs%size()

    end function size
    !******************************************************************************************













    !>  Return the name of the i'th boundary condition in the factory
    !!
    !!  @author Nathan A. Wukie
    !!  @date   2/8/2016
    !!
    !------------------------------------------------------------------------------------------
    function get_name(self,ibc) result(bcname)
        class(bc_factory_t),    intent(in)  :: self
        integer(ik)                         :: ibc

        character(len=:),   allocatable :: bcname

        ! Make sure the object was initialized
        call self%check_initialized()

        bcname = self%registered_bcs%data(ibc)%bc%get_name()

    end function get_name
    !******************************************************************************************








    !>  Return the initialization status of the factory
    !!
    !!  @author Nathan A. Wukie
    !!  @date   9/6/2016
    !!
    !!
    !------------------------------------------------------------------------------------------
    function initialized(self) result(initialized_)
        class(bc_factory_t),    intent(in)  :: self

        logical :: initialized_

        initialized_ = self%initialized_

    end function initialized
    !******************************************************************************************






    !>  Check the initialization status of the object and handle any action or reporting
    !!
    !!  @author Nathan A. Wukie
    !!  @date   9/5/2016
    !!
    !!
    !-------------------------------------------------------------------------------------------
    subroutine check_initialized(self)
        class(bc_factory_t),    intent(in)  :: self


        if (.not. self%initialized()) call chidg_signal(FATAL,"bc_factory: Factory was not initialized so it also &
                                                               won't have any components to create. Make sure to  &
                                                               initialize the ChiDG environment. Try calling chidg%init('env')")

    end subroutine check_initialized
    !*******************************************************************************************
























end module mod_bc
