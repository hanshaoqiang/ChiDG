!>  This module isolates the assignment operator for returning polymorhpic types from the vector.
!!  In this way, a data type being store, (ex: point_t) can be used here, but it's definition doesn't
!!  get proliferate throughout the program when vector_t is used.
!!
!!  @author Nathan A. Wukie
!!  @date   6/4/2016
!!
!-------------------------------------------------------------------------------------------------
module mod_vector_return_data
#include <messenger.h>
    use mod_kinds,  only: rk, ik
    use type_point, only: point_t


    interface assignment(=)
        module procedure assign_ik, assign_rk, assign_point
    end interface


contains

    subroutine assign_ik(a,b)
        integer(ik),    intent(inout)   :: a
        class(*),       intent(in)      :: b

        select type ( b )
            type is (integer(ik))
                a = b
            class default
                call chidg_signal(FATAL,"vector%assign: different result and assign types")
        end select

    end subroutine assign_ik




    subroutine assign_rk(a,b)
        real(rk),   intent(inout)   :: a
        class(*),   intent(in)      :: b

        select type ( b )
            type is (real(rk))
                a = b
            class default
                call chidg_signal(FATAL,"vector%assign: different result and assign types")
        end select

    end subroutine assign_rk




    recursive subroutine assign_point(a,b)
        type(point_t),  intent(inout)   :: a
        class(*),       intent(in)      :: b

        select type ( b )
            type is (point_t)
                a = b
            class default
                call chidg_signal(FATAL,"vector%assign: different result and assign types")
        end select

    end subroutine assign_point

end module mod_vector_return_data
!************************************************************************************************












!>  A data type to store an array of polymorhpic data.
!!
!!
!!
!------------------------------------------------------------------------------------------------
module type_vector_data
    implicit none

    type, public :: vector_data_t
        class(*), allocatable :: elem
    end type vector_data_t

end module type_vector_data
!*************************************************************************************************














!>  The vector data container. USE the entire module as: 
!!
!!        use type_vector
!!
!!  In this way, the assignment operators will also be imported as well for returning
!!  polymorphic data.
!!
!!
!--------------------------------------------------------------------------------
module type_vector
#include <messenger.h>
    use mod_kinds,              only: rk, ik
    use type_vector_data,       only: vector_data_t
    use mod_vector_return_data, only: assignment(=)
    implicit none



    !>
    !!
    !!  @author Nathan A. Wukie
    !!  @date   5/11/2016
    !!
    !------------------------------------------------------------------------
    type, public :: vector_t

        integer(ik)                         :: size_        = 0
        integer(ik)                         :: capacity_    = 0
        integer(ik)                         :: buffer_      = 20
        type(vector_data_t),   allocatable  :: data(:)

    contains
        procedure, public   :: size
        procedure, public   :: capacity


        ! Data modifiers
        procedure,  public  :: push_back
        procedure,  public  :: clear
        procedure,  private :: increase_capacity


        ! Data accessors
        procedure,  public   :: at
        procedure,  public   :: check_bounds


    end type vector_t
    !*************************************************************************



contains



    !> This function returns the number of elements stored in the container
    !!
    !!  @author Nathan A. Wukie
    !!  @date   2/17/2016
    !!
    !!
    !-------------------------------------------------------------------------------
    function size(self) result(res)
        class(vector_t),    intent(in)  :: self
        integer(ik) :: res

        res = self%size_
    end function size
    !*******************************************************************************



    !> This function returns the total capacity of the container to store elements
    !!
    !!  @author Nathan A. Wukie
    !!  @date   2/17/2016
    !!
    !!
    !-------------------------------------------------------------------------------
    function capacity(self) result(res)
        class(vector_t),    intent(in)  :: self
        integer(ik) :: res

        res = self%capacity_
    end function capacity
    !*******************************************************************************









    !> Store element to end of vector
    !!
    !!  @author Nathan A. Wukie
    !!  @date   2/17/2016
    !!
    !!
    !-----------------------------------------------------------------------------------------
    subroutine push_back(self,element)
        class(vector_t), intent(inout)   :: self
        class(*),        intent(in)      :: element
        
        type(vector_data_t)     :: wrapper
        logical                 :: capacity_reached
        integer                 :: ierr, size

        !
        ! Test if container has storage available. If not, then increase capacity
        !
        capacity_reached = (self%size() == self%capacity())
        if (capacity_reached) then
            call self%increase_capacity()
        end if
        

        !
        ! Allocate wrapper component and store data
        !
        allocate(wrapper%elem, source=element, stat=ierr)
        if (ierr /= 0) call AllocationError


        !
        ! Add element to end of vector
        !
        size = self%size()
        allocate(self%data(size + 1)%elem, source=element, stat=ierr)
        if (ierr /= 0) call AllocationError
        !self%data(size + 1) = element


        !
        ! Increment number of stored elements
        !
        self%size_ = self%size_ + 1


    end subroutine push_back
    !*****************************************************************************************









    !> Clear container contents
    !!
    !!  @author Nathan A. Wukie
    !!  @date   5/11/2016
    !!
    !!
    !----------------------------------------------------------------------------------------
    subroutine clear(self)
        class(vector_t),   intent(inout)   :: self

        self%size_     = 0
        self%capacity_ = 0

        deallocate(self%data)

    end subroutine clear
    !****************************************************************************************












    !> Access element at index location
    !!
    !!  @author Nathan A. Wukie
    !!  @date   5/11/2016
    !!
    !----------------------------------------------------------------------------------------
    function at(self,index) result(res)
        class(vector_t),    intent(in)  :: self
        integer(ik),        intent(in)  :: index

        class(*), allocatable :: res


        call self%check_bounds(index)

        allocate(res, source=self%data(index)%elem)


    end function at
    !****************************************************************************************











    !> Increase the storage capacity of the vector by a buffer size predefined in the container
    !!
    !!  @author Nathan A. Wukie
    !!  @date   2/17/2016
    !!
    !!
    !------------------------------------------------------------------------------------------
    subroutine increase_capacity(self)
        class(vector_t),    intent(inout)   :: self

        type(vector_data_t), allocatable    :: temp(:)
        integer(ik)                         :: newsize, ierr


        !
        ! Allocate temporary vector of current size plus a buffer
        !
        if ( allocated(self%data) ) then
            newsize = ubound(self%data,1) + self%buffer_
        else
            newsize = self%buffer_
        end if

        allocate(temp(newsize),stat=ierr)
        if (ierr /= 0) call AllocationError


        !
        ! Copy any current data to temporary vector
        !
        if (allocated(self%data)) then
            temp(lbound(self%data,1):ubound(self%data,1))  =  self%data
        end if


        !
        ! Move alloc to move data back to self%data and deallocate temp
        !
        call move_alloc(FROM=temp,TO=self%data)


        !
        ! Reset capacity info
        !
        self%capacity_ = newsize


    end subroutine increase_capacity
    !******************************************************************************************









    !>  Check that the vector data access is within bounds. Error if out-of-bounds.
    !!
    !!  @author Nathan A. Wukie
    !!  @date   6/4/2016
    !!
    !!
    !!
    !-----------------------------------------------------------------------------------------
    subroutine check_bounds(self,index)
        class(vector_t),    intent(in)  :: self
        integer(ik),        intent(in)  :: index

        logical :: out_of_bounds

        out_of_bounds = (index > self%size())
        if (out_of_bounds) then
            call chidg_signal(FATAL,'vector_t%check_bounds: out of bounds access')
        end if

    end subroutine check_bounds
    !****************************************************************************************





end module type_vector
!**************************************************************************************************************
