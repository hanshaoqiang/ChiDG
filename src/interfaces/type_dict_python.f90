!> Dictionary container
!!
!!  @author Nathan A. Wukie
!!  @date   2/1/2016
!!
!!   The general structure for this type was crafted after
!!   an example on fortranwiki.org, hash table example.
!!   However, here we are just traversing a linked-list to find things,
!!   and not hashing anything
!!
!------------------------------------------------------------------------
module type_dict
    use mod_kinds,      only: rk,ik
    implicit none





    !> Dictionary Type for storing key-value pairs
    !!
    !!  @author Nathan A. Wukie
    !!  @date   2/1/2016
    !!
    !!
    !-------------------------------------------------
    type, public :: dict_t


    end type dict_t
    !*************************************************



contains





    !******************************************************
    subroutine get_real(self,key,val)
        type(dict_t),    intent(in)    :: self
        character(len=*), intent(in)    :: key
        real(rk),    intent(inout) :: val

    end subroutine 

    subroutine set_real(self,key,val)
        type(dict_t),    intent(inout) :: self
        character(len=*), intent(in)    :: key
        real(rk),    intent(in)    :: val


    end subroutine




    subroutine get_int(self,key,val)
        type(dict_t),       intent(in)    :: self
        character(len=*),   intent(in)    :: key
        integer(ik),        intent(inout) :: val

    end subroutine 

    subroutine set_int(self,key,val)
        type(dict_t),       intent(inout) :: self
        character(len=*),   intent(in)    :: key
        integer(ik),        intent(in)    :: val


    end subroutine



end module type_dict
