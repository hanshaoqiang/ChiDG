module type_gaussseidel
#include <messenger.h>
    use mod_kinds,          only: rk, ik
    use mod_constants,      only: DIAG
    use mod_inv,            only: inv
    use atype_matrixsolver, only: matrixsolver_t 
    use type_blockmatrix,   only: blockmatrix_t
    use type_densematrix,   only: densematrix_t
    use type_blockvector
        






    !> Direct solve of linear system of equations via 
    !! inversion of A in Ax=b
    !!
    !!  @author Nathan A. Wukie
    !!
    !-------------------------------------------
    type, public, extends(matrixsolver_t) :: gaussseidel_t



    contains

        procedure   :: solve
    end type gaussseidel_t





contains


    !> Solution routine
    !!
    !!  @author Nathan A. Wukie
    !!
    !!
    !!
    !--------------------------------------------------------------
    subroutine solve(self,A,x,b)
        class(gaussseidel_t),   intent(inout)   :: self
        type(blockmatrix_t),    intent(inout)   :: A
        type(blockvector_t),    intent(inout)   :: x
        type(blockvector_t),    intent(inout)   :: b



        type(blockvector_t)                     :: r, diff, xold
        type(densematrix_t)                     :: D(size(x%lvecs))
        integer(ik) :: iparent, ierr
        real(rk)    :: res, err

        integer(ik) :: i


        print*, '           Matrix Solver: '


        ! Initialize D blocks
        do ielem = 1,size(A%lblks,1)
            D(ielem) = A%lblks(ielem,DIAG)
        end do


        ! Replace the block diagonal D with inverse of D
        do ielem = 1,size(A%lblks,1)
            D(ielem)%mat = inv(D(ielem)%mat)
        end do


        xold = x
        call x%clear()
        call xold%clear()


        res = 1._rk
        do while (res > self%tol)
            
            

            ! form b - Ax, except for diagonal
            r = b
            do ielem = 1,size(A%lblks,1)
                do iblk = 1,6   ! don't multiply diagonal

                    if (allocated(A%lblks(ielem,iblk)%mat)) then
                        ! Check for self-periodicity, where iparent == ielem. We want to skip this case,
                        ! since it should fall in the diagonal block.
                        iparent = A%lblks(ielem,iblk)%parent()
                        r%lvecs(ielem)%vec = r%lvecs(ielem)%vec - matmul(A%lblks(ielem,iblk)%mat,x%lvecs(iparent)%vec)

                    end if

                end do
                ! Multiply by inverse block diagonal
                x%lvecs(ielem)%vec = matmul(D(ielem)%mat,r%lvecs(ielem)%vec)
            end do


            diff = x - xold
            res = diff%norm()/x%norm()
            print*, res

            xold = x    ! store old solution

        end do ! res > self%tol




        err = self%error(A,x,b)
        print*, '   Matrix Solver Error: ', err








    end subroutine solve



end module type_gaussseidel
