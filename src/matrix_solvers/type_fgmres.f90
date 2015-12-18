module type_fgmres
#include <messenger.h>
    use mod_kinds,              only: rk, ik
    use mod_constants,          only: DIAG, ZERO, TWO
    use mod_inv,                only: inv
    use atype_matrixsolver,     only: matrixsolver_t 
    use type_preconditioner,    only: preconditioner_t
    use type_chidgVector
    use type_chidgMatrix

    use operator_chidg_mv
    use operator_chidg_dot,     only: dot
    use mod_inv,                only: inv


    use precon_jacobi,      only: precon_jacobi_t
    use precon_identity,    only: precon_identity_t
    implicit none
        






    !> Generalized Minimum Residual linear system solver
    !!
    !!  @author Nathan A. Wukie
    !!
    !-------------------------------------------
    type, public, extends(matrixsolver_t) :: fgmres_t

        integer(ik)     :: m = 200

        !type(fgmres_t), allocatable  :: multigrid   not supported by gfortran yet
        !type(fgmres_t), pointer  :: multigrid

    contains

        procedure   :: solve
    end type fgmres_t





contains


    !> Solution routine
    !!
    !!  @author Nathan A. Wukie
    !!
    !!
    !!
    !--------------------------------------------------------------
    subroutine solve(self,A,x,b,M,info)
        class(fgmres_t),            intent(inout)               :: self
        type(chidgMatrix_t),        intent(inout)               :: A
        type(chidgVector_t),        intent(inout)               :: x
        type(chidgVector_t),        intent(inout)               :: b
        class(preconditioner_t),    intent(inout), optional     :: M
        character(len=*),           intent(inout), optional     :: info



        type(chidgVector_t)                     :: r, r0, diff, xold, w, x0
        type(chidgVector_t), allocatable        :: v(:), z(:)
        real(rk),            allocatable        :: h(:,:), h_square(:,:)
        real(rk),            allocatable        :: p(:), y(:), c(:), s(:), p_dim(:), y_dim(:)
        real(rk)                                :: pj, pjp, h_ij, h_ipj

        integer(ik) :: iparent, ierr, ivec, isol, nvecs, ielem
        integer(ik) :: i, j, k, l                 ! Loop counters
        real(rk)    :: res, err, r0norm, gam

        logical     :: converged = .false.
        logical     :: max_iter  = .false.

        logical :: equal = .false.

        real(rk), allocatable    :: Dref(:,:), Dp(:,:)



        type(chidgMatrix_t)             :: Amg
        type(chidgVector_t)             :: xmg, bmg
        type(fgmres_t)                  :: multigrid
        type(precon_identity_t)         :: M_identity
        character(len=:), allocatable   :: info_mg


        !
        ! Print information
        !
        if (present(info)) print*, '               Matrix Solver - Info: ', info




        !
        ! Start timer
        !
        call self%timer%reset()
        call self%timer%start()
        call write_line('           Matrix Solver: ')
!        print*, '           Matrix Solver: '






        !
        ! Update preconditioner
        !
        call M%update(A,b)






        !
        ! Allocate and initialize Krylov vectors V
        !
        allocate(v(self%m+1),  &
                 z(self%m+1), stat=ierr)
        if (ierr /= 0) call AllocationError

        do ivec = 1,size(v)
            v(ivec) = b
            z(ivec) = b
            call v(ivec)%clear()
            call z(ivec)%clear()
        end do




        !
        ! Allocate hessenberg matrix to store orthogonalization
        !
        allocate(h(self%m + 1, self%m), stat=ierr)
        if (ierr /= 0) call AllocationError
        h = ZERO




        !
        ! Allocate vectors for solving hessenberg system
        !
        allocate(p(self%m+1), &
                 y(self%m+1), &
                 c(self%m+1), &
                 s(self%m+1), stat=ierr)
        if (ierr /= 0) call AllocationError
        p = ZERO
        y = ZERO
        c = ZERO
        s = ZERO



        !
        ! Set initial solution x. ZERO
        !
        x0 = x
        call x0%clear()
        call x%clear()


        self%niter = 0





        !
        ! Coarse-scale correction Algebraic Multigrid
        !
        if (present(info)) then
            if ( info == 'multigrid' ) then

                !
                ! If we are already in the multigrid solve, don't do another one. Just report.
                !
                print*, '                 Coarse grid correction'

            end if

        else

            !
            ! Fill coarse-scale data
            !
            Amg = A%P(0)
            xmg = x%P(0)
            bmg = b%P(0)

            !
            ! Compute coarse-scale correction
            !
            info_mg = 'multigrid'
            call multigrid%solve(Amg,xmg,bmg,M_identity,info_mg)

            !
            ! TODO: Prolongate coarse-scale correction to full resolution correction
            !
            x0 = xmg%P(3)

        end if











        res = 1._rk
        do while (res > self%tol)

            !
            ! Clear working variables
            !
            do ivec = 1,size(v)
                call v(ivec)%clear()
                call z(ivec)%clear()
            end do


            p = ZERO
            y = ZERO
            c = ZERO
            s = ZERO
            h = ZERO



            !
            ! Compute initial residual r0, residual norm, and normalized r0
            !
            r0      = self%residual(A,x0,b)
            p(1)    = r0%norm()
            v(1)    = r0/r0%norm()


            !
            ! Outer GMRES Loop
            !
            nvecs = 0
            do j = 1,self%m
                nvecs = nvecs + 1
           


                !
                ! Apply preconditioner:  z(j) = Minv * v(j)
                !
                z(j) = M%apply(A,v(j))



                !
                ! Compute w = Av for the current iteration
                !
                w = A*z(j)



                !
                ! Orthogonalization loop
                !
                do i = 1,j

                    h(i,j) = dot(w,v(i))
                    
                    w  = w - h(i,j)*v(i)

                end do

                h(j+1,j) = w%norm()




                !
                ! Compute next Krylov vector
                !
                v(j+1) = w/h(j+1,j)




                !
                ! Previous Givens rotations on h
                !
                if (j /= 1) then
                    do i = 1,j-1
                        ! Need temp values here so we don't directly overwrite the h(i,j) and h(i+1,j) values 
                        h_ij     = c(i)*h(i,j)  +  s(i)*h(i+1,j)
                        h_ipj    = -s(i)*h(i,j)  +  c(i)*h(i+1,j)


                        h(i,j)    = h_ij
                        h(i+1,j)  = h_ipj
                    end do
                end if



                !
                ! Compute next rotation
                !
                gam  = sqrt( h(j,j)*h(j,j)  +  h(j+1,j)*h(j+1,j) )
                c(j) = h(j,j)/gam
                s(j) = h(j+1,j)/gam



                !
                ! Givens rotation on h
                !
                h(j,j)   = gam
                h(j+1,j) = ZERO


                !
                ! Givens rotation on p. Need temp values here so we aren't directly overwriting the p(j) value until we want to
                !
                pj = c(j)*p(j)
                pjp = -s(j)*p(j)


                p(j)     = pj
                p(j+1)   = pjp



                
                !
                ! Update iteration counter
                !
                self%niter = self%niter + 1_ik



                !
                ! Test exit conditions
                !
                res = abs(p(j+1))
                call write_line(res)
!                print*, res
                converged = (res < self%tol)
                
                if ( converged ) then
                    exit
                end if




            end do  ! Outer GMRES Loop - m








            !
            ! Solve upper-triangular system y = hinv * p
            !
            if (allocated(h_square)) then
                deallocate(h_square,p_dim,y_dim)
            end if
            
            allocate(h_square(nvecs,nvecs), &
                    p_dim(nvecs),      &
                    y_dim(nvecs), stat=ierr)
            if (ierr /= 0) call AllocationError




            !
            ! Store h and p values to appropriately sized matrices
            !
            do l=1,nvecs
                do k=1,nvecs
                    h_square(k,l) = h(k,l)
                end do
                p_dim(l) = p(l)
            end do




            !
            ! Solve the system
            !
            h_square = inv(h_square)
            y_dim = matmul(h_square,p_dim)



            !
            ! Reconstruct solution
            !
            x = x0
            do isol = 1,nvecs
                x = x + y_dim(isol)*z(isol)
            end do



            !
            ! Test exit condition
            !
            if ( converged ) then
                exit
            else
                x0 = x
            end if



        end do   ! while









        !
        ! Report
        !
        err = self%error(A,x,b)
        call write_line('   Matrix Solver Error: ', err,delimiter='')
!        print*, '   Matrix Solver Error: ', err

        call self%timer%stop()
        call self%timer%report('Matrix solver compute time: ')



    end subroutine solve



end module type_fgmres
