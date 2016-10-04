!>  Chimera-based, discontinuous Galerkin equation solver
!!
!!  This program is designed to solve partial differential equations,
!!  and systems of partial differential equations, using the discontinuous
!!  Galerkin method for spatial discretization using Chimera, overset grids to
!!  represent the simulation domain.
!!
!!  @author Nathan A. Wukie
!!  @date   1/31/2016
!!
!!
!---------------------------------------------------------------------------------------------
program driver
#include <messenger.h>
    use mod_kinds,              only: rk, ik
    use mod_constants,          only: ONE, TWO, ZERO, HALF
    use type_chidg,             only: chidg_t
!    use mod_grid_operators,     only: initialize_variable
    use type_dict,              only: dict_t
    use type_function,          only: function_t
    use mod_function,           only: create_function
    use mod_tecio,              only: write_tecio_variables
    use mod_chidg_edit,         only: chidg_edit
    use mod_chidg_convert,      only: chidg_convert
    use mod_chidg_interpolate,  only: chidg_interpolate
    use mod_chidg_post,         only: chidg_post
    use mod_kirchoffs,          only: kirchoff
    use mod_io


    use type_vector
    use type_point, only: point_t


    
    !
    ! Variable declarations
    !
    implicit none
    type(chidg_t)                       :: chidg
    type(dict_t)                        :: toptions, noptions, loptions, poptions
    class(function_t),  allocatable     :: constant, monopole, blockage, gaussian

    integer(ik)                         :: narg
    character(len=1024)                 :: chidg_action, filename, file_a, file_b



    !
    ! Check for command-line arguments
    !
    narg = command_argument_count()


    !
    ! Execute ChiDG calculation
    !
    if ( narg == 0 ) then
        !
        ! Initialize ChiDG environment
        !
        call chidg%init('env')
        call chidg%init('io')


        !
        ! Set time-scheme options
        !
        call toptions%set('dt',dt)
        call toptions%set('nsteps',time_steps)
        call toptions%set('nwrite',nwrite)


        !
        ! Set nonlinear solver options
        !
        call noptions%set('tol',ntol)
        call noptions%set('cfl0',cfl0)
        call noptions%set('nsteps',nonlinear_steps)

        !
        ! Set linear solver options
        !
        call loptions%set('tol',ltol)


        !
        ! Set ChiDG components
        !
        call chidg%set('time_scheme',      time_scheme,      toptions)
        call chidg%set('nonlinear_solver', nonlinear_solver, noptions)
        call chidg%set('linear_solver',    linear_solver,    loptions)
        call chidg%set('preconditioner',   preconditioner,   poptions)






        !
        ! Read grid data from file
        !
        call chidg%read_grid(gridfile, spacedim)
        call chidg%read_boundaryconditions(gridfile)




        !
        ! Initialize solution data storage
        !
        call chidg%initialize_solution_domains(nterms_s)
        call chidg%init('chimera')
        call chidg%initialize_solution_solver()



        !
        ! Initialize solution
        !
        if (solutionfile_in == 'none') then

!            call create_function(gaussian,'gaussian')
!            call gaussian%set_option('b_x', 0.5_rk)
!            call gaussian%set_option('c', 0.1_rk)
!            call chidg%data%sdata%q%project(chidg%data%mesh,gaussian,1)


            call create_function(constant,'constant')

            ! rho
            call constant%set_option('val',1.20_rk)
            !call initialize_variable(chidg%data,1,constant)
            call chidg%data%sdata%q%project(chidg%data%mesh,constant,1)

            ! rho_u
            call constant%set_option('val',50._rk)
            !call initialize_variable(chidg%data,2,constant)
            call chidg%data%sdata%q%project(chidg%data%mesh,constant,2)

            ! rho_v
            call constant%set_option('val',0._rk)
            !call initialize_variable(chidg%data,3,constant)
            call chidg%data%sdata%q%project(chidg%data%mesh,constant,3)

            ! rho_w
            call constant%set_option('val',0._rk)
            !call initialize_variable(chidg%data,4,constant)
            call chidg%data%sdata%q%project(chidg%data%mesh,constant,4)

            ! rho_E
            call constant%set_option('val',230000._rk)
            !call initialize_variable(chidg%data,5,constant)
            call chidg%data%sdata%q%project(chidg%data%mesh,constant,5)




        else

            !
            ! TODO: put in check that solutionfile actually contains solution
            !
            call chidg%read_solution(solutionfile_in)

        end if

        

        !
        ! Wrap-up initialization activities
        !
        call chidg%init('finalize')



        !
        ! Write initial solution
        !
        if (initial_write) call chidg%write_solution(solutionfile_out)





        !
        ! Run ChiDG simulation
        !
        call chidg%run()


        !
        ! Write final solution
        !
        if (final_write) call chidg%write_solution(solutionfile_out)




        !
        ! Reporting
        !
        call chidg%report()































    !
    ! ChiDG tool execution. 2 arguments.
    !
    else if ( narg == 2 ) then


        call get_command_argument(1,chidg_action)
        call get_command_argument(2,filename)
        chidg_action = trim(chidg_action)
        filename = trim(filename)
        

        !
        ! Initialize ChiDG environment
        !
        call chidg%init('env')


        !
        ! Select ChiDG action
        !
        if ( trim(chidg_action) == 'edit' ) then
            call chidg_edit(trim(filename))

        else if ( trim(chidg_action) == 'convert' ) then
            call chidg_convert(trim(filename))

        else if ( trim(chidg_action) == 'post' ) then
            call chidg_post(trim(filename))

        else if ( trim(chidg_action) == 'kirchoff' ) then
            call kirchoff(filename)


        else
            call chidg_signal(FATAL,"chidg: unrecognized action '"//trim(chidg_action)//"'. Valid options are: 'edit', 'convert'")

        end if



    !
    ! ChiDG tool execution. 3 arguments.
    !
    else if ( narg == 3 ) then


        call get_command_argument(1,chidg_action)
        call get_command_argument(2,file_a)
        call get_command_argument(3,file_b)
        


        !
        ! Initialize ChiDG environment
        !
        call chidg%init('env')



        if ( trim(chidg_action) == 'interpolate' ) then
            call chidg_interpolate(trim(file_a), trim(file_b))

        else
            call chidg_signal(FATAL,"chidg: unrecognized action '"//trim(chidg_action)//"'. Valid options are: 'edit', 'convert'")

        end if







    else
        call chidg_signal(FATAL,"chidg: invalid number of arguments. Expecting (0) arguments: 'chidg'. or (2) arguments: 'chidg action file'.")
    end if








    !
    ! Close ChiDG interface
    !
    call chidg%close()




end program driver
