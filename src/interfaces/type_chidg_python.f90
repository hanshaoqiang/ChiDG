module type_chidg
    implicit none





    !>  The ChiDG Environment container
    !!
    !!      - Contains an array of domains, a time advancement scheme, a nonlinear solver, a linear solver, and a preconditioner
    !!
    !!  @author Nathan A. Wukie
    !!  @date   2/1/2016
    !!
    !--------------------------------------------------------------------------------------------------------
    type, public    :: chidg_t


    end type chidg_t
    !*********************************************************************************************************



contains



    !> ChiDG environment initialization routine
    !!      - Call initiailization procedures for equations, grid data, reading input
    !!      - chidg%init('env') should be called before any activity with ChiDG is begun.
    !!
    !!  @author Nathan A. Wukie
    !!  @date   2/1/2016
    !!
    !!  @param[in]  level   Initialization level specification. 'env', 'io', or 'finalize'
    !!
    !--------------------------------------------------------------------------------------------
    subroutine init(self,level)
        type(chidg_t),  intent(inout)   :: self
        character(*),   intent(in)      :: level


    end subroutine init
    !**********************************************************************************************************




    !>  Set ChiDG environment components
    !!
    !!      -   Set time-scheme
    !!      -   Set nonlinear solver
    !!      -   Set linear solver
    !!      -   Set preconditioner
    !!      -   Set number of allocated domains (default=1)
    !!
    !!  @author Nathan A. Wukie
    !!  @date   2/1/2016
    !!
    !!  @param[in]      selector    Character string for selecting the chidg component for initialization
    !!  @param[in]      selection   Character string for specializing the component being initialized
    !!  @param[inout]   options     Dictionary for initialization options
    !!
    !--------------------------------------------------------------------------------------------------------
    subroutine set(self,selector,selection,options)
        type(chidg_t),          intent(inout)   :: self
        character(*),           intent(in)      :: selector
        character(*),           intent(in)      :: selection
        type(dict_t),           intent(inout)   :: options 


    end subroutine set
    !********************************************************************************************************











    !>  Read grid from file.
    !!
    !!  @author Nathan A. Wukie
    !!  @date   2/1/2016
    !!
    !!  @param[in]  gridfile    String containing a grid file name, including extension.
    !!  @param[in]  spacedim    Number of spatial dimensions
    !!
    !!  TODO: Generalize spacedim
    !!
    !----------------------------------------------------------------------------------------------
    subroutine read_grid(self,gridfile,spacedim)
        type(chidg_t),     intent(inout)   :: self
        character(*),       intent(in)      :: gridfile
        integer(ik),        intent(in)      :: spacedim


    end subroutine read_grid
    !*************************************************************************************************************











    !>  Read boundary conditions from grid file.
    !!
    !!  @author Nathan A. Wukie
    !!  @date   2/5/2016
    !!
    !!  @param[in]  gridfile    String specifying a gridfile, including extension.
    !!
    !-------------------------------------------------------------------------------------------------------------
    subroutine read_boundaryconditions(self, gridfile)
        type(chidg_t),  intent(inout)   :: self
        character(*),   intent(in)      :: gridfile


    end subroutine read_boundaryconditions
    !**************************************************************************************************************






    !>  Read solution from file.
    !!
    !!  @author Nathan A. Wukie
    !!  @date   2/1/2016
    !!
    !!  @param[in]  solutionfile    String containing a solution file name, including extension.
    !!
    !------------------------------------------------------------------------------------------------------------
    subroutine read_solution(self,solutionfile)
        type(chidg_t),  intent(inout)   :: self
        character(*),   intent(in)      :: solutionfile


    end subroutine read_solution
    !************************************************************************************************************



    

    !>  Initialize all solution and solver storage.
    !!
    !!  @author Nathan A. Wukie
    !!  @date   4/11/2016
    !!
    !!  @param[in]  nterms_s    Number of terms in the solution polynomial expansion.
    !!
    !------------------------------------------------------------------------------------------------------------
    subroutine initialize_solution_domains(self,nterms_s)
        type(chidg_t),  intent(inout)   :: self
        integer(ik),    intent(in)      :: nterms_s


    end subroutine initialize_solution_domains
    !************************************************************************************************************






    !>  Initialize all solution and solver storage.
    !!
    !!  @author Nathan A. Wukie
    !!  @date   4/11/2016
    !!
    !!  @param[in]  nterms_s    Number of terms in the solution polynomial expansion.
    !!
    !------------------------------------------------------------------------------------------------------------
    subroutine initialize_solution_solver(self)
        type(chidg_t),     intent(inout)   :: self


    end subroutine initialize_solution_solver
    !************************************************************************************************************





    !> Write solution to file.
    !!
    !!  @author Nathan A. Wukie
    !!  @date   2/1/2016
    !!
    !!  @param[in]  solutionfile    String containing a solution file name, including extension.
    !!
    !------------------------------------------------------------------------------------------------------------
    subroutine write_solution(self,solutionfile)
        type(chidg_t),  intent(inout)   :: self
        character(*),   intent(in)      :: solutionfile



    end subroutine write_solution
    !************************************************************************************************************






    !>  Run ChiDG simulation
    !!
    !!      - This routine passes the domain data, nonlinear solver, linear solver, and preconditioner
    !!        components to the time scheme for iteration
    !!
    !!  @author Nathan A. Wukie
    !!  @date   2/1/2016
    !!
    !------------------------------------------------------------------------------------------------------------
    subroutine run(self)
        type(chidg_t),     intent(inout)   :: self


    end subroutine run
    !************************************************************************************************************










    !> Report on ChiDG simulation
    !!
    !!  @author Nathan A. Wukie
    !!  @date   2/1/2016
    !!
    !!
    !!
    !------------------------------------------------------------------------------------------------------------
    subroutine report(self)
        type(chidg_t), intent(inout)   :: self


    end subroutine report
    !************************************************************************************************************









    !> Any activities that need performed before the program completely terminates.
    !!
    !!  @author Nathan A. Wukie
    !!  @date   2/1/2016
    !!
    !!
    !------------------------------------------------------------------------------------------------------------
    subroutine close(self)
        type(chidg_t), intent(inout)   :: self


    end subroutine close
    !************************************************************************************************************











end module type_chidg
