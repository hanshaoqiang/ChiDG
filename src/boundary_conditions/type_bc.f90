module type_bc
#include <messenger.h>
    use mod_kinds,              only: rk, ik
    use mod_constants,          only: XI_MIN, XI_MAX, ETA_MIN, ETA_MAX, ZETA_MIN, ZETA_MAX, BOUNDARY, CHIMERA, ORPHAN, BC_BLK, ONE, TWO
    use type_mesh,              only: mesh_t
    use type_point,             only: point_t
    use type_ivector,           only: ivector_t
    use type_solverdata,        only: solverdata_t
    use type_properties,        only: properties_t
    use type_face_info,         only: face_info_t
    use type_function_info,     only: function_info_t
    use type_bcproperty_set,    only: bcproperty_set_t
    use type_function,          only: function_t
    implicit none
    private


    ! Export class methods so they can be used by extended types in the python interface
    public :: get_nproperties, get_property_name, get_noptions, &
              get_option_key, get_option_value, get_ncoupled_elems, &
              get_name, set_name, set_fcn, set_fcn_option



    !> Abstract base-type for boundary conditions
    !!  - contains a list of associated element indices
    !!  - contains a list of face indices
    !!
    !!  @author Nathan A. Wukie
    !!  @date   2/3/2016
    !!
    !--------------------------------------------------------------------------------------------
    type, public :: bc_t

        character(len=:),   allocatable :: name
        logical,    public              :: isInitialized = .false.  !< Logical switch for indicating the boundary condition initializaiton status

        !
        ! Boundary condition geometry
        !
        integer(ik),        allocatable :: dom(:)                   !< Indices of domains
        integer(ik),        allocatable :: elems(:)                 !< Indices of elements associated with boundary condition
        integer(ik),        allocatable :: faces(:)                 !< Indices of the boundary face for elements elems(ielems)
        type(ivector_t),    allocatable :: coupled_elems(:)         !< For each element on the boundary, a vector of element block-indices coupled with the current element.


        !
        ! Boundary condition options
        !
        !type(bcparameter_set_t)    :: bcparameters
        type(bcproperty_set_t)      :: bcproperties

    contains



        procedure   :: init                     !< Boundary condition initialization
        procedure   :: init_spec                !< Call specialized initialization routine
        procedure   :: init_boundary_coupling   !< Initialize book-keeping for coupling interaction between elements.
        procedure   :: compute                  !< Implements boundary condition function
        procedure   :: apply                    !< Apply bc function over bc elements
        !procedure(compute_interface), deferred  :: compute                  !< Implements boundary condition function



        procedure   :: set_name                !< Set the boundary condition name
        procedure   :: get_name                !< Return the boundary condition name

        
        procedure   :: add_options             !< Specialized by each bc_t implementation. Adds options available

        procedure   :: set_fcn                 !< Set a particular function definition for a specified bcfunction_t
        procedure   :: set_fcn_option          !< Set function-specific options for a specified bcfunction_t


        procedure   :: get_nproperties         !< Return the number of properties associated with the boundary condition.
        procedure   :: get_property_name       !< Return the name of a property given a property index.

        procedure   :: get_noptions            !< Return the number of available options for a given property, specified by a property index.
        procedure   :: get_option_key          !< Return the key for an option, given a property index and subsequent option index.
        procedure   :: get_option_value        !< Return the value of a given key, inside of a specified property.

        procedure   :: get_ncoupled_elems      !< Return the number of elements coupled with a specified boundary element.

    end type bc_t
    !*********************************************************************************************



!    abstract interface
!        subroutine compute_interface(self,mesh,sdata,prop,face,flux)
!            use mod_kinds,  only: ik
!            import bc_t
!            import mesh_t
!            import solverdata_t
!            import properties_t
!            import face_info_t
!            import function_info_t
!
!            class(bc_t),            intent(inout)   :: self
!            type(mesh_t),           intent(in)      :: mesh(:)
!            type(solverdata_t),     intent(inout)   :: sdata
!            class(properties_t),    intent(inout)   :: prop
!            type(face_info_t),      intent(in)      :: face
!            type(function_info_t),  intent(in)      :: flux
!        end subroutine
!    end interface



contains

    !> Initialize boundary condition routine
    !!
    !!  @author Nathan A. Wukie
    !!  @date   1/31/2016
    !!
    !!  @param[in]  mesh    mesh_t object containing elements and faces
    !!  @param[in]  iface   block face index to which the boundary condition is being applied
    !!
    !------------------------------------------------------------------------------------------
    subroutine init(self,mesh,iface)
        class(bc_t),            intent(inout)       :: self
        type(mesh_t),           intent(inout)       :: mesh
        integer(ik),            intent(in)          :: iface 

        type(point_t)                   :: pnt
        character(len=:),   allocatable :: bcname        
        real(rk)                        :: time
        integer(ik)                     :: nelem_xi, nelem_eta, nelem_zeta, nelem_bc, ielem_bc, & 
                                           xi_begin, eta_begin, zeta_begin, xi_end, eta_end, zeta_end, & 
                                           ixi, ieta, izeta, ierr, ielem, ielem_test
        
        nelem_xi   = mesh%nelem_xi
        nelem_eta  = mesh%nelem_eta
        nelem_zeta = mesh%nelem_zeta

        xi_begin   = 1
        eta_begin  = 1
        zeta_begin = 1

        xi_end   = nelem_xi
        eta_end  = nelem_eta
        zeta_end = nelem_zeta


        !
        ! Compute number of elements associated with the boundary condition
        ! Constrain index ranges for a particular face on the block
        !
        select case (iface)
            case (XI_MIN)                           ! XI_MIN constant
                nelem_bc = nelem_eta * nelem_zeta
                xi_end = 1
            case (XI_MAX)                           ! XI_MAX constant
                nelem_bc = nelem_eta * nelem_zeta
                xi_begin = nelem_xi
            case (ETA_MIN)                          ! ETA_MIN constant
                nelem_bc = nelem_xi * nelem_zeta
                eta_end = 1
            case (ETA_MAX)                          ! ETA_MAX constant
                nelem_bc = nelem_xi * nelem_zeta
                eta_begin = nelem_eta
            case (ZETA_MIN)                         ! ZETA_MIN constant
                nelem_bc = nelem_xi * nelem_eta
                zeta_end = 1
            case (ZETA_MAX)                         ! ZETA_MAX constant
                nelem_bc = nelem_xi * nelem_eta
                zeta_begin = nelem_zeta
            case default
                call chidg_signal(FATAL,"bc%init: Invalid block face 'iface'. Valid face indices are iface = [1-6]")
        end select


        !
        ! Allocate storage for element and face indices
        !
        allocate(self%dom(nelem_bc), self%elems(nelem_bc), self%faces(nelem_bc), self%coupled_elems(nelem_bc), stat=ierr)
        if (ierr /= 0) call AllocationError


        ielem_bc = 1
        !
        ! Loop over a face of the block and store element indices
        !
        do izeta = zeta_begin,zeta_end
            do ieta = eta_begin,eta_end
                do ixi = xi_begin,xi_end
                    ielem = ixi + nelem_xi*(ieta-1) + nelem_xi*nelem_eta*(izeta-1)

                    self%dom(ielem_bc)   = mesh%idomain
                    self%elems(ielem_bc) = ielem
                    self%faces(ielem_bc) = iface
                    ielem_bc = ielem_bc + 1


                    bcname = self%get_name()
                    if ( trim(bcname) == 'periodic' ) then
                        !
                        ! Set to ORPHAN face so it will be recognized as chimera in the detection process.
                        !
                        mesh%faces(ielem,iface)%ftype = ORPHAN

                        !
                        ! Set periodic offset from boundary condition to the face. To be used in detection of gq_donor.
                        !
                        if ( self%bcproperties%compute('type', time, pnt) == ONE ) then
                            mesh%faces(ielem,iface)%periodic_type = 'cartesian'
                        else if ( self%bcproperties%compute('type', time, pnt) == TWO ) then
                            mesh%faces(ielem,iface)%periodic_type = 'cylindrical'
                        end if

                        mesh%faces(ielem,iface)%chimera_offset_x     = self%bcproperties%compute('offset_x', time, pnt) ! time, pnt and do nothing here, but interface for function requires them.
                        mesh%faces(ielem,iface)%chimera_offset_y     = self%bcproperties%compute('offset_y', time, pnt)
                        mesh%faces(ielem,iface)%chimera_offset_z     = self%bcproperties%compute('offset_z', time, pnt)
                        mesh%faces(ielem,iface)%chimera_offset_theta = self%bcproperties%compute('offset_theta', time, pnt)

                    else
                        !
                        ! Set face to boundary condition face
                        !
                        mesh%faces(ielem,iface)%ftype = BOUNDARY
                    end if

                end do ! ixi
            end do ! ieta
        end do ! izeta


        !
        ! Call user-specialized boundary condition initialization
        !
        call self%init_spec(mesh,iface)


        !
        ! Call user-specialized boundary coupling initialization
        !
        call self%init_boundary_coupling(mesh,iface)



        self%isInitialized = .true. ! Set initialization confirmation

    end subroutine init
    !**********************************************************************************************
    








    !> Default specialized initialization procedure. This is called from the base bc%init procedure
    !! and can be overwritten by derived types to implement specialized initiailization details.
    !!
    !!  @author Nathan A. Wukie
    !!  @date   1/31/2016
    !!
    !!  @param[inout]   mesh        mesh_t object containing elements and faces
    !!  @param[in]      iface       block face index to which the boundary condition is being applied
    !!
    !----------------------------------------------------------------------------------------------
    subroutine init_spec(self,mesh,iface)
        class(bc_t),            intent(inout)   :: self
        type(mesh_t),           intent(inout)   :: mesh
        integer(ik),            intent(in)      :: iface




    end subroutine init_spec
    !**********************************************************************************************









    !>  Default boundary coupling initialization routine. 
    !!
    !!  Default initializes coupling for a given element to just itself and no coupling with 
    !!  other elements on the boundary. For a boundary condition that is coupled across the face
    !!  this routine can be overwritten to set the coupling information specific to the boundary 
    !!  condition.
    !!  
    !!
    !!  @author Nathan A. Wukie
    !!  @date   2/16/2016
    !!
    !----------------------------------------------------------------------------------------------
    subroutine init_boundary_coupling(self,mesh,iface)
        class(bc_t),    intent(inout)   :: self
        type(mesh_t),   intent(in)      :: mesh
        integer(ik),    intent(in)      :: iface

        integer(ik) :: ielem_bc, ielem



        !
        ! Loop through elements and set default coupling information
        !
        do ielem_bc = 1,size(self%elems)


            !
            ! Get block-element index of current ielem_bc
            !
            ielem = self%elems(ielem_bc)

            
            !
            ! Add the element index as the only dependency.
            !
            call self%coupled_elems(ielem_bc)%push_back(ielem)


        end do ! ielem_bc


    end subroutine init_boundary_coupling
    !**********************************************************************************************









    !>  Function for returning the number of elements coupled with a specified boundary element.
    !!
    !!  @author Nathan A. Wukie
    !!  @date   2/16/2016
    !!
    !!
    !!
    !----------------------------------------------------------------------------------------------
    function get_ncoupled_elems(self,ielem) result(ncoupled_elems)
        class(bc_t),    intent(inout)   :: self
        integer(ik),    intent(in)      :: ielem

        integer(ik) :: ncoupled_elems


        ncoupled_elems = self%coupled_elems(ielem)%size()


    end function get_ncoupled_elems
    !**********************************************************************************************
















    !>  Apply boundary condition to the mesh and solution
    !!      - Loops through the associated elements(faces) and calls the specialized bc_t%compute
    !!        procedure for computing the rhs and linearization.
    !!
    !!
    !!  @author Nathan A. Wukie
    !!  @date   1/31/2016
    !!
    !!  @param[in]      mesh    mesh_t defining elements and faces
    !!  @param[inout]   sdata   solverdata_t containing solution, rhs, and linearization(lin) data
    !!  @param[in]      iblk    Block of the linearization for the current element that is being computed (XI_MIN, XI_MAX, eta.)
    !!  @param[inout]   prop    properties_t object containing equationset properties and material_t objects
    !!
    !---------------------------------------------------------------------------------------------
    subroutine apply(self,mesh,sdata,prop,idom)
        class(bc_t),            intent(inout)   :: self
        type(mesh_t),           intent(in)      :: mesh(:)
        class(solverdata_t),    intent(inout)   :: sdata
        class(properties_t),    intent(inout)   :: prop
        integer(ik),            intent(in)      :: idom

        integer(ik) :: ielem_bc, ielem, iface, idonor, iflux, icoupled_elem, ncoupled_elems

        type(face_info_t)       :: face
        type(function_info_t)   :: flux

        !
        ! Loop through associated boundary condition elements and call compute routine for the boundary flux calculation
        !
        do ielem_bc = 1,size(self%elems)
            ielem  = self%elems(ielem_bc)   ! Get index of the element being operated on
            iface  = self%faces(ielem_bc)   ! Get face index of element 'ielem' that is being operated on


            face%idomain  = idom
            face%ielement = ielem
            face%iface    = iface

            flux%ifcn     = 0       ! Boundary conditions are not tracked.
            flux%idonor   = 0       ! Chimera interface not applicable on boundary condition.
            flux%iblk     = BC_BLK  ! Indicates to storage routine in LHS to store in BC section.

            
            !
            ! For current element, get number of coupled elements.
            !
            ncoupled_elems = self%get_ncoupled_elems(ielem_bc)


            !
            ! Compute current element function enough times to linearize all the coupled elements.
            ! If no coupling accross the face, the ncoupled_elems=1 for just the local interior element.
            !
            do icoupled_elem = 1,ncoupled_elems

                !
                ! Get coupled element to linearize against.
                !
                face%seed%idom  = idom
                face%seed%ielem = self%coupled_elems(ielem_bc)%at(icoupled_elem)

                !
                ! For the current boundary element(face), call specialized compute procedure.
                !
                call self%compute(mesh,sdata,prop,face,flux)

            end do !ielem_c


        end do !ielem_bc


    end subroutine apply
    !********************************************************************************************




















    !> Default options initialization procedure. This is called at the creation of a boundary condition
    !! in create_bc to set the options of a concrete bc_t. This function can be overwritten by a concrete
    !! bc_t to set case-specific options; parameters and functions.
    !!
    !!      - add entries to self%bcfunctions
    !!      - add entries to self%bcparameters
    !!
    !!  @author Nathan A. Wukie
    !!  @date   2/3/2016
    !!
    !!
    !--------------------------------------------------------------------------------------------
    subroutine add_options(self)
        class(bc_t),            intent(inout)   :: self




    end subroutine add_options
    !********************************************************************************************










    !>  Set a function for a specified property.
    !!
    !!  @author Nathan A. Wukie
    !!  @date   2/3/2016
    !!
    !!  @param[in]  bcprop  String specifying a bcproperty_t to edit.
    !!  @param[in]  fcn     String specifying the concrete function_t to set.
    !!
    !--------------------------------------------------------------------------------------------
    subroutine set_fcn(self,bcprop,fcn)
        class(bc_t),            intent(inout)   :: self
        character(*),           intent(in)      :: bcprop
        character(*),           intent(in)      :: fcn


        call self%bcproperties%set_fcn(bcprop,fcn)


    end subroutine set_fcn
    !*********************************************************************************************









    !>  Set a function option for a specified property.
    !!
    !!  @author Nathan A. Wukie
    !!  @date   2/3/2016
    !!
    !!  @param[in]  bcprop  String specifying a bcproperty_t to edit.
    !!  @param[in]  option  String specifying a particular option within bcproperty_f%fcn to edit
    !!  @param[in]  val     Real value to be set for the option.
    !!
    !-----------------------------------------------------------------------------------------------
    subroutine set_fcn_option(self,bcprop,option,val)
        class(bc_t),            intent(inout)   :: self
        character(*),           intent(in)      :: bcprop
        character(*),           intent(in)      :: option
        real(rk),               intent(in)      :: val

        call self%bcproperties%set_fcn_option(bcprop,option,val)

    end subroutine set_fcn_option
    !************************************************************************************************








    !>  Return number of properties available in the boundary condition.
    !!
    !!  @author Nathan A. Wukie
    !!  @date   2/4/2016
    !!
    !!
    !!
    !---------------------------------------------------------------------------------------------------
    function get_nproperties(self) result(nprop)
        class(bc_t),    intent(in)  :: self

        integer(ik) :: nprop

        nprop = self%bcproperties%get_nproperties()

    end function get_nproperties
    !***************************************************************************************************








    !>  Return a property name string, given the index of the property in the boundary condition.
    !!
    !!  This probably works best by first calling get_nproperties, and then iterating through the 
    !!  number of available properties to get their names.
    !!
    !!  @author Nathan A. Wukie
    !!  @date   2/4/2016
    !!
    !!  @param[in]  iprop   Integer specifying the index of the property to be queried.
    !!  @result     pname   String of the property name associated with the index.
    !!
    !---------------------------------------------------------------------------------------------------
    function get_property_name(self,iprop) result(pname)
        class(bc_t),    intent(in)  :: self
        integer(ik),    intent(in)  :: iprop

        character(len=:),   allocatable :: pname

        pname = self%bcproperties%get_property_name(iprop) 

    end function get_property_name
    !***************************************************************************************************












    !>  Return an option key, given a property index and option index. 
    !!
    !!  One probably calls get_noptions(iprop)
    !!  first, to get the number of available options for the function currently set for the property 'iprop'.
    !!  Then one can loop over the number of available options and return their availble names dynamically.
    !!
    !!  @author Nathan A. Wukie
    !!  @date   2/4/2016
    !!
    !!
    !!  @param[in]  iprop       Integer index of a property to modify.
    !!  @param[in]  ioption     Integer index of an option inside bcproperty%fcn
    !!  @result     key         String(key) corresponding to the option index (ioption)
    !!
    !----------------------------------------------------------------------------------------------------
    function get_option_key(self,iprop,ioption) result(key)
        class(bc_t),    intent(inout)   :: self
        integer(ik),    intent(in)      :: iprop
        integer(ik),    intent(in)      :: ioption

        character(len=:),   allocatable :: key

        key = self%bcproperties%bcprop(iprop)%get_option_key(ioption)

    end function get_option_key
    !****************************************************************************************************










    !>  Return an option value, given a property index and option key.
    !!
    !!  @author Nathan A. Wukie
    !!  @date   2/4/2016
    !!
    !!  @param[in]  iprop       Integer index of a property to modify.
    !!  @param[in]  key         String(key) specifying the option to be queried.
    !!  @result     val         Returned value of the selected key.
    !!
    !!
    !----------------------------------------------------------------------------------------------------
    function get_option_value(self,iprop,key) result(val)
        class(bc_t),    intent(inout)  :: self
        integer(ik),    intent(in)  :: iprop
        character(*),   intent(in)  :: key

        real(rk)        :: val

        val = self%bcproperties%bcprop(iprop)%get_option_value(key)

    end function get_option_value
    !****************************************************************************************************









    !>  Return the number of available options, given a property index.
    !!
    !!  @author Nathan A. Wukie
    !!  @date   2/4/2016
    !!
    !!  @param[in]  iprop       Integer index of a property to query.
    !!  @result     noption     Returned number of options available for the property. Dependends on 
    !!                          the function that is set for the property.
    !!
    !---------------------------------------------------------------------------------------------------
    function get_noptions(self,iprop) result(noptions)
        class(bc_t),    intent(inout)  :: self
        integer(ik),    intent(in)  :: iprop

        integer(ik)     :: noptions

        noptions = self%bcproperties%bcprop(iprop)%get_noptions()

    end function get_noptions
    !***************************************************************************************************





    !>  Set the boundary condition name.
    !!
    !!  @author Nathan A. Wukie
    !!  @date   2/8/2016
    !!
    !!
    !---------------------------------------------------------------------------------------------------
    subroutine set_name(self,bcname)
        class(bc_t),    intent(inout)   :: self
        character(*),   intent(in)      :: bcname

        self%name = trim(bcname)


    end subroutine set_name
    !***************************************************************************************************








    !>  Return the boundary condition name.
    !!
    !!  @author Nathan A. Wukie
    !!  @date   2/8/2016
    !!
    !!
    !--------------------------------------------------------------------------------------------------
    function get_name(self) result(bcname)
        class(bc_t),    intent(in)  :: self

        character(len=:), allocatable :: bcname

        bcname = self%name

    end function get_name
    !***************************************************************************************************



    subroutine compute(self,mesh,sdata,prop,face,flux)
        class(bc_t),            intent(inout)   :: self
        type(mesh_t),           intent(in)      :: mesh(:)
        type(solverdata_t),     intent(inout)   :: sdata
        class(properties_t),    intent(inout)   :: prop
        type(face_info_t),      intent(in)      :: face
        type(function_info_t),  intent(in)      :: flux


    end subroutine compute


end module type_bc
