!>  Utility to convert a plot3d grid to an hdf5 file
!!  which is read by the ChiDG code
!!
!!  @author Nathan A. Wukie
!!  @date   4/11/2016
!!
!!
!!
!!
!--------------------------------------------------------------------------------------------
module mod_chidg_convert_p3d_hdf5
#include <messenger.h>
    use mod_kinds,          only: rk,ik, rdouble
    use mod_constants,      only: IO_DESTINATION
    use mod_hdf_utilities,  only: initialize_chidg_file_hdf, set_storage_version_major_hdf, &
                                  set_storage_version_minor_hdf, set_ndomains_hdf, set_domain_index_hdf, &
                                  set_domain_mapping_hdf, set_domain_dimensionality_hdf, set_domain_equation_set_hdf, &
                                  set_contains_grid_hdf, STORAGE_FORMAT_MAJOR, STORAGE_FORMAT_MINOR
    use hdf5
    use h5lt
    implicit none





contains


    !>  Specific routine for converting plot3d grid to hdf5
    !!
    !!  @author Nathan A. Wukie
    !!  @date   2/8/2016
    !!
    !!
    !-----------------------------------------------------------------------------------------------
    subroutine chidg_convert_p3d_hdf5(filename)
        character(*),   intent(in)  :: filename

        ! File, group vars
        character(1024)             :: file_prefix, blockgroup, blockname
        logical                     :: file_exists

        ! HDF5 vars
        integer(HID_T)              :: file_id,   Block_id,  Grid_id,  BC_id
        integer(HID_T)              :: xspace_id, yspace_id, zspace_id
        integer(HID_T)              :: xset_id,   yset_id,   zset_id
        integer(HID_T)              :: ximin_id,  ximax_id,  etamin_id,  etamax_id,  zetamin_id,  zetamax_id
        integer(HSIZE_T)            :: dims(3), adim

        ! Plot3d vars
        integer(ik)                 :: i,j,k,imax,jmax,kmax,ext_loc, fileunit
        integer(ik)                 :: igrid,nelem,nblks,mapping, spacedim
        integer(ik),    allocatable :: blkdims(:,:)
        real(rdouble),  allocatable :: xcoords(:,:,:), ycoords(:,:,:), zcoords(:,:,:)
        integer                     :: ierr

        ! equation set string
        character(len=100)          :: eqnset_string

        ! boundary condition types
        integer(ik)                 :: ximin_bc, ximax_bc, etamin_bc, etamax_bc, zetamin_bc, zetamax_bc


        !
        ! Send output to screen, not file.
        !
        IO_DESTINATION = 'screen'



        !
        ! Check if input file exists
        !
        inquire(file=filename, exist=file_exists)
        if (file_exists) then
            call write_line("Found "//trim(filename))
            ext_loc = index(filename,'.')           ! get location of extension
            file_prefix = filename(1:(ext_loc-1))   ! save file prefix w/o extension
        else
            call chidg_signal_one(FATAL,"File not found.",filename)
        end if



        !
        ! Initialize HDF5
        !
        call h5open_f(ierr)
        if (ierr /= 0) call chidg_signal(FATAL,"Error: h5open_f")


        !
        ! Create base ChiDG file and get file identifier
        !
        file_id = initialize_chidg_file_hdf(file_prefix)


        !
        ! Add file major.minor version numbers as attributes
        !
        call set_storage_version_major_hdf(file_id,STORAGE_FORMAT_MAJOR)
        call set_storage_version_minor_hdf(file_id,STORAGE_FORMAT_MINOR)


        !
        ! Read plot3d grid
        !
        open(newunit=fileunit, file=trim(filename), form='unformatted')
        read(fileunit) nblks
        call write_line(nblks," grid blocks", delimiter=" ")


        !
        ! Add number of grid domains as attribute
        !
        call set_ndomains_hdf(file_id,nblks)


        !
        ! Make space for storing dimensions of each block domain
        !
        allocate(blkdims(3,nblks),stat=ierr)
        if (ierr /= 0) call AllocationError

        !
        ! read index dimensions for each block
        !
        read(fileunit) (blkdims(1,igrid), blkdims(2,igrid), blkdims(3,igrid), igrid=1,nblks)



        !
        ! Loop through grid domain and for each domain, create an HDF5 group (D_$BLOCKNAME)
        !
        do igrid = 1,nblks

            ! Read spacedim from user
            spacedim = 0
            do while ( (spacedim < 2) .or. (spacedim > 3) )
                call write_line("Enter number of spatial dimensions for block: ", igrid, delimiter=" ")
                call write_line("Key -- ( 2 = 2D, 3 = 3D )")
                read*, spacedim
            end do


            ! Read mapping from user
            call write_line("Enter mapping for block: ", igrid, delimiter=" ")
            call write_line("Key -- (1 = linear, 2 = quadratic, 3 = cubic, 4 = quartic, 5 = quintic, 6 = sextic, 7 = septic )")
            read*, mapping


            !
            ! Dimensions for reading plot3d grid
            !
            imax = blkdims(1,igrid)
            jmax = blkdims(2,igrid)
            kmax = blkdims(3,igrid)


            !
            ! Dimensions for writing HDF5 grid
            !
            dims = [blkdims(1,igrid), blkdims(2,igrid), blkdims(3,igrid)]

            allocate(xcoords(imax,jmax,kmax),ycoords(imax,jmax,kmax),zcoords(imax,jmax,kmax),stat=ierr)
            if (ierr /= 0) stop "memory allocation error: plot3d_to_hdf5"

            read(fileunit) ((( xcoords(i,j,k), i=1,imax), j=1,jmax), k=1,kmax), &
                           ((( ycoords(i,j,k), i=1,imax), j=1,jmax), k=1,kmax), &
                           ((( zcoords(i,j,k), i=1,imax), j=1,jmax), k=1,kmax)

            !
            ! Create a domain-group for the current block domain
            !
            write(blockname, '(I2.2)') igrid
            blockgroup = "D_"//trim(blockname)
            call h5gcreate_f(file_id, trim(blockgroup), Block_id, ierr)
            if (ierr /= 0) stop "Error: h5gcreate_f"

            !
            ! Write domain attributes
            !
            call set_domain_index_hdf(block_id,igrid)
            call set_domain_mapping_hdf(block_id,mapping)
            call set_domain_dimensionality_hdf(block_id, spacedim)


            !
            ! Create a grid-group within the current block domain
            !
            call h5gcreate_f(Block_id, "Grid", Grid_id, ierr)
            if (ierr /= 0) stop "Error: h5gcreate_f"


            !
            ! Create dataspaces for grid coordinates
            !
            call h5screate_simple_f(3, dims, xspace_id, ierr)
            if (ierr /= 0) stop "Error: h5screate_simple_f"
            call h5screate_simple_f(3, dims, yspace_id, ierr)
            if (ierr /= 0) stop "Error: h5screate_simple_f"
            call h5screate_simple_f(3, dims, zspace_id, ierr)
            if (ierr /= 0) stop "Error: h5screate_simple_f"


            !
            ! Create datasets for grid coordinates
            !
            call h5dcreate_f(Grid_id, 'CoordinateX', H5T_NATIVE_DOUBLE, xspace_id, xset_id, ierr)
            if (ierr /= 0) stop "Error: h5dcreate_f"
            call h5dcreate_f(Grid_id, 'CoordinateY', H5T_NATIVE_DOUBLE, yspace_id, yset_id, ierr)
            if (ierr /= 0) stop "Error: h5dcreate_f"
            call h5dcreate_f(Grid_id, 'CoordinateZ', H5T_NATIVE_DOUBLE, zspace_id, zset_id, ierr)
            if (ierr /= 0) stop "Error: h5dcreate_f"


            !
            ! Write coordinates to datasets
            !
            call h5dwrite_f(xset_id, H5T_NATIVE_DOUBLE, xcoords, dims, ierr)
            if (ierr /= 0) stop "Error: h5dwrite_f"
            call h5dwrite_f(yset_id, H5T_NATIVE_DOUBLE, ycoords, dims, ierr)
            if (ierr /= 0) stop "Error: h5dwrite_f"
            call h5dwrite_f(zset_id, H5T_NATIVE_DOUBLE, zcoords, dims, ierr)
            if (ierr /= 0) stop "Error: h5dwrite_f"


            !
            ! Read equationset from user
            !
            call write_line("Setting equation set for domain ", igrid, delimiter=" ")
            call write_line("Enter equation set: ")
            read*, eqnset_string


            !
            ! Write equationset attribute
            !
            call set_domain_equation_set_hdf(Block_id,trim(eqnset_string))


            !
            ! Create a boundary condition-group within the current block domain
            !
            call h5gcreate_f(Block_id, "BoundaryConditions", BC_id, ierr)
            if (ierr /= 0) stop "Error: h5gcreate_f"


            !
            ! Create empty groups for boundary conditions
            !
            call h5gcreate_f(BC_id,"XI_MIN",ximin_id, ierr)
            if (ierr /= 0) stop "Error: h5gcreate_f"
            call h5gcreate_f(BC_id,"XI_MAX",ximax_id, ierr)
            if (ierr /= 0) stop "Error: h5gcreate_f"
            call h5gcreate_f(BC_id,"ETA_MIN",etamin_id, ierr)
            if (ierr /= 0) stop "Error: h5gcreate_f"
            call h5gcreate_f(BC_id,"ETA_MAX",etamax_id, ierr)
            if (ierr /= 0) stop "Error: h5gcreate_f"
            call h5gcreate_f(BC_id,"ZETA_MIN",zetamin_id, ierr)
            if (ierr /= 0) stop "Error: h5gcreate_f"
            call h5gcreate_f(BC_id,"ZETA_MAX",zetamax_id, ierr)
            if (ierr /= 0) stop "Error: h5gcreate_f"



            !
            ! Close boundary condition groups
            !
            call h5gclose_f(ximin_id,   ierr) 
            call h5gclose_f(ximax_id,   ierr) 
            call h5gclose_f(etamin_id,  ierr) 
            call h5gclose_f(etamax_id,  ierr) 
            call h5gclose_f(zetamin_id, ierr) 
            call h5gclose_f(zetamax_id, ierr) 


            !
            ! Close datasets
            !
            call h5dclose_f(xset_id,ierr)
            if (ierr /= 0) stop "Error: h5dclose_f"
            call h5dclose_f(yset_id,ierr)
            if (ierr /= 0) stop "Error: h5dclose_f"
            call h5dclose_f(zset_id,ierr)
            if (ierr /= 0) stop "Error: h5dclose_f"


            !
            ! Close dataspaces
            !
            call h5sclose_f(xspace_id,ierr)
            if (ierr /= 0) stop "Error: h5sclose_f"
            call h5sclose_f(yspace_id,ierr)
            if (ierr /= 0) stop "Error: h5sclose_f"
            call h5sclose_f(zspace_id,ierr)
            if (ierr /= 0) stop "Error: h5sclose_f"


            !
            ! Close active groups
            !
            call h5gclose_f(Grid_id, ierr)
            if (ierr /= 0) stop "Error: h5gclose_f"
            call h5gclose_f(BC_id, ierr)
            if (ierr /= 0) stop "Error: h5gclose_f"
            call h5gclose_f(Block_id, ierr)
            if (ierr /= 0) stop "Error: h5gclose_f"


            deallocate(zcoords,ycoords,xcoords)
        end do




        !
        ! Add grid/solution attributes. Indicating the file contains a grid, and no solution.
        !
        call set_contains_grid_hdf(file_id,"True")




        !
        ! Close files and interfaces
        !
        close(fileunit)                 ! Close plot3d file
        call h5fclose_f(file_id,ierr)   ! Close hdf5 file
        call h5close_f(ierr)            ! Close hdf5 interface


        
        !
        ! Exit message
        !
        call write_line("Saved ", trim(file_prefix)//'.h5', delimiter=" ")


    end subroutine chidg_convert_p3d_hdf5
    !*****************************************************************************************











end module mod_chidg_convert_p3d_hdf5
