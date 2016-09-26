module mod_file_utilities
#include <messenger.h>
    use mod_string_utilities,   only: get_file_extension
    use type_file_properties,   only: file_properties_t
    implicit none




contains


    !>  Copy source file to target file.
    !!
    !!  @author Nathan A. Wukie
    !!  @date   1/28/2016
    !!
    !!
    !!
    !!
    !---------------------------------------------------------------------------------------------------------
    subroutine copy_file(sourcefile, targetfile)
        character(*),   intent(in)  :: sourcefile
        character(*),   intent(in)  :: targetfile

        integer             :: unit_src, unit_tar, ierr, irec
        character(len=1)    :: c

        
        !
        ! Open files for source and target.
        !
        open(newunit=unit_tar, file=targetfile, access='stream', action='write', iostat=ierr)
        if ( ierr /= 0 ) call chidg_signal(FATAL,"copy_file: error opening target file.")

        open(newunit=unit_src, file=sourcefile, access='stream', action='read',  iostat=ierr)
        if ( ierr /= 0 ) call chidg_signal(FATAL,"copy_file: error opening source file.")



        !
        ! While there is a character coming from the source file, get character write to target file
        !
        ierr = 0
        do while ( ierr == 0 )

            !
            ! Get from source file
            !
            read(unit=unit_src, iostat=ierr) c

            !
            ! Write to target file
            !
            if ( ierr == 0 ) then
                write(unit_tar) c
            end if

        end do




        !
        ! Close files
        !
        close(unit_src)
        close(unit_tar)



    end subroutine copy_file
    !*************************************************************************************************************














    !>  Delete file.
    !!
    !!  @author Nathan A. Wukie
    !!  @date   1/28/2016
    !!
    !!
    !!
    !!
    !-------------------------------------------------------------------------------------------------------------
    subroutine delete_file(sourcefile)
        character(*),   intent(in)  :: sourcefile

        integer     :: unit_src, ierr, irec
        
        !
        ! Open source file for deletion.
        !
        open(newunit=unit_src, file=sourcefile, status='old', iostat=ierr)

        !
        ! Close and delete.
        !
        if (ierr == 0) close(unit_src, status='delete')
        

    end subroutine delete_file
    !*************************************************************************************************************











end module mod_file_utilities
