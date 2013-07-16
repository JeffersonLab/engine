      SUBROUTINE g_write_event(ABORT,err)
*------------------------------------------------------------
*  prototype C analysis routine
*
*   purpose and methods : write the event in buffer to a file
*
*   output : abort: sucess or fail
*            err:   reason for failure
*
*  created  apr-30-1996 Dave Meekins
*
* $Log: g_write_event.f,v $
* Revision 1.1  1996/06/10 17:47:32  saw
* Initial revision
*
*------------------------------------------------------------

*     DECLARATIONS

      IMPLICIT NONE
      SAVE

      character*20 here
      parameter (here='g_write_event')

      logical ABORT
      character*(*) err

      INCLUDE 'gen_data_structures.cmn'
      INCLUDE 'gen_craw.cmn'
      INCLUDE 'gen_filenames.cmn'

      integer*4 status
      integer*4 evwrite          ! coda routine to write event

*------------------------------------------------------------

*        START OF CODE

      err=' '
      ABORT=.NOT.g_preproc_opened

      if(g_preproc_in_hndl.ne.0)then
        status=evwrite(g_preproc_in_hndl,craw)
        if(status.ne.0)then
          ABORT=.true.
          call cemsg(status,0,err)
        endif
      else
        err='no preprocessor output file opened'
      endif

      if(ABORT)then
        call G_add_path(here)
      endif

      RETURN
      END
