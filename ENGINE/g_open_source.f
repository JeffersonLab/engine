      SUBROUTINE G_open_source(ABORT,err)
*----------------------------------------------------------------------
*-       Prototype hall C open (FASTBUS) CODA file routine
*- 
*-   Purpose and Methods : Initialization is performed and status returned
*- 
*-   Output: ABORT      - success or failure
*-         : err        - reason for failure, if any
*- 
*-   Created  30-Nov-1993   Kevin B. Beard
*-    $Log$
*-    Revision 1.1  1994/02/04 22:11:29  cdaq
*-    Initial revision
*-
c
*-
*- All standards are from "Proposal for Hall C Analysis Software
*- Vade Mecum, Draft 1.0" by D.F.Geesamn and S.Wood, 7 May 1993
*-
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*40 here
      parameter (here= 'G_open_source')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'gen_filenames.cmn'
*
      logical OK
      character*8  flag           !must be declared exactly
      integer*4 status
      integer*4 evopen                          ! CODA routine
*
*--------------------------------------------------------
      err= ' '
      g_data_source_in_hndl= 0
*
      status = evopen(g_data_source_filename,'r',g_data_source_in_hndl)
      if(status.ne.0) then
*         call cemsg(status,0,err)
         g_data_source_opened = .false.
      else
         g_data_source_opened = .true.
      endif
*
      IF(.not.g_data_source_opened) THEN
        err= ':could not open "'//g_data_source_filename//'"'
        call G_add_path(here,err)
      ENDIF
*
      RETURN
      END

