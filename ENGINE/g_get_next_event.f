      SUBROUTINE G_get_next_event(ABORT,err)
*--------------------------------------------------------
*-       Prototype C analysis routine
*-
*-
*-   Purpose and Methods : gets the CRAW (C raw data) buffer
*-                         from a FASTBUS CODA file
*-
*-   Output: ABORT      - success or failure
*-         : err        - reason for failure, if any
*- 
*-   Created  29-Oct-1993   Kevin B. Beard
*-   Modified 1-Dec-1993    KBB: borrowed L.Dennis's hall B routines
* $Log$
* Revision 1.4  1996/01/16 18:32:21  cdaq
* no change
*
* Revision 1.3  1994/04/12 18:45:53  cdaq
* (SAW) Add include for the CRAW event buffer common
*
* Revision 1.2  1994/02/11  15:43:08  cdaq
* Replace fbgen library call with plain evread call
*
* Revision 1.1  1994/02/01  20:40:55  cdaq
* Initial revision
*
*-
*- All standards are from "Proposal for Hall C Analysis Software
*- Vade Mecum, Draft 1.0" by D.F.Geesamn and S.Wood, 7 May 1993
*-
*-
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*16 here
      parameter (here= 'G_get_next_event')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'gen_data_structures.cmn'
      include 'gen_craw.cmn'
      INCLUDE 'gen_filenames.cmn'
*
      integer maxsize
      integer*4 status
      integer*4 evread                          ! Coda event read routine
*
*--------------------------------------------------------
*
      err= ' '
*
      ABORT= .NOT.g_data_source_opened
*
      IF(ABORT) THEN
*
        err= ':no data source open'
*
      ELSE                     !try to get next event
*
        maxsize= LENGTH_CRAW

        status = evread(g_data_source_in_hndl,CRAW,maxsize)

        if(status.ne.0) then
           call cemsg(status,0,err)             ! Get error string from CODA
           ABORT = .true.
        endif
      ENDIF
*
      IF(ABORT) call G_add_path(here,err)    
*
      RETURN
      END

