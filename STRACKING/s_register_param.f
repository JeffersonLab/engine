       SUBROUTINE s_register_param(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Initializes SOS quantities 
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
*-   Created  8-Nov-1993   Kevin B. Beard
*-   Modified 20-Nov-1993  KBB for new errors
*-            14 Feb-1994  DFG Put in real variables
*-
*- All standards are from "Proposal for Hall C Analysis Software
*- Vade Mecum, Draft 1.0" by D.F.Geesamn and S.Wood, 7 May 1993
* $Log$
* Revision 1.5  1994/03/24 19:54:54  cdaq
* (DFG) Put actual registering of variables in subroutines
*
* Revision 1.4  1994/02/23  15:39:50  cdaq
* (SAW) ABORT now when ierr.NE.0
*
* Revision 1.3  1994/02/22  20:39:28  cdaq
* (SAW) Fix booboo
*
* Revision 1.2  1994/02/22  18:52:19  cdaq
* (SAW) Move regpar declarations to gen_routines.dec.  Make title arg null.
*
* Revision 1.1  1994/02/22  18:42:21  cdaq
* Initial revision
*
*-
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*16 here
      parameter (here= 's_register_param')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'gen_data_structures.cmn'
      INCLUDE 'gen_routines.dec'
      INCLUDE 'sos_tracking.cmn'
      INCLUDE 'sos_geometry.cmn'
*
*
*--------------------------------------------------------
      err= ' '
      ABORT = .false.
*
*
*     register tracking variables
      call s_register_track_param(ABORT,err)
      IF(ABORT) THEN
         call G_add_path(here,err)
      ENDIF
*
*     register cal, tof and cer variables
      call s_register_id_param(ABORT,err)
      IF(ABORT) THEN
         call G_add_path(here,err)
      ENDIF
*
      RETURN
      END
