       SUBROUTINE h_register_param(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Initializes HMS quantities 
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
* Revision 1.7  1994/06/17 17:46:36  cdaq
* (KBB) Upgrade error reporting
*
* Revision 1.6  1994/06/06  17:13:37  cdaq
* (DFG) add call to register bypass switches and statistics
*
* Revision 1.5  1994/03/24  19:41:33  cdaq
* (DFG) Move actual registereing of variables to subroutines
*
* Revision 1.4  1994/02/23  15:39:02  cdaq
* (SAW) ABORT now when ierr.NE.0
*
* Revision 1.3  1994/02/22  20:39:21  cdaq
* (SAW) Fix booboo
*
* Revision 1.2  1994/02/22  18:52:06  cdaq
* (SAW) Move regpar declarations to gen_routines.dec.  Make title arg null.
*
*-
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*16 here
      parameter (here= 'H_register_param')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'gen_data_structures.cmn'
      INCLUDE 'gen_routines.dec'
*
      logical FAIL
      character*1000 why
*
*--------------------------------------------------------
      err= ' '
      ABORT = .false.
*
*     register tracking variables
      call h_register_track_param(ABORT,err)
*
*     register cal, tof and cer variables
      call h_register_id_param(FAIL,why)
      IF(err.NE.' ' .and. why.NE.' ') THEN
         call G_append(err,' & '//why)
      ELSEIF(why.NE.' ') THEN
         err = why
      ENDIF
      ABORT= ABORT .or. FAIL
*
*     register bypass switches
      call h_register_bypass(FAIL,why)
      IF(err.NE.' ' .and. why.NE.' ') THEN
         call G_append(err,' & '//why)
      ELSEIF(why.NE.' ') THEN
         err = why
      ENDIF
      ABORT= ABORT .or. FAIL
*
*     register hms statistics
      call h_register_statistics(FAIL,why)
      IF(err.NE.' ' .and. why.NE.' ') THEN
         call G_append(err,' & '//why)
      ELSEIF(why.NE.' ') THEN
         err = why
      ENDIF
      ABORT= ABORT .or. FAIL
*
      IF(ABORT .or. err.NE.' ') call G_add_path(here,err)
*
      RETURN
      END
