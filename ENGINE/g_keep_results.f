      SUBROUTINE G_keep_results(ABORT,err)
*----------------------------------------------------------------------
*-       Prototype hall C keep_results routine
*- 
*-   Purpose and Methods : Given previously filled data structures,
*-                         keep_results stores the reconstructed info.
*- 
*-   Output: ABORT	- success or failure
*-         : err	- reason for failure, if any
*- 
*-   Created  20-Nov-1993   Kevin B. Beard, HU
*-    $Log$
*-    Revision 1.4  1994/07/21 19:51:14  cdaq
*-    (SAW) Add call to thgethit
*-
* Revision 1.3  1994/06/17  03:50:36  cdaq
* (KBB) Upgrade error reporting
*
* Revision 1.2  1994/04/15  20:35:29  cdaq
* (KBB) Add calls to thtstexe and thhstexe
*
* Revision 1.1  1994/02/04  22:10:48  cdaq
* Initial revision
*
*-
*- All standards are from "Proposal for Hall C Analysis Software
*- Vade Mecum, Draft 1.0" by D.F.Geesamn and S.Wood, 7 May 1993
*-
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*40 here
      parameter (here= 'G_keep_results')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'gen_data_structures.cmn'
      include 'gen_routines.dec'
*
      logical FAIL
      character*1024 why
      character*80 msg
      integer ierr
*
*--------------------------------------------------------
*
      err= ' '                                  !erase any old errors
*
      ierr= thgethit()
      ABORT= ierr.NE.0
      IF(ABORT) THEN
         call G_build_note(':failure#$ in thgethit',
     &                                 '$',ierr,' ',0.,' ',err)
         ierr= thtstexe()
         ABORT= ierr.NE.0
         IF(ABORT) THEN
            call G_build_note(':failure#$ in thtstexe',
     &           '$',ierr,' ',0.,' ',err)
         ELSE
            ierr= thhstexe()
            ABORT= ierr.NE.0
            If(ABORT) call G_build_note(':failure#$ in thhstexe',
     &           '$',ierr,' ',0.,' ',err)
         ENDIF
      ENDIF
*
*-HMS 
      call H_keep_results(FAIL,why)
      IF(err.NE.' ' .and. why.NE.' ') THEN
        call G_append(err,' & '//why)
      ELSEIF(why.NE.' ') THEN
        err= why
      ENDIF
      ABORT= ABORT .or. FAIL
*
*-SOS 
      call S_keep_results(FAIL,why)
      IF(err.NE.' ' .and. why.NE.' ') THEN
        call G_append(err,' & '//why)
      ELSEIF(why.NE.' ') THEN
        err= why
      ENDIF
      ABORT= ABORT .or. FAIL
*
*-COIN
      call C_keep_results(ABORT,err)
      IF(err.NE.' ' .and. why.NE.' ') THEN
        call G_append(err,' & '//why)
      ELSEIF(why.NE.' ') THEN
        err= why
      ENDIF
      ABORT= ABORT .or. FAIL
*
      IF(ABORT .or. err.NE.' ') call G_add_path(here,err)
*
      RETURN
      END
