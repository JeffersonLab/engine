      SUBROUTINE G_keep_results(groupname,ABORT,err)
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
*
* $Log$
* Revision 1.8  1996/01/16 18:18:33  cdaq
* (JRA) Add group name to CTP calls
*
* Revision 1.7  1995/04/01 19:49:49  cdaq
* (SAW) Fix mistake in error reporting
*
* Revision 1.6  1995/01/13  18:15:39  cdaq
* (SAW) Put in a missing else that conspired with a broken thgethit (CTP) so that
* things actually worked on HPUX.  (But not Ultrix)
*
* Revision 1.5  1994/08/30  14:48:50  cdaq
* (SAW) Add call to increment scalers
*
* Revision 1.4  1994/07/21  19:51:14  cdaq
* (SAW) Add call to thgethit
*
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
      character*20 groupname
      integer ierr
*
*--------------------------------------------------------
*
      err= ' '                                  !erase any old errors
*
      ierr= thgethitg(groupname)
      ABORT= ierr.NE.0
      IF(ABORT) THEN
        call G_build_note(':failure#$ in thgethitg',
     &       '$',ierr,' ',0.,' ',err)
      else
        ierr= thtstexeg(groupname)
        call thtstinsg(groupname)                ! Increment scalers
*
        ABORT= ierr.NE.0
        IF(ABORT) THEN
          call G_build_note(':failure#$ in thtstexeg',
     &         '$',ierr,' ',0.,' ',err)
        ELSE
          ierr= thhstexeg(groupname)
          ABORT= ierr.NE.0
          If(ABORT) call G_build_note(':failure#$ in thhstexeg',
     &         '$',ierr,' ',0.,' ',err)
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
      call C_keep_results(FAIL,why)
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
