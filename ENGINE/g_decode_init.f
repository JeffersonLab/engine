      SUBROUTINE G_decode_init(ABORT,err)
*----------------------------------------------------------------------
*-       Prototype hall C initialize routine
*- 
*-   Purpose and Methods : Initialization decoding algorithm
*- 
*-   Output: ABORT      - success or failure
*-         : err        - reason for failure, if any
*- 
*-   Created   3-Dec-1993   Kevin B. Beard
*
* $Log: g_decode_init.f,v $
* Revision 1.3  1996/01/16 20:54:59  cdaq
* no change
*
* Revision 1.2  1994/03/24 18:15:59  cdaq
* (SAW) Move g_decode_clear into this routine.
*
* Revision 1.1  1994/02/04  21:51:53  cdaq
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
      parameter (here= 'G_decode_init')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'gen_filenames.cmn'    !names of config. files
*
*--------------------------------------------------------
*
*-all crucial setup information here; failure is fatal
*
      call g_decode_clear(ABORT,err)
      if(ABORT) then
         call G_add_path(here,err)
         return
      endif
*
      call G_decode_config(ABORT,err,g_decode_map_filename)
      IF(ABORT) THEN
         call G_add_path(here,err)
      ELSE
         err= ' '
      ENDIF
*
      RETURN
      END
