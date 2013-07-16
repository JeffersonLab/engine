      subroutine s_register_variables(ABORT,err)
*----------------------------------------------------------------------
*
*     CTP variable registration routine for the SOS
*
*     Purpose : Register all variables that are to be used by CTP, that are
*     connected with the SOS.  This includes externally configured
*     parameters/contants, event data that can be a histogram source, and
*     possible test results and scalers.
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*     Created: 9-Feb-1994  Stephen A. Wood
*
* $Log: s_register_variables.f,v $
* Revision 1.11  1996/01/16 16:27:28  cdaq
* no change
*
* Revision 1.10  1995/08/11 15:41:09  cdaq
* (DD) Add sos sieve slit ntuple
*
* Revision 1.9  1995/05/22  13:32:11  cdaq
* (SAW) Add call to register sos_data_structures.cmn variables
*
* Revision 1.8  1995/05/11  18:59:39  cdaq
* (SAW) Add register call for s_ntuple.cmn
*
* Revision 1.7  1994/08/18  04:11:36  cdaq
* (SAW) Call makereg generated routines to register variables
*
* Revision 1.6  1994/06/17  03:27:31  cdaq
* (KBB) Execute all code despite registration errors
*
* Revision 1.5  1994/06/16  03:45:21  cdaq
* (SAW) Register filenames for reports
*
* Revision 1.4  1994/04/12  17:26:00  cdaq
* (KBB) Add ntuple call
*
* Revision 1.3  1994/02/22  19:39:19  cdaq
* (SAW) Remove CTP register calls to fortran PARAMETER's
*
* Revision 1.2  1994/02/22  18:58:00  cdaq
* (SAW) Make a call to h_register_param
*
* Revision 1.1  1994/02/11  04:18:56  cdaq
* Initial revision
*
*
*----------------------------------------------------------------------
      implicit none
      save
*
      character*20 here
      parameter (here='s_register_variables')
*
      logical ABORT
      character*(*) err
*
      logical FAIL
      character*1000 why
*
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*
      call r_sos_data_structures

      call r_sos_filenames

      call r_s_ntuple

      call s_register_param(FAIL,why) ! TRACKING ROUTINE
      IF(err.NE.' ' .and. why.NE.' ') THEN   !keep warnings
        call G_append(err,' & '//why)
      ELSEIF(why.NE.' ') THEN
        err= why
      ENDIF
      ABORT= ABORT .or. FAIL
*
      call s_ntuple_register(FAIL,why)  ! Remove this when ctp files fixed
      IF(err.NE.' ' .and. why.NE.' ') THEN   !keep warnings
        call G_append(err,' & '//why)
      ELSEIF(why.NE.' ') THEN
        err= why
      ENDIF
      ABORT= ABORT .or. FAIL
*
      call s_sv_nt_register(FAIL,why)  ! Remove this when ctp files fixed
      IF(err.NE.' ' .and. why.NE.' ') THEN   !keep warnings
        call G_append(err,' & '//why)
      ELSEIF(why.NE.' ') THEN
        err= why
      ENDIF
      ABORT= ABORT .or. FAIL
*
      if(ABORT .or. err.NE.' ') call G_add_path(here,err)
*
      return
      end
