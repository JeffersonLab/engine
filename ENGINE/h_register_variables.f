      subroutine h_register_variables(ABORT,err)
*----------------------------------------------------------------------
*
*     CTP variable registration routine for the HMS
*
*     Purpose : Register all variables that are to be used by CTP, that are
*     connected with the HMS.  This includes externally configured
*     parameters/contants, event data that can be a histogram source, and
*     possible test results and scalers.
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*     Created: 9-Feb-1994  Stephen A. Wood
*     $Log$
*     Revision 1.10  1995/05/11 18:57:25  cdaq
*     (SAW) Add calls to register h_ntuple.cmn and h_sieve_ntuple.cmn
*
* Revision 1.9  1995/01/27  20:15:54  cdaq
* (SAW) Add call to sieve slit register routine
*
* Revision 1.8  1994/08/18  04:11:26  cdaq
* (SAW) Call makereg generated routines to register variables
*
* Revision 1.7  1994/06/17  03:25:29  cdaq
* (KBB) Execute all code despite registration errors
*
* Revision 1.6  1994/06/16  03:43:47  cdaq
* (SAW) Register filenames for reports
*
* Revision 1.5  1994/04/12  17:25:03  cdaq
* (KBB) Add ntuple call
*
* Revision 1.4  1994/02/22  19:37:53  cdaq
* (SAW) Remove CTP register calls to fortran PARAMETER's
*
* Revision 1.3  1994/02/22  18:56:45  cdaq
* (SAW) Make a call to h_register_param
*
* Revision 1.2  1994/02/11  18:36:17  cdaq
* Split off CTP variables registration from initialize routines
*
* Revision 1.1  1994/02/11  04:18:24  cdaq
* Initial revision
*
*
*----------------------------------------------------------------------
      implicit none
      save
*
      character*20 here
      parameter (here='h_register_variables')
*
      logical ABORT
      character*(*) err
*
      logical FAIL
      character*1000 why
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*
      call r_hms_filenames

      call r_h_ntuple

      call r_h_sieve_ntuple

      call h_register_param(FAIL,why) ! TRACKING ROUTINE
      IF(err.NE.' ' .and. why.NE.' ') THEN   !keep warnings
        call G_append(err,' & '//why)
      ELSEIF(why.NE.' ') THEN
        err= why
      ENDIF
      ABORT= ABORT .or. FAIL
*
      call h_ntuple_register(FAIL,why)  ! Remove this when ctp files fixed
      IF(err.NE.' ' .and. why.NE.' ') THEN  !keep warnings
        call G_append(err,' & '//why)
      ELSEIF(why.NE.' ') THEN
        err= why
      ENDIF
      ABORT= ABORT .or. FAIL
*
      if(ABORT .or. err.NE.' ') call G_add_path(here,err)
*
      call h_sv_nt_register(FAIL,why)   ! Remove this when ctp files fixed
      IF(err.NE.' ' .and. why.NE.' ') THEN  !keep warnings
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
