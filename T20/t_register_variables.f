      subroutine t_register_variables(ABORT,err)
*----------------------------------------------------------------------
*
*     CTP variable registration routine for T20
*
*     Purpose : Register all variables that are to be used by CTP, that are
*     connected with the SOS.  This includes externally configured
*     parameters/contants, event data that can be a histogram source, and
*     possible test results and scalers.
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*     Created: 22-Jan-1997  Stephen A. Wood
*
* $Log$
* Revision 1.1  1998/12/01 20:57:17  saw
* Initial revision
*
*----------------------------------------------------------------------
      implicit none
      save
*
      character*20 here
      parameter (here='t_register_variables')
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
      call r_t20_data_structures

      call r_t20_filenames

      call r_t20_test_detectors

      call r_t20_hodo

      call r_t20_reg_polder_structures

      call r_t20_misc

      call r_t20_hms

      call r_t_ntuple

      call t_register_param(FAIL,why) ! TRACKING ROUTINE
      IF(err.NE.' ' .and. why.NE.' ') THEN   !keep warnings
        call G_append(err,' & '//why)
      ELSEIF(why.NE.' ') THEN
        err= why
      ENDIF
      ABORT= ABORT .or. FAIL
*
      call t_ntuple_register(FAIL,why)  ! Remove this when ctp files fixed
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
