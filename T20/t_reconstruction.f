      SUBROUTINE T_reconstruction(ABORT,err)
*--------------------------------------------------------
*-       Prototype C analysis routine
*-
*-   Purpose and Methods : reconstruction of T20 quantities 
*-
*-   Output: ABORT              - success or failure
*-         : err             - reason for failure, if any
*- 
* $Log$
* Revision 1.1  1998/12/01 20:54:23  saw
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*16 here
      parameter (here= 'T_reconstruction')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 't20_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      include 't20_bypass_switches.cmn'
*
*     local variables
*      integer*4 istat
*--------------------------------------------------------
*
ccc      ABORT= .TRUE.
ccc      err= ':no events analyzed!'
* increment reconstructed number
      err = ' '
c      t_recon_num= t_recon_num + 1
*
*     dump all raw data
      call t_raw_dump_all(ABORT,err)
      if(ABORT) then
         call g_add_path(here,err)
         return
      endif
*
*     TRANSLATE SCINTILATORS AND CALCULATE START TIME
*     SOS_RAW_SCIN ====> SOS_DECODED_SCIN
*     
c      If(sbypass_trans_scin.eq.0) then
c        call S_TRANS_SCIN(ABORT,err)
c        if(ABORT)  then
c           call G_add_path(here,err)
c*          return
c        endif                                     ! end test on SCIN ABORT
c      endif                              ! end test on sbypass_trans_scin
*
*     TRANSLATE SMISC TDC HITS.
*     S_RAW_MISC ====> SOS_DECODED_MISC
*
c      If(sbypass_trans_scin.eq.0) then
c         call S_TRANS_MISC(ABORT,err)
c         if(ABORT)  then
c            call G_add_path(here,err)
c*     return
c         endif                          ! end test on SCIN ABORT
c      endif                             ! end test on hbypass_trans_scin
*
      if(tbypass_test.eq.0) then        ! Analyze test detector straw tubes
        call t_test_straw_analyze
      endif
*
* The next two routines are not needed by the t_polder_* routines
      call t_hodos(ABORT,err)          !raw data transfer to CTP & user hist.
      call t_mwpc(ABORT,err)           !raw data transfer to CTP & user hist.
      call t_misc(ABORT,err)           !raw data transfer to CTP & user hist.
* The next routine is needed by the t_polder_* routines
* not any more 3/31/97

      call t_hms(ABORT,err)

      if(tbypass_polder.eq.0) then
        call t_polder_analyse(ABORT,err)
      else if(tbypass_polder.eq.1) then
        call t_polder_cuts(ABORT,err)
      else if(tbypass_polder.eq.2) then
        call t_polder_alignement(ABORT,err)
      else if(tbypass_polder.eq.3) then
        call t_polder_calmwpc3(ABORT,err)
      endif

      if(tbypass_test.eq.0) then        ! Analyze test detector straw tubes
        call t_test_stpld_comp
      endif

c
*     Successful return
      ABORT=.FALSE.
      RETURN
      END
