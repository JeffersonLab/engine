      subroutine c_register_variables(ABORT,err)
*----------------------------------------------------------------------
*
*     CTP variable registration routine for coincidences
*
*     Purpose : Register all variables that are to be used by CTP, that are
*     connected with the coincidence calculations.  This includes
*     externally configured parameters/contants, event data that can be a
*     histogram source, and possible test results and scalers.
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*     Created: 9-Feb-1994  Stephen A. Wood
*     $Log$
*     Revision 1.5  1994/06/17 03:19:44  cdaq
*     (KBB) Execute all code despite registration errors
*
* Revision 1.4  1994/06/16  03:41:41  cdaq
* (SAW) Register filenames for reports
*
* Revision 1.3  1994/06/14  03:19:03  cdaq
* (DFG) register target and beam variables
*
* Revision 1.2  1994/04/12  17:13:37  cdaq
* (KBB) Add ntuple call
*
* Revision 1.1  1994/02/11  18:32:18  cdaq
* Initial revision
*
*----------------------------------------------------------------------
      implicit none
      include 'gen_data_structures.cmn'
      include 'coin_filenames.cmn'
      include 'gen_routines.dec'
      save
*
      character*20 here
      parameter (here='c_register_variables')
*
      logical ABORT
      character*(*) err
*
      integer*4 ierr
      logical FAIL
      character*1000 why
*..................................................................
*
      ABORT = .false.
      err= ' '
*
*     Register variables in coin_beam
      ierr= regparmreal('CEBEAM',CEBEAM,0)
      IF(ierr.NE.0) call G_append(err,',CEBEAM')
      ABORT= ierr.ne.0 .or. ABORT
      ierr= regparmreal('CPBEAM',CPBEAM,0)
      IF(ierr.NE.0) call G_append(err,',CPBEAM')
      ABORT= ierr.ne.0 .or. ABORT
      ierr= regparmreal('CXRAST',CXRAST,0)
      IF(ierr.NE.0) call G_append(err,',CXRAST')
      ABORT= ierr.ne.0 .or. ABORT
      ierr= regparmreal('CYRAST',CYRAST,0)
      IF(ierr.NE.0) call G_append(err,',CYRAST')
      ABORT= ierr.ne.0 .or. ABORT
*     Register variables in TARGET
      ierr= regparmreal('TMASS_TARGET',TMASS_TARGET,0)
      IF(ierr.NE.0) call G_append(err,',TMASS_TARGET')
      ABORT= ierr.ne.0 .or. ABORT
      ierr= regparmreal('TZ_TARGET',TZ_TARGET,0)
      IF(ierr.NE.0) call G_append(err,',TZ_TARGET')
      ABORT= ierr.ne.0 .or. ABORT
      ierr= regparmreal('TA_TARGET',TA_TARGET,0)
      IF(ierr.NE.0) call G_append(err,',TA_TARGET')
      ABORT= ierr.ne.0 .or. ABORT
      ierr= regparmreal('TTHETA_TAR',TTHETA_TAR,0)
      IF(ierr.NE.0) call G_append(err,',TTHETA_TAR')
      ABORT= ierr.ne.0 .or. ABORT
      ierr= regparmreal('TPHI_TAR',TPHI_TAR,0)
      IF(ierr.NE.0) call G_append(err,',TPHI_TAR')
      ABORT= ierr.ne.0 .or. ABORT
      ierr= regparmreal('TRAD_LENGTH',TRAD_LENGTH,0)
      IF(ierr.NE.0) call G_append(err,',TRAD_LENGTH')
      ABORT= ierr.ne.0 .or. ABORT
*
      ierr = regparmstring('c_report_template_filename'
     $     ,c_report_template_filename,0)
      if(ierr.ne.0) call g_append(err,',"c_report_template_filename"')
      ABORT = ierr.ne.0.or.ABORT
*
      ierr = regparmstring('c_report_blockname'
     $     ,c_report_blockname,0)
      if(ierr.ne.0) call g_append(err,',"c_report_blockname"')
      ABORT = ierr.ne.0.or.ABORT
*
      IF(ABORT) THEN
        call G_prepend(':unable to register',err)
      ENDIF
*
      call c_ntuple_register(FAIL,why)
      IF(err.NE.' ' .and. why.NE.' ') THEN
        call G_append(err,' & '//why)
      ELSEIF(why.NE.' ') THEN
        err= why
      ENDIF
      ABORT= ABORT .or. FAIL
*
      if(ABORT .or. err.NE.' ') call g_add_path(here,err)
*
      return
      end
