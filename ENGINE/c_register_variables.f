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
*     Revision 1.4  1994/06/16 03:41:41  cdaq
*     (SAW) Register filenames for reports
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
      integer*4 ierr
      logical ABORT
      character*(*) err
*
      ABORT = .false.
*
*     Register variables in coin_beam
      ierr= regparmreal('CEBEAM',CEBEAM,0)
      IF(ierr.NE.0) err= 'unable to register "CEBEAM"'
      ABORT= ierr.ne.0 .or. ABORT
      ierr= regparmreal('CPBEAM',CPBEAM,0)
      IF(ierr.NE.0) err= 'unable to register "CPBEAM"'
      ABORT= ierr.ne.0 .or. ABORT
      ierr= regparmreal('CXRAST',CXRAST,0)
      IF(ierr.NE.0) err= 'unable to register "CXRAST"'
      ABORT= ierr.ne.0 .or. ABORT
      ierr= regparmreal('CYRAST',CYRAST,0)
      IF(ierr.NE.0) err= 'unable to register "CYRAST"'
      ABORT= ierr.ne.0 .or. ABORT
*     Register variables in TARGET
      ierr= regparmreal('TMASS_TARGET',TMASS_TARGET,0)
      IF(ierr.NE.0) err= 'unable to register "TMASS_TARGET"'
      ABORT= ierr.ne.0 .or. ABORT
      ierr= regparmreal('TZ_TARGET',TZ_TARGET,0)
      IF(ierr.NE.0) err= 'unable to register "TZ_TARGET"'
      ABORT= ierr.ne.0 .or. ABORT
      ierr= regparmreal('TA_TARGET',TA_TARGET,0)
      IF(ierr.NE.0) err= 'unable to register "TA_TARGET"'
      ABORT= ierr.ne.0 .or. ABORT
      ierr= regparmreal('TTHETA_TAR',TTHETA_TAR,0)
      IF(ierr.NE.0) err= 'unable to register "TTHETA_TAR"'
      ABORT= ierr.ne.0 .or. ABORT
      ierr= regparmreal('TPHI_TAR',TPHI_TAR,0)
      IF(ierr.NE.0) err= 'unable to register "TPHI_TAR"'
      ABORT= ierr.ne.0 .or. ABORT
      ierr= regparmreal('TRAD_LENGTH',TRAD_LENGTH,0)
      IF(ierr.NE.0) err= 'unable to register "TRAD_LENGTH"'
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
      if(.not.ABORT) call c_ntuple_register(ABORT,err)
*
      if(ABORT) call g_add_path(here,err)
*
      return
      end
