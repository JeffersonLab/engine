      subroutine g_register_variables(ABORT,err)
*----------------------------------------------------------------------
*
*     CTP variable registration routine
*
*     Purpose : Register all variables that are to be used by CTP.  This
*     includes externally configured parameters/contants, event data that
*     can be a histogram source, and possible test results and scalers.
*
*     Method: 1. Register variables needed to use CTP to get various
*     filenames.  And register other common variables.
*             2. Call Register routines for HMS, SOS and coincidence.
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*     Created: 9-Feb-1994  Stephen A. Wood
*     $Log$
*     Revision 1.1  1994/02/11 18:35:11  cdaq
*     Initial revision
*
*
*----------------------------------------------------------------------
      implicit none
      save
*
      character*20 here
      parameter (here='g_register_variables')
*
      logical ABORT
      character*(*) err
*
      include 'gen_filenames.cmn'
      include 'gen_routines.dec'
*
      integer ierr
      logical HMS_ABORT,SOS_ABORT
      character*132 HMS_err,SOS_err
*
*     Register the variables that contain the filenames and other
*     configuration variables.
*
      err = ' '
      ierr = regparmstring('hist_filename',g_ctp_hist_filename,0)
      if(ierr.ne.0) err = 'unable to register "hist_filename"'
      abort = ierr.ne.0
      ierr = regparmstring('parm_filename',g_ctp_parm_filename,0)
      if(ierr.ne.0) err = 'unable to register "parm_filename"'
      abort = ierr.ne.0
      ierr = regparmstring('test_filename',g_ctp_test_filename,0)
      if(ierr.ne.0) err = 'unable to register "test_filename"'
      abort = ierr.ne.0
      ierr = regparmstring('data_source_filename'
     $     ,g_data_source_filename,0)
      if(ierr.ne.0) err = 'unable to register "data_source_filename"'
      abort = ierr.ne.0
      ierr = regparmstring('alias_filename',g_alias_filename,0)
      if(ierr.ne.0) err = 'unable to register "alias_filename"'
      abort = ierr.ne.0
      ierr = regparmstring('histout_filename',g_histout_filename,0)
      if(ierr.ne.0) err = 'unable to register "histout_filename"'
      abort = ierr.ne.0
      ierr = regparmstring('decode_map_filename'
     $     ,g_decode_map_filename,0)
      if(ierr.ne.0) err = 'unable to register "decode_map_filename"'
      abort = ierr.ne.0
      ierr = regparmstring('max_events',g_max_events,0)
      if(ierr.ne.0) err = 'unable to register "max_events"'
      abort = ierr.ne.0
*
      if(abort) then
         call g_add_path(here,err)
         return
      endif
*
      call h_register_variables(HMS_ABORT, HMS_err) ! HMS

      call s_register_variables(SOS_ABORT, SOS_err) ! SOS

      ABORT= HMS_ABORT .or. SOS_ABORT
      If(HMS_ABORT .and. .NOT.SOS_ABORT) Then
         err= HMS_err
      ElseIf(SOS_ABORT .and. .NOT.HMS_ABORT) Then
         err= SOS_err
      ElseIf(HMS_ABORT .and. SOS_ABORT) Then
         err= '&'//SOS_err
         call G_prepend(HMS_err,err)
      EndIf
*
      if(.not.ABORT) then
         call c_register_variables(ABORT,err)
      endif

      if(ABORT .or. err.ne. ' ') call g_add_path(here,err)

      return
      end















