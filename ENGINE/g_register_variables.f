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
*     Modified: 17-May-1994 Kevin B. Beard, Hampton U.
*     Modified: 24-May-1994 K.B.Beard
*
*     $Log$
*     Revision 1.7  1994/08/04 03:47:05  cdaq
*     (SAW) Add call to Breuer's hack_register_variables
*
* Revision 1.6  1994/06/21  16:40:20  cdaq
* (SAW) Register g_report_rebook and scalers
*
* Revision 1.5  1994/06/17  03:30:35  cdaq
* (KBB) Execute all code despite registration errors
*
* Revision 1.4  1994/06/16  03:24:28  cdaq
* (SAW) Register reconstruction filenames and report generator filenames etc.
*
* Revision 1.3  1994/06/07  18:14:57  cdaq
* (KBB) Add regististration for enable_EvtypeN and triggered_EvTypeN
*
* Revision 1.2  1994/03/24  15:29:53  cdaq
* (SAW) Add registration of rebook flags for parm,test,hist
*
* Revision 1.1  1994/02/11  18:35:11  cdaq
* Initial revision
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
      integer ierr,m,i
      logical FAIL
      character*1000 why
      character*30 msg
      real rv(10)
*
      INCLUDE 'gen_run_info.cmn'
      INCLUDE 'gen_run_info.dte'
      INCLUDE 'gen_event_info.cmn'
      INCLUDE 'gen_run_pref.cmn'
      INCLUDE 'gen_run_pref.dte'
      INCLUDE 'gen_scalers.cmn'
*
*----------------------------------------------------------------------
*
*     Register the variables that contain the filenames and other
*     configuration variables.
*
      ABORT= .FALSE.
      err = ' '
*
      ierr = regparmstring('hist_filename',g_ctp_hist_filename,0)
      if(ierr.ne.0) call G_append(err,',"hist_filename"')
      ABORT= ierr.ne.0 .or. ABORT
*
      ierr = regparmint('hist_rebook',g_hist_rebook,0)
      if(ierr.ne.0) call G_append(err,',"hist_rebook"')
      ABORT= ierr.ne.0 .or. ABORT
*
      ierr = regparmstring('parm_filename',g_ctp_parm_filename,0)
      if(ierr.ne.0) call G_append(err,',"parm_filename"')
      ABORT= ierr.ne.0 .or. ABORT
*
      ierr = regparmint('parm_rebook',g_parm_rebook,0)
      if(ierr.ne.0) call G_append(err,',"parm_rebook"')
      ABORT= ierr.ne.0 .or. ABORT
*
      ierr = regparmstring('test_filename',g_ctp_test_filename,0)
      if(ierr.ne.0) call G_append(err,',"test_filename"')
      ABORT= ierr.ne.0 .or. ABORT
*
      ierr = regparmint('test_rebook',g_test_rebook,0)
      if(ierr.ne.0) call G_append(err,',"test_rebook"')
      ABORT= ierr.ne.0 .or. ABORT
*
      ierr = regparmint('report_rebook',g_report_rebook,0)
      if(ierr.ne.0) call G_append(err,',"report_rebook"')
      ABORT= ierr.ne.0 .or. ABORT
*
      ierr = regparmstring('data_source_filename'
     $     ,g_data_source_filename,0)
      if(ierr.ne.0) call G_append(err,',"data_source_filename"')
      ABORT= ierr.ne.0 .or. ABORT
*
      ierr = regparmstring('alias_filename',g_alias_filename,0)
      if(ierr.ne.0) call G_append(err,',"alias_filename"')
      ABORT= ierr.ne.0 .or. ABORT
*
      ierr = regparmstring('histout_filename',g_histout_filename,0)
      if(ierr.ne.0) call G_append(err,',"histout_filename"')
      ABORT= ierr.ne.0 .or. ABORT
*
      ierr = regparmstring('decode_map_filename'
     $     ,g_decode_map_filename,0)
      if(ierr.ne.0) call G_append(err,',"decode_map_filename"')
      ABORT= ierr.ne.0 .or. ABORT
*
      ierr = regparmstring('g_report_template_filename'
     $     ,g_report_template_filename,0)
      if(ierr.ne.0) call g_append(err,',"g_report_template_filename"')
      ABORT = ierr.ne.0.or.ABORT
*
      ierr = regparmstring('g_report_output_filename'
     $     ,g_report_output_filename,0)
      if(ierr.ne.0) call g_append(err,',"g_report_output_filename"')
      ABORT = ierr.ne.0.or.ABORT
*
      ierr = regparmstring('g_report_blockname'
     $     ,g_report_blockname,0)
      if(ierr.ne.0) call g_append(err,',"g_report_blockname"')
      ABORT = ierr.ne.0.or.ABORT
*
      ierr = regparmint('max_events',g_max_events,0)
      if(ierr.ne.0) call G_append(err,',"max_events"')
      ABORT= ierr.ne.0 .or. ABORT
*
*-KBB
      ierr = regparmint('RUN_number',gen_run_number,0)
      if(ierr.ne.0) call G_append(err,',"RUN_number"')
      ABORT= ierr.ne.0 .or. ABORT
*
      ierr = regparmint('RUN_type',gen_run_type,0)
      if(ierr.ne.0) call G_append(err,',"RUN_type"')
      ABORT= ierr.ne.0 .or. ABORT
*
      ierr = regparmint('RUN_total_events',gen_run_total_events,0)
      if(ierr.ne.0) call G_append(err,',"RUN_total_events"')
      ABORT= ierr.ne.0 .or. ABORT
*
      ierr = regparmstring('RUN_comment',gen_run_comment,0)
      if(ierr.ne.0) call G_append(err,',"RUN_comment"')
      ABORT= ierr.ne.0 .or. ABORT
*
      ierr = regparmstring('RUN_start_date',gen_run_date_start,0)
      if(ierr.ne.0) call G_append(err,',"RUN_start_date"')
      ABORT= ierr.ne.0 .or. ABORT
*
      ierr = regparmstring('RUN_stop_date',gen_run_date_stop,0)
      if(ierr.ne.0) call G_append(err,',"RUN_stop_date"')
      ABORT= ierr.ne.0 .or. ABORT
*
      ierr = regparmstring('RUN_last_date',gen_run_date_last,0)
      if(ierr.ne.0) call G_append(err,',"RUN_last_date"')
      ABORT= ierr.ne.0 .or. ABORT
*
      ierr = regparmint('RUN_start_event',gen_run_starting_event,0)
      if(ierr.ne.0) call G_append(err,',"RUN_start_event"')
      ABORT= ierr.ne.0 .or. ABORT
*
      ierr = regparmint('RUN_stop_event',gen_run_stopping_event,0)
      if(ierr.ne.0) call G_append(err,',"RUN_stop_event"')
      ABORT= ierr.ne.0 .or. ABORT
*
      Do m=0,gen_MAX_trigger_types
         call G_build_note('enable_EvType$','$',m,' ',rv,' ',msg)
         call squeeze(msg,i)
         ierr= regparmint(msg(1:i),gen_run_enable(m),0)
         if(ierr.ne.0) call G_append(err,',"'//msg(1:i)//'"')
         ABORT= ierr.ne.0 .or. ABORT
      EndDo
*
      Do m=0,gen_MAX_trigger_types
         call G_build_note('triggered_EvType$','$',m,' ',rv,' ',msg)
         call squeeze(msg,i)
         ierr= regparmint(msg(1:i),gen_run_triggered(m),0)
         if(ierr.ne.0) call G_append(err,',"'//msg(1:i)//'"')
         ABORT= ierr.ne.0 .or. ABORT
      EndDo
*
      ierr = regparmint('EVENT_id',gen_event_ID_number,0)
      if(ierr.ne.0) call G_append(err,',"EVENT_id"')
      ABORT= ierr.ne.0 .or. ABORT
*
      ierr = regparmint('EVENT_type',gen_event_type,0)
      if(ierr.ne.0) call G_append(err,',"EVENT_type"')
      ABORT= ierr.ne.0 .or. ABORT
*
      ierr = regparmint('EVENT_class',gen_event_class,0)
      if(ierr.ne.0) call G_append(err,',"EVENT_class"')
      ABORT= ierr.ne.0 .or. ABORT
*
      ierr = regparmint('EVENT_sequenceN',gen_event_sequence_N,0)
      if(ierr.ne.0) call G_append(err,',"EVENT_sequenceN"')
      ABORT= ierr.ne.0 .or. ABORT
*-KBB
*
      ierr = regparmint('SHOW_progress',gen_show_progress,0)
      if(ierr.ne.0) call G_append(err,',"SHOW_progress"')
      ABORT= ierr.ne.0 .or. ABORT
*
      ierr = regparmint('SHOW_interval',gen_show_interval,0)
      if(ierr.ne.0) call G_append(err,',"SHOW_interval"')
      ABORT= ierr.ne.0 .or. ABORT
*
      ierr = regparmint('PREF_muddleON',gen_pref_muddleON,0)
      if(ierr.ne.0) call G_append(err,',"PREF_muddleON"')
      ABORT= ierr.ne.0 .or. ABORT
*
*     Scalers
*
      ierr = regparmint('scalers',scalers,'Raw scalers')
      if(ierr.ne.0) call G_append(err,',"scalers"')
      ABORT= ierr.ne.0 .or. ABORT
*
      IF(ABORT) call G_prepend(':unable to register',err)
*
      call h_register_variables(FAIL,why) ! HMS
      IF(err.NE.' ' .and. why.NE.' ') THEN
         call G_append(err,' & '//why)
      ELSEIF(why.NE.' ') THEN
         err= why
      ENDIF
      ABORT= ABORT .or. FAIL 
*     
      call s_register_variables(FAIL,why) ! SOS
      IF(err.NE.' ' .and. why.NE.' ') THEN
         call G_append(err,' & '//why)
      ELSEIF(why.NE.' ') THEN
         err= why
      ENDIF
      ABORT= ABORT .or. FAIL 
*
      call c_register_variables(FAIL,why)
      IF(err.NE.' ' .and. why.NE.' ') THEN
         call G_append(err,' & '//why)
      ELSEIF(why.NE.' ') THEN
         err= why
      ENDIF
      ABORT= ABORT .or. FAIL 
*
      call hack_register_variables(FAIL,why)
      IF(err.NE.' ' .and. why.NE.' ') THEN
         call G_append(err,' & '//why)
      ELSEIF(why.NE.' ') THEN
         err= why
      ENDIF
      ABORT= ABORT .or. FAIL 

      if(ABORT .or. err.NE.' ') call g_add_path(here,err)
*
      return
      end
