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
*     Revision 1.10  1995/07/27 19:38:27  cdaq
*     (SAW) Relocate data statements for f2c compatibility
*
* Revision 1.9  1994/10/11  18:39:59  cdaq
* (SAW) Add some hacks for event dislpay
*
* Revision 1.8  1994/08/18  04:11:47  cdaq
* (SAW) Call makereg generated routines to register variables
*
* Revision 1.7  1994/08/04  03:47:05  cdaq
* (SAW) Add call to Breuer's hack_register_variables
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
      include 'gen_routines.dec'
*
      include 'gen_run_info.cmn'
      include 'gen_run_pref.cmn'

      integer ierr,m,i
      logical FAIL
      character*1000 why
      character*30 msg
*
      include 'gen_run_info.dte'
      include 'gen_run_pref.dte'
*
*----------------------------------------------------------------------
*
*     Register the variables that contain the filenames and other
*     configuration variables.
*
      ABORT= .FALSE.
      err = ' '
*
      call r_gen_filenames

      call r_gen_run_info

      call r_gen_event_info

      call r_gen_scalers

      call r_gen_run_pref

      call r_gen_data_structures        ! Contains both HMS and SOS stuff

*HDISPLAY      call r_one_ev_io
*
*     Need to change in parm files
*     hist_filename -> g_ctp_hist_filename
*     g_hist_rebook -> hist_rebook
*     parm_filename -> g_ctp_parm_filename
*     parm_rebook -> g_parm_rebook
*     test_filename -> g_ctp_test_filename
*     test_rebook -> g_test_rebook
*     report_rebook -> g_report_rebook
*     data_source_filename -> g_data_source_filename
*     alias_filename -> g_alias_filename
*     histout_filename -> g_histout_filename
*     decode_map_filename -> g_decode_map_filename
*     g_report_template_filename -> g_report_template_filename
*     g_report_output_filename -> g_report_output_filename
*     g_report_blockname -> g_report_blockname
*     max_events -> g_max_events
*     RUN_number -> gen_run_number
*     RUN_type -> gen_run_type
*     RUN_total_events -> gen_run_total_events
*     RUN_comment -> gen_run_comment
*     RUN_start_date -> gen_run_date_start
*     RUN_stop_date -> gen_run_date_stop
*     RUN_last_date -> gen_run_date_last
*     RUN_start_event -> gen_run_starting_event
*     RUN_stop_event -> gen_run_stopping_event
*     EVENT_id -> gen_event_ID_number
*     EVENT_type -> gen_event_type
*     EVENT_class -> gen_event_class
*     EVENT_sequenceN -> gen_event_sequence_N
*     SHOW_progress -> gen_show_progress
*     SHOW_interval -> gen_show_interval
*     PREF_muddleON -> gen_pref_muddleON

*
* Leave in these aliases
*
      Do m=0,gen_MAX_trigger_types
        write(msg,'("enable_EvType",i4)') m
        call squeeze(msg,i)
        ierr= regparmint(msg(1:i),gen_run_enable(m),0)
        if(ierr.ne.0) call G_append(err,',"'//msg(1:i)//'"')
        ABORT= ierr.ne.0 .or. ABORT
      EndDo
*
      Do m=0,gen_MAX_trigger_types
        write(msg,'("triggered_EvType",i4)') m
        call squeeze(msg,i)
        ierr= regparmint(msg(1:i),gen_run_triggered(m),0)
        if(ierr.ne.0) call G_append(err,',"'//msg(1:i)//'"')
        ABORT= ierr.ne.0 .or. ABORT
      EndDo
*
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
