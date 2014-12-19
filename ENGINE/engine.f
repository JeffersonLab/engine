* PROGRAM Engine
*--------------------------------------------------------
*-       Prototype C analysis routine
*-
*- This is the analysis shell for CEBAF hall C.
*  It gets all of its instructions via the CTP package
*- Loops through data until it encounters an error.
*-
*-   Created  18-Nov-1993   Kevin B. Beard, Hampton Univ.
* $Log: engine.f,v $
* Revision 1.41.6.3  2004/07/09 14:12:46  saw
* Add function calls to fill CTP ROOT Trees
*
* Revision 1.41.6.2  2004/06/30 19:31:49  cdaq
* Add call to g_examine_picture_event (DJG)
*
* Revision 1.41.6.1  2004/06/18 11:24:11  cdaq
*  Fixed so that runstats works under Linux
*
* Revision 1.41  2004/05/27 23:51:28  jones
* Initialize EoF = .false.
*
* Revision 1.40  2004/05/27 22:01:55  jones
* Comment call to  g_analyze_scalers when there is an event 129
* ( a CODA 1.4 scaler event).
*
* Revision 1.39  2004/05/19 21:33:52  jones
* Initialize physics_events=0
*
* Revision 1.38  2004/05/11 18:29:27  jones
* Add ability when using syncfilter to skip events if "skip_event"
* is set to true in g_analyze_scaler_bank.f
*
* Revision 1.37  2003/12/19 17:45:31  jones
* a) Fill gscaler_skipped and gscaler_saved only when using syncfilter
* b) Fixed bug with skipping events for low beam current. Make sure it
*     only works when using syncfilter
* c) Clean up output about syncfilter effects
*
* Revision 1.36  2003/12/17 15:10:56  jones
*  fix problem in sync filter part
*
* Revision 1.35  2003/09/05 21:49:12  jones
* Merge in online03 changes  (mkj)
*
* Revision 1.34  2003/04/03 00:30:28  jones
* Add call to s_cal_calib ( V. Tadevosyan)
*
* Revision 1.33  2003/03/24 22:49:41  jones
* Changes for HMS calo calibration. Include hms_calorimeter.cmn and add call
* to h_cal_calib at end of run if hdbg_tracks_cal .lt. 0
*
* Revision 1.32.2.8  2003/09/04 20:30:48  jones
* Changes for running with syncfilter (mkj)
*
* Revision 1.32.2.7  2003/08/14 00:42:23  cdaq
* Modify to be able to write scaler rates for each read to a file (mkj)
*
* Revision 1.32.2.6  2003/06/26 12:38:11  cdaq
* add write statement when genable_sos_satcorr .ne. 0  (mkj)
*
* Revision 1.32.2.5  2003/04/21 23:45:58  cdaq
* Modified so only one message about scaler kludge is printed. (MKJ)
*
* Revision 1.32.2.4  2003/04/14 18:02:06  jones
* Modified so that engine will not analyze events until after first scaler read.
*
* Revision 1.32.2.3  2003/04/09 02:47:00  cdaq
* Update readout code to ignore HV and EPICS events when searching for run_info event
*
* Revision 1.32.2.2  2003/04/03 01:02:44  cdaq
* match main branch apr-02-2003
*
* Revision 1.32.2.1  2003/03/25 03:03:40  cdaq
*  match main brach mar-24-2003
*
* Revision 1.32  2003/02/21 14:51:13  jones
* Added line to call s_fieldcorr subroutine
*
* Revision 1.31  2003/02/15 17:11:45  jones
* Eliminated STOP command when run info event found after starting the analyze physics events. Just wrote out comments to let the user decide what to do.
*
* Revision 1.30  2003/02/12 20:30:59  jones
* Initialize variable 'problems' to false ( E. Brash)
*
* Revision 1.29  2002/12/20 21:55:23  jones
* Modified by Hamlet for new HMS aerogel
*
* Revision 1.28  2002/09/24 20:10:34  jones
* Added calls to subroutines h_fieldcorr.f and g_apply_offsets.f
*
* Revision 1.27  1999/11/04 20:35:14  saw
* Linux/G77 compatibility fixes
*
* Revision 1.26  1999/06/10 14:30:35  csa
* (JRA) Cleanup, handling of go_info event
*
* Revision 1.25  1999/02/23 16:47:30  csa
* (JRA) Changes to scaler event handling and cleanup
*
* Revision 1.24  1998/12/01 16:01:48  saw
* (SAW) Close preproc output file at end of run
*
* Revision 1.23  1996/11/08 15:40:09  saw
* (JRA) Add analysis of epics events.
*
* Revision 1.22  1996/09/04 15:33:43  saw
* (JRA) Assorted changes and diagnostics
*
* Revision 1.21  1996/04/29 19:19:04  saw
* (JRA) Corrections
*
* Revision 1.20  1996/01/24 16:11:10  saw
* (JRA) Change evtype to registered gen_event_type.  Refresh statistics
*       file at a time interval rather than event interval
*
* Revision 1.19  1996/01/16 21:12:41  cdaq
* (JRA) Add tcl run statistics display
*
* Revision 1.18  1995/10/09 19:59:00  cdaq
* (JRA) Improve event counting for periodic dumping.  Dump pedestal data
* at end of run.
*
* Revision 1.17  1995/09/22 19:39:13  cdaq
* (SAW) Move g_ctp_database from g_init_filenames to here.  Process all
* CTP command line vars after every ctp file read so that command line
* overrides everything.
*
* Revision 1.16  1995/07/27 19:45:40  cdaq
* (SAW) f2c compatibility changes.  Only shutdown ntuples at very end.
*       ctp command line variables override at every oportunity
*-
* Revision 1.15  1995/05/11  19:02:23  cdaq
* (SAW) Add ability to set CTP variables from the command line
*
* Revision 1.14  1995/04/01  20:12:58  cdaq
* (SAW) Call g_proper_shutdown instead of dump_hists for periodic hist dumps
*
* Revision 1.13  1995/03/13  18:11:05  cdaq
* (JRA) Write scaler report when histograms are dumped at intervals
*
* Revision 1.12  1995/01/31  21:12:17  cdaq
* (SAW) Add gen_run_hist_dump_interval for in run hist dumping.  Add commented
*           out code to query user for # of event and hist dump interval.
*
* Revision 1.11  1994/11/22  20:12:01  cdaq
* (SAW) Change "" to " " so this would compile under ultrix.
*
* Revision 1.10  1994/10/19  20:40:29  cdaq
* (SAW) Add handling of RPC requests
*
* Revision 1.9  1994/07/07  15:28:29  cdaq
* (SAW) Move check for scaler event to proper place
*
* Revision 1.8  1994/06/26  02:07:03  cdaq
* (KBB) Add ability to analyze selected subset of events.  Add evcount stats.
* (SAW) Add call to scaler analysis
*
* Revision 1.7  1994/06/17  03:35:00  cdaq
* (KBB) Upgrade error reporting
*
* Revision 1.6  1994/06/15  14:27:30  cdaq
* (SAW) Actually add call to g_examine_physics_event
*
* Revision 1.5  1994/06/07  18:22:58  cdaq
* (SAW) Add calls to g_examine_physics_event and g_examine_control_event
*
* Revision 1.4  1994/04/15  20:31:25  cdaq
* (SAW) Changes for ONLINE use
*
* Revision 1.3  1994/03/24  22:02:12  cdaq
* Reorganize for online compatibility
*
* Revision 1.2  1994/02/11  18:32:06  cdaq
* Split off CTP variables registration from initialize routines
*
* Revision 1.1  1994/02/04  21:04:59  cdaq
* Initial revision
*
*- 
*--------------------------------------------------------
c      program engine
c
      IMPLICIT NONE
      SAVE
      external jishft, jiand, jieor

      character*6 here
      parameter (here= 'Engine')

      logical ABORT,EoF
      character*800 err,mss

      include 'gen_filenames.cmn'
      include 'gen_craw.cmn'
      include 'gen_run_info.cmn'
      include 'gen_event_info.cmn'
      include 'gen_run_pref.cmn'
      include 'gen_routines.dec'
      include 'gen_scalers.cmn'
      include 'gen_data_structures.cmn'
      include 'hms_data_structures.cmn'
      include 'sos_data_structures.cmn'
      include 'hms_calorimeter.cmn' !for HMS calorimeter calibration
      include 'sos_calorimeter.cmn' !for SOS calorimeter calibration

      logical problems, finished_extracting
      integer total_event_count
      integer physics_events
      integer analyzed_events(0:gen_max_trigger_types)
      integer sum_analyzed,sum_analyzed_skipped
      integer recorded_events(0:gen_max_trigger_types)
      integer skipped_badsync_events(0:gen_max_trigger_types)
      integer skipped_lowbcm_events(0:gen_max_trigger_types)
      integer sum_recorded
      integer num_events_skipped
      integer i,since_cnt,lastdump
      integer mkj,ii
      integer rpc_pend                  ! # Pending asynchronous RPC requests
c
      common /aevents/ analyzed_events
c
      character*80 g_config_environmental_var
      parameter (g_config_environmental_var= 'ENGINE_CONFIG_FILE')

      integer*4 jishft,jiand,jieor

      integer ierr
      integer*4 status
      integer*4 evclose
      character*132 file
      character*20 groupname
      character*132 system_string

      real*4 ebeam,phms,thms,psos,tsos,ntarg

      integer start_time,lasttime
      integer time
      integer*4 preprocessor_keep_event
      external time
c
      integer*4 skipped_events_scal,tindex
      real*8 delta_time
*
*
*--------------------------------------------------------
*
      print *
      print *,' Hall C Proudly Presents: PHYSICS Analysis Engine'

      print *

      total_event_count= 0                      ! Need to register this
      lastdump=0
      physics_events=0
      skipped_events_scal = 0      
      do i=0,gen_max_trigger_types
        analyzed_events(i)=0
        recorded_events(i)=0
        skipped_badsync_events(i)=0
        skipped_lowbcm_events(i)=0
      enddo
      sum_analyzed=0
      sum_analyzed_skipped=0
      sum_recorded=0
      num_events_skipped=0

      rpc_on=0                          ! RPC servicing off by default
      rpc_control=-1                    ! If RPC on, don't block by default

      call g_register_variables(ABORT,err)
      if(ABORT.or.err.ne.' ') then
         call G_add_path(here,err)
         call G_rep_err(ABORT,err)
         If(ABORT) STOP
         err= ' '
      ENDIF

      g_config_filename = ' '

      call engine_command_line(.false.) ! Set CTP vars from command line
*       
      call G_init_filenames(ABORT,err,g_config_environmental_var)
      if(ABORT.or.err.ne.' ') then
         call G_add_path(here,err)
         call G_rep_err(ABORT,err)
         If(ABORT) STOP
         err= ' '
      ENDIF
      call engine_command_line(.false.) ! Set CTP vars from command line
*
* If there is a g_ctp_database_filename set, pass the run number
* to it to set CTP variables
*
      if(.not.ABORT.and.g_ctp_database_filename.ne.' ') then
        call g_ctp_database(ABORT, err ,gen_run_number, g_ctp_database_filename)
        IF(ABORT) THEN
          call G_add_path(here,err)
        endif
      ENDIF

      call engine_command_line(.false.) ! Set CTP vars from command line

      call G_decode_init(ABORT,err)
      if(ABORT.or.err.ne.' ') then
         call G_add_path(here,err)
         call G_rep_err(ABORT,err)
         If(ABORT) STOP
         err= ' '
      endif


      g_data_source_opened = .false.     !not opened yet
      g_data_source_in_hndl= 0           !none
      call G_open_source(ABORT,err)
      if(ABORT.or.err.ne.' ') then
         call G_add_path(here,err)
         call G_rep_err(ABORT,err)
         If(ABORT) STOP
         err= ' '
      endif
c
*
* if preprocessor on, open event file
*
      if(g_preproc_on.ne.0)then
        g_preproc_opened=.false. !not opened yet
        g_preproc_in_hndl=0      !none IO opened
        call g_preproc_open(ABORT,err)
        if (ABORT.or.err.ne.' ')then
          call G_add_path(here,err)
          call G_rep_err(ABORT,err)
          if (ABORT) STOP
          err=' '
        endif
        write(6,*)'Opened CODA event file for preprocessor output'
      endif

      finished_extracting = .false.
      problems = .false.
      syncfilter_on = .false.
      insync = 0
      EoF=.false.
      DO WHILE(.NOT.problems .and. .NOT.ABORT .and. .NOT.EoF .and.
     &         .NOT.finished_extracting)
        mss= ' '
        g_replay_time=time()-start_time

        call G_clear_event(ABORT,err)   !clear out old data
        problems= problems .OR. ABORT

        if(mss.NE.' ' .and. err.NE.' ') then
          call G_append(mss,' & '//err)
        elseif(err.NE.' ') then
          mss= err
        endif

        If(.NOT.problems) Then
          call G_get_next_event(ABORT,err) !get and store 1 event 
          problems= problems .OR. ABORT 
          if(.NOT.ABORT) total_event_count= total_event_count+1

        EndIf

        if(mss.NE.' ' .and. err.NE.' ') then
          call G_append(mss,' & '//err)
        elseif(err.NE.' ') then
          mss= err
        endif
*
* Check if this is a physics event or a CODA control event.
*
        if(.not.problems) then
           gen_event_type = jishft(craw(2),-16)
          if(gen_event_type.le.gen_MAX_trigger_types) then
            recorded_events(gen_event_type)=recorded_events(gen_event_type)+1
            if (gen_event_type.ne.0) sum_recorded=sum_recorded+1
            write(6,*) "AAAAAAAAAAHHHHHHHHHHHHHHHHHHHHHHHHHHH!!!!!!!!!!"
            write(6,*) "Whew, I feel much bettter now"
            write(6,*) "However, you might want to know that I've hit a physics event"
            write(6,*) "In my run info event loop and THAT SHOULD NEVER HAPPEN!!!"
            write(6,*) "KILL ME!!! KILL ME NOW!!!!!"
          endif
*
* if preprocessor is on write all events of trig type > 16
*   (i.e. all non-physics events)
*
          if(gen_event_type.ge.(gen_max_trigger_types-1) .and.
     $         g_preproc_on.ne.0) then
            call g_write_event(ABORT,err)
          endif
*
* if preprocessor is on write trig type 0 (scaler events)
*
          if(gen_event_type.eq.0 .and. g_preproc_on.ne.0) then
            call g_write_event(ABORT,err)
          else if (gen_event_type.eq.130) then !run info event (get e,p,theta)
            finished_extracting=.true.
            write(6,'(a)') 'COMMENTS FROM RUN INFO EVENT'
            call g_extract_kinematics(ebeam,phms,thms,psos,tsos,ntarg)
            write(6,'(a)') 'KINEMATICS FROM RUN INFO EVENT'
            if (ebeam.gt.10.) ebeam=ebeam/1000. !usually in MeV
            write(6,*) '   gpbeam     =',abs(ebeam),' GeV'
            gpbeam=abs(ebeam)
            write(6,*) '   hpcentral  =',abs(phms),' GeV/c'
            hpcentral=abs(phms)
            write(6,*) '   htheta_lab =',abs(thms),' deg.'
            htheta_lab=abs(thms)
            write(6,*) '   spcentral  =',abs(psos),' GeV/c'
            spcentral=abs(psos)
            write(6,*) '   stheta_lab =',abs(tsos),' deg.'
            stheta_lab=abs(tsos)
            write(6,*) '   gtarg_num  =',abs(ntarg)
            gtarg_num=ntarg
          else if (gen_event_type.eq.131 .or. gen_event_type.eq.132) then! EPICS event
            call g_examine_epics_event
          else if (gen_event_type.eq.133) then  !SAW's new go_info events
             call g_examine_go_info(CRAW,ABORT,err)
          else if (gen_event_type.eq.141 .or. gen_event_type.eq.142 .or.
     &             gen_event_type.eq.144) then
*             write(6,*) 'HV information event, event type=',gen_event_type
	  else if (gen_event_type.eq.146..or.gen_event_type.eq.147) then
c	     write(6,*) 'Cheesy poofs! - picture event'
c	     call g_examine_picture_event
          else if (gen_event_type.eq.251) then
             syncfilter_on = .true.
          else
            call g_examine_control_event(CRAW,ABORT,err)
          endif

! Go event is last 'nice tag' for point where we should have already seen
! run-info event.

          if (gen_event_type.eq.18) then
            write(6,*) "no run information event found"
            finished_extracting=.true.
          endif

        endif                           !if .not.problems
      enddo                             !do while .not.finished_extracting
      call G_initialize(ABORT,err)              !includes a total reset
      IF(ABORT.or.err.NE.' ') THEN
         call G_add_path(here,err)
         call G_rep_err(ABORT,err)
         If(ABORT) STOP
         err= ' '
      ENDIF
*
*-attempt to open FASTBUS-CODA file
*
c      g_data_source_opened = .false.     !not opened yet
c      g_data_source_in_hndl= 0           !none
c      call G_open_source(ABORT,err)
c      if(ABORT.or.err.ne.' ') then
c         call G_add_path(here,err)
c         call G_rep_err(ABORT,err)
c         If(ABORT) STOP
c         err= ' '
c      endif

      call engine_command_line(.false.) ! Set CTP vars from command line
c 
      call h_fieldcorr(ABORT,err)
c
c  call h_fieldcorr subroutine 
c    to fix problem with setting hpcentral in field programs
c    applies to experiments using fieldxx.f programs before field02.f
c    parameter genable_hms_fieldcorr is switch to determine
c    whether fix is applied.
c
      call s_fieldcorr(ABORT,err)
c
      if (genable_sos_satcorr.ne.0) then
         write(*,*) '*************'
         write(*,*) ' SOS saturation correction enabled'
         write(*,*) ' Delta modified for each event'
         write(*,*) '*************'
       endif
c
      call G_apply_offsets(ABORT,err)  
c
c  call G_apply_offsets which calls  s_apply_offsets, h_apply_offsets
c   which apply offsets to spect. momenta, angles
c
* Print out the statistics report once...
      if(g_stats_blockname.ne.' '.and.
     $     g_stats_output_filename.ne.' ') then
         file = g_stats_output_filename
         call g_sub_run_number(file, gen_run_number)
         ierr = threp(g_stats_blockname,file)
      endif
*
* Comment out the following three lines if they cause trouble or
* if wish is unavailable.
*
      write(system_string,*) 
     >'./runstats ',file(1:index(file,' ')-1), ' ',
     $     gen_run_number, ' > /dev/null &'
      call system(system_string)
*
*-zero entire event buffer
*
      DO i=1,LENGTH_CRAW
         CRAW(i)= 0
      ENDDO

      since_cnt= 0
      problems= .false.
      EoF = .false.

      if(rpc_on.ne.0) then
        print *,"*****************************************************"
        print *," "
        print *,"ENGINE is enabled to receive RPC requests"
        if(rpc_control.eq.0) then
          print *," "
          print *,"ENGINE will HANG waiting for RPC requests"
                else if(rpc_control.gt.0) then
          print *,"ENGINE will HANG to waitfor RPC requests after "
     $         ,rpc_control," events"
        endif
        if(rpc_control.ge.0) then
          print *,"If you don't want this to happen, put one of the"
          print *,"following in your CTP setup file"
          print *,"    rpc_on = 0 ; Turns off RPC handling"
          print *,"    rpc_control = -1 ; No Hanging, but RPC handled"
        endif
        print *," "
        print *,"*****************************************************"

        call thservset(0,0)             !prepare for RPC requests

      endif
      rpc_pend = 0

      start_time=time()
      lasttime=0.
c
c Start data analysis
      if ( syncfilter_on) then
         write(6,*) ' ******'
         write(6,*) ' Analyzing using Syncfilter'
         write(6,*) ' ******'
      endif
c
c      write(*,*) ' mkj start do while'
      DO WHILE(.NOT.problems .and. .NOT.ABORT .and. .NOT.EoF)
        mss= ' '
        g_replay_time=time()-start_time
        call G_clear_event(ABORT,err)   !clear out old data
        problems= problems .OR. ABORT

        if(mss.NE.' ' .and. err.NE.' ') then
          call G_append(mss,' & '//err)
        elseif(err.NE.' ') then
          mss= err
        endif

        If(.NOT.problems) Then
          call G_get_next_event(ABORT,err) !get and store 1 event
          problems= problems .OR. ABORT
          if(.NOT.ABORT) total_event_count= total_event_count+1
        EndIf
        if(mss.NE.' ' .and. err.NE.' ') then
          call G_append(mss,' & '//err)
        elseif(err.NE.' ') then
          mss= err
        endif
*
* Check if this is a physics event or a CODA control event.
*
        if (.not.problems) then
          gen_event_type = jishft(craw(2),-16)
          if(gen_event_type.le.gen_MAX_trigger_types) then
            recorded_events(gen_event_type)=recorded_events(gen_event_type)+1
            if (gen_event_type.ne.0) sum_recorded=sum_recorded+1
          endif
*
*if preprocessor is on write all events of trig type > 16
*  (i.e. all non-physics events)
*
          if(gen_event_type.ge.(gen_max_trigger_types-1) .and.
     &         g_preproc_on.ne.0) call g_write_event(ABORT,err)
*
* if preprocessor is on write trig type 0 (scaler events)
*
          if(gen_event_type.eq.0 .and. g_preproc_on.ne.0)
     &      call g_write_event(ABORT,err)
c
          if (gen_event_type .eq. 251) then
             write(6,*) ' Syncfilter event, SYNC type = ',craw(5)
             insync = craw(5)
             endif
c
          if (gen_event_type.eq.130) then       !run info event (get e,p,theta)
            write(6,*) " ***********"
            write(6,*) " A run info event after starting to analyze physics events"
            write(6,*) " If you are using the kinematics file  "
            write(6,*) " to set HMS and SOS central momentum and angles then no problem"
            write(6,*) " number of events for event types HMS,SOS,COIN",analyzed_events(1)
     >,analyzed_events(2),analyzed_events(3)
            write(6,*) " If no events analyzed yet for  HMS,SOS, or COIN then no problem"
            write(6,*) " ***********"
            write(6,*) " If you are relying on the run info event "
            write(6,*) " to set HMS and SOS central momentum and angles then for this run"
            write(6,*) " it is best to do it using the kinematics file"
            write(6,*) " ***********"
          endif
          if(jieor(jiand(CRAW(2),'FFFF'x),'10CC'x).eq.0) then ! Physics event
	    if (gen_event_type.eq.0) then          !scaler event.
              analyzed_events(gen_event_type)=analyzed_events(gen_event_type)+1
                call g_analyze_scalers_by_banks(CRAW,ABORT,err)
             if (g_writeout_scaler_filename.ne.' ' .and. analyzed_events(0) .gt. 1) then
               delta_time = max(gscaler_change(gclock_index)/gclock_rate,.000D00)
               write(G_LUN_WRITEOUT_SCALER,'(i10,10g12.5)') gen_event_ID_number,delta_time,
     >       (gscaler_change(INDEX_WRITEOUT_SCALERS(tindex))/delta_time
     >          ,tindex=1,NUM_WRITEOUT_SCALERS)
             endif            
c
               if (syncfilter_on) then 
                 if (  insync .eq. 1 .or.  skip_events ) 
     > write(*,*) ' Skipping out-of-sync events'
                 if ( ave_current_bcm(bcm_for_threshold_cut)  .le. g_beam_on_thresh_cur(bcm_for_threshold_cut)
     >  .or. insync .eq. 1 .or. skip_events ) then
                  do ii=1,MAX_NUM_SCALERS
                   gscaler_skipped(ii) = gscaler_skipped(ii) +  gscaler_change(ii)
                  enddo
                 else
                  do ii=1,MAX_NUM_SCALERS
                   gscaler_saved(ii) = gscaler_saved(ii) +  gscaler_change(ii)
                  enddo
                 endif
               endif
c
               if (analyzed_events(0) .le. 1 ) then
                  write(*,*) '************'
                  write(*,*) 
     > ' Will not analyze events until after first scaler read'
                  write(*,*) '************'
               endif
*
* if preprocessor is on write trig type 0 (scaler events)
*
              if(gen_event_type.eq.0 .and. g_preproc_on.ne.0)
     &           call g_write_event(ABORT,err)
*
* dump report at first scaler event AFTER hist_dump_interval to keep hardware
* and software scalers roughly in sync.
*
              if((physics_events-lastdump).ge.gen_run_hist_dump_interval
     &           .and.gen_run_hist_dump_interval.gt.0) then
                lastdump=physics_events   ! Wait for next interval of dump_int.
                call g_proper_shutdown(ABORT,err)
                print 112,
     >"Finished dumping histograms/scalers for first",
     &             physics_events," events"
 112            format (a,i8,a)
              endif
            else				!REAL physics event.
c
               if (analyzed_events(0) .eq. 0 .and. gen_event_type .le. 3) then
                  skipped_events_scal = skipped_events_scal + 1
                  goto 868      ! kludge mkj
               endif
c
                  if (skipped_events_scal .gt. 0 ) then
                  write(*,*) '************'
                  write(*,*) ' Will not analyze SOS,HMS or coin"
     >," events until after first scaler read'
                  write(*,*) ' Recorded events :',(recorded_events(mkj),mkj=1,3)
                  write(*,*) ' Analyzed events :',(analyzed_events(mkj),mkj=1,3)
                  write(*,*) '************'
                  endif
c
              if(gen_event_type.le.gen_MAX_trigger_types .and.
     $           gen_run_enable(min(gen_event_type,gen_MAX_trigger_types)).ne.0) then
c
               if ( insync .eq. 1 .and. gen_event_type .le. 3 .and. syncfilter_on ) then
                  skipped_badsync_events(gen_event_type)=skipped_badsync_events(gen_event_type) + 1
                  sum_analyzed_skipped = sum_analyzed_skipped + 1
                   goto 868
               endif
               if ( skip_events .and. gen_event_type .le. 3 .and. syncfilter_on ) then
                  skipped_badsync_events(gen_event_type)=skipped_badsync_events(gen_event_type) + 1
                  sum_analyzed_skipped = sum_analyzed_skipped + 1
                  goto 868
               endif
               if ( ave_current_bcm(bcm_for_threshold_cut)  .lt. g_beam_on_thresh_cur(bcm_for_threshold_cut)
     >               .and. gen_event_type .le. 3 .and. syncfilter_on) then
                  skipped_lowbcm_events(gen_event_type)=skipped_lowbcm_events(gen_event_type) + 1
                  sum_analyzed_skipped = sum_analyzed_skipped + 1
                  goto 868
               endif
c

                call g_examine_physics_event(CRAW,ABORT,err)
                problems = problems .or.ABORT
                if(mss.NE.' ' .and. err.NE.' ') then
                  call G_append(mss,' & '//err)
                elseif(err.NE.' ') then
                  mss= err
                endif

                if (gen_event_ID_number.lt.(gen_run_starting_event) .and.
     &              gen_event_type.ne.4) then ! always analyze peds.
                  num_events_skipped = num_events_skipped + 1
                else
                  if(gen_run_starting_event.eq.gen_event_id_number)
     &                 start_time=time()  !reset start time for analysis rate
                  if(.NOT.problems) then
                    if (gen_event_type.ne.0) then	!physics events (not scalers)
                      call G_reconstruction(CRAW,ABORT,err) !COMMONs
                      physics_events = physics_events + 1
                      if (gen_event_type .le. gen_max_trigger_types) then
                          analyzed_events(gen_event_type)=analyzed_events(gen_event_type)+1
                      endif
                      if (gen_event_type.ne.0) sum_analyzed=sum_analyzed+1
                      problems= problems .OR. ABORT
                    else		!gen_event_type=0, scaler event
                    endif
                  else
                     write(*,*) ' mkj problem event_type=',gen_event_type
                  endif

                  if(mss.NE.' ' .and. err.NE.' ') then
                    call G_append(mss,' & '//err)
                  elseif(err.NE.' ') then
                    mss= err
                  endif

                  groupname=' '
                  if (gen_event_type.eq.1) then
                    groupname='hms'
                  else if (gen_event_type.eq.2) then
                    groupname='sos'
                  else if (gen_event_type.eq.3) then
                    groupname='both'
                  else if (gen_event_type.eq.4) then
                    start_time=time()     !reset start time for analysis rate
                    groupname='ped'
                  else
                    write(6,*) 'gen_event_type= '
     >        ,gen_event_type,' for call to g_keep_results'
                  endif

                  If(.NOT.problems .and. groupname.ne.' ') Then
                    call G_keep_results(groupname,ABORT,err) !file away results as
                    problems= problems .OR. ABORT !specified by interface
                  EndIf

                  if(mss.NE.' ' .and. err.NE.' ') then
                    call G_append(mss,' & '//err)
                  elseif(err.NE.' ') then
                    mss= err
                  endif
*
* if preprocessor is on check event for write criteria
*
                  if(g_preproc_on.ne.0)then
                    if(.NOT.problems)then
                      call g_preproc_event(preprocessor_keep_event)
                      if(preprocessor_keep_event.eq.1)then
                        call g_write_event(ABORT,err)
                      endif
                    endif
                  endif

*
*- Here is where we insert a check for an Remote Proceedure Call (RPC)
*- from another process for CTP to interpret
*
                  if(rpc_on.ne.0) then
                    if(rpc_pend.eq.0.and.rpc_control.eq.0) then
                      do while(rpc_pend.eq.0.and.rpc_control.eq.0)
                        ierr = thservone(-1) !block until one RPC request serviced
                        rpc_pend = thcallback()
                      enddo
                    else
                      ierr = thservone(0)   !service one RPC requests
                      rpc_pend = thcallback()
                    endif
                    if(rpc_pend.lt.0) rpc_pend = 0 ! Last thcallback took care of all
                                        ! outstanding requests
                    if(rpc_control.gt.0) rpc_control = rpc_control - 1
                  endif

                endif
              else if (gen_event_type.eq.131 .or. gen_event_type.eq.132) then ! EPICS event
                call g_examine_epics_event
              endif

	    endif    !if REAL physics event as opposed to scaler (evtype=0)

          Else
              if(gen_event_type.eq.129) then
              write(6,*) 'CODA 1.4 SCALER EVENT - event type 129!!!!!'
              write(6,*) ' Will not Analyze this event'
!              call g_analyze_scalers(CRAW,ABORT,err)
!* Dump report at first scaler event AFTER hist_dump_interval to keep hardware
!* and software scalers in sync.
!              if((physics_events-lastdump).ge.gen_run_hist_dump_interval.and.
!     &            gen_run_hist_dump_interval.gt.0) then
!                lastdump=physics_events   ! Wait for next interval of dump_int.
!                call g_proper_shutdown(ABORT,err)
!                print 112,"Finished dumping histograms/scalers for first"
!     &               ,physics_events," events"
! 112            format (a,i8,a)
!              endif
            else if (gen_event_type.eq.133) then  !SAW's new go_info events
              call g_examine_go_info(CRAW,ABORT,err)
            else
              call g_examine_control_event(CRAW,ABORT,err)
            endif
            mss = err
          EndIf
        endif
c
c  skip analyzing data until after first scaler read
c    also can skip if using syncfilter and beam current is low
 868    continue
c
c
*
*Now write the statistics report every 2 sec...
*
         if (g_replay_time-lasttime.ge.2) then  !dump every 2 seconds
           lasttime=g_replay_time
           if(g_stats_blockname.ne.' '.and.
     $          g_stats_output_filename.ne.' ') then
              file = g_stats_output_filename
              call g_sub_run_number(file, gen_run_number)
              ierr = threp(g_stats_blockname,file)
           endif
        endif

        since_cnt= since_cnt+1
        if(since_cnt.GE.10000) then
          print *,' event#',total_event_count,'      trigger#',physics_events
          since_cnt= 0
        endif

        If(ABORT .or. mss.NE.' ') Then
          call G_add_path(here,mss)     !only if problems
          call G_rep_err(ABORT,mss)
        EndIf

        EoF= gen_event_type.EQ.20

        if(gen_run_stopping_event.gt.0 .and. gen_event_ID_number.gt.0) then
          EoF=EoF .or. gen_run_stopping_event.le.sum_analyzed+sum_analyzed_skipped-analyzed_events(4)
          EoF=EoF .or. gen_run_stopping_event.eq.gen_event_ID_number
        EndIf
*
*- Here is where we insert a check for an Remote Proceedure Call (RPC)
*- from another process for CTP to interpret
*
      ENDDO                             !found a problem or end of run

c...  Calibrate HMS and SOS calorimeters.

      if(hdbg_tracks_cal.lt.0) call h_cal_calib(1)

      if(sdbg_tracks_cal.lt.0) call s_cal_calib(1)

c...

      print *,'    -------------------------------------'

      IF(ABORT .or. mss.NE.' ') THEN
        call G_rep_err(ABORT,mss)       !report any errors or warnings
        err= ' '
      ENDIF

       if(rpc_on.ne.0) call thservunset(0,0)

      print *,'    -------------------------------------'
*
* Print out the statistics report one last time...
      if(g_stats_blockname.ne.' '.and.
     $     g_stats_output_filename.ne.' ') then
         file = g_stats_output_filename
         call g_sub_run_number(file, gen_run_number)
         ierr = threp(g_stats_blockname,file)
      endif

      call G_proper_shutdown(ABORT,err) !save files, etc.
      If(ABORT .or. err.NE.' ') Then
        call G_add_path(here,err)       !report any errors or warnings
        call G_rep_err(ABORT,err)
        err= ' '
      EndIf

      call g_ntuple_shutdown(ABORT,err)
      If(ABORT .or. err.NE.' ') Then
        call G_add_path(here,err)       !report any errors or warnings
        call G_rep_err(ABORT,err)
        err= ' '
      EndIf
*
* close charge scalers output file.
      if (g_charge_scaler_filename.ne.' ') close(unit=G_LUN_CHARGE_SCALER)
*
* close epics output file.
      if (g_epics_output_filename.ne.' ') close(unit=G_LUN_EPICS_OUTPUT)

      if (g_preproc_opened) then
        status= evclose(g_preproc_in_hndl)
        if (status.ne.0) write(6,*) 'status for evclose=',status
      endif

      call g_dump_peds
      call h_dump_peds
      call s_dump_peds
      print *
      print *,'Processed:'
      DO i=0,gen_MAX_trigger_types
        If(recorded_events(i).GT.0) Then
          write(mss,'(4x,i12," / ",i8," events of type",i3)')
     &             analyzed_events(i),recorded_events(i),i
          call G_log_message(mss)
        EndIf
      ENDDO
      write(mss,'(i12," / ",i8," total (neglecting scalers)")') sum_analyzed,sum_recorded
      call G_log_message(mss)
      print *,'  for run#',gen_run_number
      if ( syncfilter_on) then
      write(mss,'(i12," number of analyzed skipped ")') sum_analyzed_skipped
      call G_log_message(mss)
      DO i=1,gen_MAX_trigger_types
        If(recorded_events(i).GT.0) Then
          write(mss
     >,'(" events of type:",i3," # skipped for bad sync:",i12)')
     &             i,skipped_badsync_events(i)
          call G_log_message(mss)
        ENDIF
      ENDDO
      DO i=1,gen_MAX_trigger_types
        If(recorded_events(i).GT.0) Then
          write(mss,
     >'("  events of type:",i3," # skipped for low current:",i12)')
     &             i,skipped_lowbcm_events(i)
          call G_log_message(mss)
        ENDIF
      ENDDO
      endif

      ierr=thtreecloseg('all')

* Comment out the following two lines if they cause trouble
      call system
     &  ("kill `ps | grep runstats | awk '{ print $1}'` > /dev/null")

      END

      subroutine engine_command_line(outputflag)

      implicit none
      integer iarg
      character*132 arg
c     integer iargc
c     external iargc
      logical outputflag
*
* Process command line args that set CTP variables
*
      do iarg=1,iargc()
        call getarg(iarg,arg)
        if(index(arg,'=').gt.0) then
          call thpset(arg)
          if (outputflag) write(6,'(4x,a70)') arg(1:70)
        endif
      enddo

      return
      end
