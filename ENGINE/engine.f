* PROGRAM Engine
*--------------------------------------------------------
*-       Prototype C analysis routine
*-
*- This program is a first draft of an analysis shell for CEBAF
*- hall C.  It gets all of its instructions via the CTP package
*- and a temporary mickey-mouse interface.
*- Loops through data until it encounters an error.
*-
*-   Created  18-Nov-1993   Kevin B. Beard, Hampton Univ.
* $Log$
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
      IMPLICIT NONE
      SAVE
*
      character*6 here
      parameter (here= 'Engine')
*
      logical ABORT,EoF
      character*800 err,mss
*
      include 'gen_filenames.cmn'
      include 'gen_craw.cmn'
      include 'gen_run_info.cmn'
      include 'gen_event_info.cmn'
      include 'gen_run_pref.cmn'
      include 'gen_routines.dec'
      include 'gen_scalers.cmn'
c      include 'hms_filenames.cmn'
      include 'gen_data_structures.cmn'
      include 'hms_data_structures.cmn'
      include 'sos_data_structures.cmn'
*
      logical problems
      integer total_event_count
      integer physics_events
      integer analyzed_events(0:gen_max_trigger_types)
      integer sum_analyzed
      integer recorded_events(0:gen_max_trigger_types)
      integer sum_recorded
      integer i,since_cnt,itmp,lastdump
      integer evtype
      integer rpc_pend                  ! # Pending asynchronous RPC requests
*      integer SPAREID
*      parameter (SPAREID=67)
*
      character*80 g_config_environmental_var
      parameter (g_config_environmental_var= 'ENGINE_CONFIG_FILE')
*
      integer*4 jishft,jiand
*
      integer ierr
      character*132 file
      character*20 groupname
      character*132 system_string
*
c      real*4 ebeam,phms,thms,psos,tsos
*
      integer start_time,lasttime
      integer time
      external time
*
*
*--------------------------------------------------------
*
      print *
      print *,'    Hall C Proudly Presents: PHYSICS Analysis Engine - Winter 1995'
*
      err= ' '
      print *
*
      total_event_count= 0                      ! Need to register this
      lastdump=0
      do i=0,gen_max_trigger_types
        analyzed_events(i)=0
        recorded_events(i)=0
      enddo
      sum_analyzed=0
      sum_recorded=0
*
      rpc_on=0                          ! RPC servicing off by default
      rpc_control=-1                    ! If RPC on, don't block by default
*
      call g_register_variables(ABORT,err)
      if(ABORT.or.err.ne.' ') then
         call G_add_path(here,err)
         call G_rep_err(ABORT,err)
         If(ABORT) STOP
         err= ' '
      ENDIF
*
      g_config_filename = ' '
*
      call engine_command_line          ! Set CTP vars from command line
*       
      call G_init_filenames(ABORT,err,g_config_environmental_var)
      if(ABORT.or.err.ne.' ') then
         call G_add_path(here,err)
         call G_rep_err(ABORT,err)
         If(ABORT) STOP
         err= ' '
      ENDIF
*
      call engine_command_line          ! Set CTP vars from command line
*
*     If there is a g_ctp_database_filename set, pass the run number
*     to it to set CTP variables
*
      if(.not.ABORT.and.g_ctp_database_filename.ne.' ') then
        call g_ctp_database(ABORT, err
     $       ,gen_run_number, g_ctp_database_filename)
        IF(ABORT) THEN
          call G_add_path(here,err)
        endif
      ENDIF
*       
      call engine_command_line          ! Set CTP vars from command line
*
      call G_decode_init(ABORT,err)
      if(ABORT.or.err.ne.' ') then
         call G_add_path(here,err)
         call G_rep_err(ABORT,err)
         If(ABORT) STOP
         err= ' '
      endif
*
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
      g_data_source_opened = .false.     !not opened yet
      g_data_source_in_hndl= 0           !none
      call G_open_source(ABORT,err)
      if(ABORT.or.err.ne.' ') then
         call G_add_path(here,err)
         call G_rep_err(ABORT,err)
         If(ABORT) STOP
         err= ' '
      endif
*
      call engine_command_line          ! Set CTP vars from command line
*
* Print out the statistics report once...
      if(g_stats_blockname.ne.' '.and.
     $     g_stats_output_filename.ne.' ') then
         file = g_stats_output_filename
         call g_sub_run_number(file, gen_run_number)
         ierr = threp(g_stats_blockname,file)
      endif
*
*     Comment out the following three lines if they cause trouble or
*     if wish is unavailable.
      write(system_string,*) 'runstats ',g_stats_output_filename,
     $     ' ',gen_run_number, '> /dev/null 2>&1 &'
      call system(system_string)
*
*-zero entire event buffer
*
      DO i=1,LENGTH_CRAW
         CRAW(i)= 0
      ENDDO
*
* input number of events to analyze and hist dumping interval
*
c      write(6,'(/,"Starting Event #: [",i7,"] "$)') gen_run_starting_event
c      read(5,'(i30)') itmp
c      if (itmp.ne.0) gen_run_starting_event=itmp
c      write(6,'("Stopping Event #: [",i7,"] "$)') gen_run_stopping_event
c      read(5,'(i30)') itmp
c      if (itmp.ne.0) gen_run_stopping_event=itmp
c      write(6,'("Hist dump intrvl: (-1=@end) [",i7,"] "$)') gen_run_hist_dump_interval
c      read(5,'(i30)') itmp
c      if (itmp.ne.0) gen_run_hist_dump_interval=itmp
*
      since_cnt= 0
      problems= .false.
      EoF = .false.
*
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
*
      start_time=time()
      lasttime=0.
*
      DO WHILE(.NOT.problems .and. .NOT.ABORT .and. .NOT.EoF)
        mss= ' '
        g_real_time=time()-start_time
*
        call G_clear_event(ABORT,err)   !clear out old data
        problems= problems .OR. ABORT
*
        if(mss.NE.' ' .and. err.NE.' ') then
          call G_append(mss,' & '//err)
        elseif(err.NE.' ') then
          mss= err
        endif
*
        If(.NOT.problems) Then
          call G_get_next_event(ABORT,err) !get and store 1 event 
          problems= problems .OR. ABORT 
          if(.NOT.ABORT) total_event_count= total_event_count+1
*     
        EndIf
*
        if(mss.NE.' ' .and. err.NE.' ') then
          call G_append(mss,' & '//err)
        elseif(err.NE.' ') then
          mss= err
        endif
*
*     Check if this is a physics event or a CODA control event.
*
        if(.not.problems) then
          evtype = jishft(craw(2),-16)
          if(evtype.le.gen_MAX_trigger_types) then
            recorded_events(evtype)=recorded_events(evtype)+1
            sum_recorded=sum_recorded+1
          endif
          gen_event_type= evtype        ! reassigned later? 

          if (evtype.eq.130) then       !run info event (get e,p,theta)
c            call g_extract_kinematics(ebeam,phms,thms,psos,tsos)
c            if (cpbeam .ge. 7. .and. ebeam.le.7.) then !sometimes ebeam in MeV
c              cpbeam=abs(ebeam)
c              write(6,*) 'cpbeam=',abs(ebeam),' GeV'
c            endif
c            if (hpcentral .ge. 7.) then
c              write(6,*) 'hpcentral=',abs(phms),' GeV/c'
c              hpcentral=abs(phms)
c            endif
c            if (htheta_lab .le. 0.) then
c              write(6,*) 'htheta_lab=',abs(thms),' deg.'
c              htheta_lab=abs(thms)*3.14159265/180.
c            endif
c            if (spcentral .ge. 7.) then
c              write(6,*) 'spcentral=',abs(psos),' GeV/c'
c              spcentral=abs(psos)
c            endif
c            if (stheta_lab .le. 0.) then
c              write(6,*) 'stheta_lab=',abs(tsos),' deg.'
c              stheta_lab=abs(tsos)*3.14159265/180.
c            endif
          endif


          if(jiand(CRAW(2),'FFFF'x).eq.'10CC'x) then ! Physics event
*     
            if(evtype.le.gen_MAX_trigger_types .and.
     $           gen_run_enable(evtype-1).ne.0) then
                  
              call g_examine_physics_event(CRAW,ABORT,err)
              problems = problems .or.ABORT
*     
              if(mss.NE.' ' .and. err.NE.' ') then
                call G_append(mss,' & '//err)
              elseif(err.NE.' ') then
                mss= err
              endif
*     
              IF(gen_run_starting_event.LE.gen_event_ID_number) THEN
                if(gen_run_starting_event.eq.gen_event_id_number)
     &               start_time=time()  !reset start time for analysis rate
                if(.NOT.problems) then
                  call G_reconstruction(CRAW,ABORT,err) !COMMONs
                  physics_events = physics_events + 1
                  analyzed_events(evtype)=analyzed_events(evtype)+1
                  sum_analyzed=sum_analyzed+1
                  problems= problems .OR. ABORT
                endif
*     
                if(mss.NE.' ' .and. err.NE.' ') then
                  call G_append(mss,' & '//err)
                elseif(err.NE.' ') then
                  mss= err
                endif
*
                groupname=' '
                if (gen_event_type.eq.1) then
                  groupname='hms'
                else if (gen_event_type.eq.2) then
                  groupname='sos'
                else if (gen_event_type.eq.3) then
                  groupname='both'
                else if (gen_event_type.eq.4) then
                  start_time=time()     !reset start time for analysis rate
                else
                  write(6,*) 'gen_event_type= ',gen_event_type,' for call to g_keep_results'
                endif
*
                If(.NOT.problems .and. groupname.ne.' ') Then
                  call G_keep_results(groupname,ABORT,err) !file away results as
                  problems= problems .OR. ABORT !specified by interface
                EndIf
*
                if(mss.NE.' ' .and. err.NE.' ') then
                  call G_append(mss,' & '//err)
                elseif(err.NE.' ') then
                  mss= err
                endif

*
*- Here is where we insert a check for an Remote Proceedure Call (RPC) 
*- from another process for CTP to interpret
*
                if(rpc_on.ne.0) then
                  if(rpc_pend.eq.0.and.rpc_control.eq.0) then
                    do while(rpc_pend.eq.0.and.rpc_control.eq.0)
                      call thservone(-1) !block until one RPC request serviced
                      rpc_pend = thcallback()
                    enddo
                  else
                    call thservone(0)   !service one RPC requests
                    rpc_pend = thcallback()
                  endif
                  if(rpc_pend.lt.0) rpc_pend = 0 ! Last thcallback took care of all
                                        ! outstanding requests
                  if(rpc_control.gt.0) rpc_control = rpc_control - 1
                endif


              ENDIF
            endif
          Else
            if(evtype.eq.129) then
              call g_analyze_scalers(CRAW,ABORT,err)
*  Dump report at first scaler event AFTER hist_dump_interval to keep hardware
*  and software scalers in sync.
              if((physics_events-lastdump).ge.gen_run_hist_dump_interval.and.
     &            gen_run_hist_dump_interval.gt.0) then
                lastdump=physics_events   ! Wait for next interval of dump_int.
                call g_proper_shutdown(ABORT,err)
                print 112,"Finished dumping histograms/scalers for first"
     &               ,physics_events," events"
 112            format (a,i8,a)
              endif
            else if (evtype.eq.133) then  !SAW's new go_info events
              call g_examine_go_info(CRAW,ABORT,err)
            else
              call g_examine_control_event(CRAW,ABORT,err)
            endif
            mss = err
          EndIf
        endif
*
*Now write the statistics report every 100 events...
*
ccc        if((physics_events/200)*200.eq.physics_events) then
         if (g_real_time-lasttime.ge.2.) then  !dump every 2 seconds
           lasttime=g_real_time
           if(g_stats_blockname.ne.' '.and.
     $          g_stats_output_filename.ne.' ') then
              file = g_stats_output_filename
              call g_sub_run_number(file, gen_run_number)
              ierr = threp(g_stats_blockname,file)
           endif
        endif
*
        since_cnt= since_cnt+1
        if(since_cnt.GE.1000) then
          print *,' event#',total_event_count,'  ',ABORT
          since_cnt= 0
        endif
*
        If(ABORT .or. mss.NE.' ') Then
          call G_add_path(here,mss)     !only if problems
          call G_rep_err(ABORT,mss)
        EndIf
*
        EoF= gen_event_type.EQ.20
*
        If(gen_run_stopping_event.GT.0 .and. 
     &       gen_event_ID_number.GT.0) Then
          EoF= EoF .or. gen_run_stopping_event.LE.sum_analyzed
        EndIf
*
*- Here is where we insert a check for an Remote Proceedure Call (RPC) 
*- from another process for CTP to interpret
*
      ENDDO                             !found a problem or end of run
*
      print *,'    -------------------------------------'
*
      IF(ABORT .or. mss.NE.' ') THEN
        call G_rep_err(ABORT,mss)       !report any errors or warnings
        err= ' '
      ENDIF
*
      if(rpc_on.ne.0) call thservunset(0,0)
*
      print *,'    -------------------------------------'
*
* Print out the statistics report one last time...
      if(g_stats_blockname.ne.' '.and.
     $     g_stats_output_filename.ne.' ') then
         file = g_stats_output_filename
         call g_sub_run_number(file, gen_run_number)
         ierr = threp(g_stats_blockname,file)
      endif
*
      call G_proper_shutdown(ABORT,err) !save files, etc.
      If(ABORT .or. err.NE.' ') Then
        call G_add_path(here,err)       !report any errors or warnings
        call G_rep_err(ABORT,err)
        err= ' '
      EndIf
*
      call g_ntuple_shutdown(ABORT,err)
      If(ABORT .or. err.NE.' ') Then
        call G_add_path(here,err)       !report any errors or warnings
        call G_rep_err(ABORT,err)
        err= ' '
      EndIf
*
      call h_dump_peds
      call s_dump_peds
*
      print *,'Processed:'
      DO i=0,gen_MAX_trigger_types
        If(recorded_events(i).GT.0) Then
          write(mss,'(4x,i12," / ",i8," events of type",i3)')
     &             analyzed_events(i),recorded_events(i),i
          call G_log_message(mss)
        EndIf
      ENDDO
      write(mss,'(i12," / ",i8," total")') sum_analyzed,sum_recorded
      call G_log_message(mss)
      print *,'  for run#',gen_run_number
*
*     Comment out the following two lines if they cause trouble
      call system
     &  ("kill `ps | grep runstats | awk '{ print $1}'` > /dev/null 2>&1")
*
      END

      subroutine engine_command_line
*
      implicit none
      integer iarg
      character*132 arg
      integer iargc
      external iargc
*
*     Process command line args that set CTP variables
*
      do iarg=1,iargc()
        call getarg(iarg,arg)
        if(index(arg,'=').gt.0) then
          call thpset(arg)
        endif
      enddo
*
      return
      end
