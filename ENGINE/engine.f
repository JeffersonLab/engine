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
      include 'hms_filenames.cmn'
*
      logical problems
      integer total_event_count
      integer physics_events
      integer i,since_cnt,itmp,lastdump
      integer evtype
      integer rpc_pend                  ! # Pending asynchronous RPC requests
*      integer SPAREID
*      parameter (SPAREID=67)
*
      character*80 g_config_environmental_var
      parameter (g_config_environmental_var= 'ENGINE_CONFIG_FILE')
*
      integer*4 iargc,jishft,jiand
*
*--------------------------------------------------------
*
      print *
      print *,'       Hall C Proudly Presents: Analysis Engine - Summer 1995'
*
      err= ' '
      print *
*
      total_event_count= 0                      ! Need to register this
      lastdump=0
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
      call G_initialize(ABORT,err)              !includes a total reset
      IF(ABORT.or.err.NE.' ') THEN
         call G_add_path(here,err)
         call G_rep_err(ABORT,err)
         If(ABORT) STOP
         err= ' '
      ENDIF
*
      call engine_command_line          ! Set CTP vars from command line
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
      DO WHILE(.NOT.problems .and. .NOT.ABORT .and. .NOT.EoF)
        mss= ' '
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
          gen_event_type= evtype        ! reassigned later? 
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
                if(.NOT.problems) then
                  call G_reconstruction(CRAW,ABORT,err) !COMMONs
                  physics_events = physics_events + 1
                  problems= problems .OR. ABORT
                endif
*     
                if(mss.NE.' ' .and. err.NE.' ') then
                  call G_append(mss,' & '//err)
                elseif(err.NE.' ') then
                  mss= err
                endif
*     
                If(.NOT.problems) Then
                  call G_keep_results(ABORT,err) !file away results as
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
            else
              call g_examine_control_event(CRAW,ABORT,err)
            endif
            mss = err
          EndIf
          if(gen_run_hist_dump_interval.gt.0.and.physics_events.ne.lastdump) then
            if((physics_events/gen_run_hist_dump_interval)*
     $           gen_run_hist_dump_interval.eq.physics_events) then
              lastdump=physics_events   ! Wait till next mult. of dump_int.
*
*     Dump scaler report as well
              call g_proper_shutdown(ABORT,err)
              print 112,"Finished dumping histograms/scalers for first"
     &             ,physics_events," events"
 112          format (a,i8,a)
            endif
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
          EoF= EoF .or. gen_run_stopping_event.LE.gen_event_ID_number
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
      print *,' Total number of events=',total_event_count
*
      print *,'     Processed:'
      DO i=0,gen_MAX_trigger_types
        If(gen_run_triggered(i).GT.0) Then
          write(mss,'(i10," events of type",i3)') gen_run_triggered(i),i
          call G_log_message(mss)
        EndIf
      ENDDO
      print *,'  for run #',gen_run_number
*
      END

      subroutine engine_command_line
*
      implicit none
      integer iarg
      character*132 arg
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
