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
*-    $Log$
*-    Revision 1.13  1995/03/13 18:11:05  cdaq
*-    (JRA) Write scaler report when histograms are dumped at intervals
*-
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
      integer i,since_cnt,itmp
      integer evtype
      integer rpc_pend                  ! # Pending asynchronous RPC requests
      integer ierr
*      integer SPAREID
*      parameter (SPAREID=67)
*
      character*80 g_config_environmental_var
      parameter (g_config_environmental_var= 'ENGINE_CONFIG_FILE')
*
*--------------------------------------------------------
*
      type *
      type *,'                hall C analysis engine October 1994'
      type *
*
      err= ' '
      type *
*
      total_event_count= 0                      ! Need to register this
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
      call G_init_filenames(ABORT,err,g_config_environmental_var)
      if(ABORT.or.err.ne.' ') then
         call G_add_path(here,err)
         call G_rep_err(ABORT,err)
         If(ABORT) STOP
         err= ' '
      ENDIF
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
          evtype = ishft(craw(2),-16)
          gen_event_type= evtype        ! reassigned later? 
          if(iand(CRAW(2),'FFFF'x).eq.'10CC'x) then ! Physics event
*     
            if(evtype.le.gen_MAX_trigger_types) then
                  
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
          EndIf
          if(gen_run_hist_dump_interval.gt.0) then
            if((total_event_count/gen_run_hist_dump_interval)
     $           *gen_run_hist_dump_interval.eq.total_event_count)
     $           then
              print *,"Dumping histograms at event ",total_event_count
              call g_dump_histograms(ABORT,err)
*
*     Dump scaler report as well
              call h_scin_eff_shutdown(ABORT,err)
              call h_cal_eff_shutdown(ABORT,err)
              ierr = threp(g_report_blockname,g_report_output_filename)
              if(ierr.ne.0) then
                print *,'ierr=',ierr,', error dumping report'
c                bad_report = .true.
c                err = 'threpa failed to append report'
              endif
            endif
          endif
        endif
*
        since_cnt= since_cnt+1
        if(since_cnt.GE.1000) then
          type *,' event#',total_event_count,'  ',ABORT
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
      type *,'    -------------------------------------'
*
      IF(ABORT .or. mss.NE.' ') THEN
        call G_rep_err(ABORT,mss)       !report any errors or warnings
        err= ' '
      ENDIF
*
      if(rpc_on.ne.0) call thservunset(0,0)
*
      type *,'    -------------------------------------'
*
      call G_proper_shutdown(ABORT,err) !save files, etc.
      If(ABORT .or. err.NE.' ') Then
        call G_add_path(here,err)       !report any errors or warnings
        call G_rep_err(ABORT,err)
        err= ' '
      EndIf
*
      type *
      type *,'      total number of events=',total_event_count
      type *
*
      type *,' Processed:'
      DO i=0,gen_MAX_trigger_types
        If(gen_run_triggered(i).GT.0) Then
          write(mss,'(i10," events of type",i3)') gen_run_triggered(i),i
          call G_log_message(mss)
        EndIf
      ENDDO
*
      END








