      subroutine usrevent(event, len, flag)
*
*     ONLINE ENGINE - Hall C online Analyzer
*
* $Log: usrevent.f,v $
* Revision 1.2  1994/07/07 15:21:24  cdaq
* (SAW) Add scaler analysis
*
* Revision 1.1  1994/06/16  03:49:10  cdaq
* Initial revision
*
*
      implicit none
      integer*4 event(*), len, flag
*
*     event(*) - Contains the event.
*                event(1)+1 is the total length of the event
*                event(event(1)+2) [First word after event] contains the number
*                of events that are waiting to be analyzed.
*     len      - Amount of space available for the event.  (len-event(1)) is
*                the amount of data that could be appended to the event.
*     flag     - If true (the default), write the event to disk after
*                usrevent returns.
*
      character*8 here
      parameter (here='usrevent')
*
      logical OK, ABORT, problems
      character*800 err
*
      INCLUDE 'gen_run_info.cmn'
*
      integer evtype
*
*     We need to make sure that clear_event doesn't know about event array.
*
      ABORT = .false.
      err = ' '
      call g_clear_event(ABORT,err)
      if(ABORT .or. err.ne.' ') then
         call g_add_path(here,err)
         call dalogmsg(err)
      endif
*
      ABORT = .false.
      err = ' '

      evtype = ishft(event(2),-16)

      if(evtype.le.gen_MAX_trigger_types) then
         call g_examine_physics_event(event,ABORT,err)
*
         if(.NOT.ABORT) then
            call g_reconstruction(event,ABORT,err)
            if(ABORT .or. err.ne.' ') then
               call g_add_path(here,err)
               call dalogmsg(err)
            endif
         
*
            ABORT = .false.
            err = ' '
            call g_keep_results(ABORT,err)
*            if(ABORT .or. err.ne.' ') then
            if(ABORT) then              ! Don't show warnings.
               call g_add_path(here,err)
               call dalogmsg(err)
            endif
*     
         endif
      else                              ! Analyze scalers
* Assume for now that all other events are scalers
         call g_analyze_scalers(event,ABORT,err)
      endif

      return
      end




