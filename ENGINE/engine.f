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
*-    Revision 1.6  1994/06/15 14:27:30  cdaq
*-    (SAW) Actually add call to g_examine_physics_event
*-
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
      logical OK,ABORT
      character*800 err
*
      include 'gen_filenames.cmn'
      include 'gen_craw.cmn'
*
      logical problems
      integer total_event_count
      integer i
* 
      character*80 g_config_environmental_var
      parameter (g_config_environmental_var= 'ENGINE_CONFIG_FILE')
*
      EXTERNAL thwhalias,thbook
*
*--------------------------------------------------------
*
      err= ' '
      type *
*
      total_event_count= 0                      ! Need to register this

      call g_register_variables(ABORT,err)
      if(ABORT.or.err.ne.' ') then
         call G_add_path(here,err)
         call G_rep_err(ABORT,err)
         If(.not. ABORT) err= ' '
      ENDIF
*
      call G_init_filenames(ABORT,err,g_config_environmental_var)
      if(ABORT.or.err.ne.' ') then
         call G_add_path(here,err)
         call G_rep_err(ABORT,err)
         If(ABORT) then
            stop
         else
            err= ' '
         endif
      ENDIF
*
      call G_decode_init(ABORT,err)
      if(ABORT.or.err.ne.' ') then
         call G_add_path(here,err)
         call G_rep_err(ABORT,err)
         If(ABORT) then
            stop
         else
            err= ' '
         endif
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
         If(ABORT) then
            stop
         else
            err= ' '
         endif
      endif
*
      call G_initialize(ABORT,err)              !includes a total reset
      IF(ABORT.or.err.NE.' ') THEN
         call G_add_path(here,err)
         call G_rep_err(ABORT,err)
         If(ABORT) then
            stop
         else
            err= ' '
         endif
      ENDIF
*
*-zero entire event buffer
*
      DO i=1,LENGTH_CRAW
         CRAW(i)= 0
      ENDDO

*
      problems= .false.
*
      DO WHILE(.NOT.problems .and. .NOT.ABORT)
*
         call G_clear_event(ABORT,err)          !clear out old data
         problems= problems .OR. ABORT
*
         If(.NOT.problems) Then
            call G_get_next_event(ABORT,err)    !get and store 1 event 
            problems= problems .OR. ABORT 
            if(.NOT.ABORT) total_event_count= total_event_count+1
*
*            type *,' event#',total_event_count,'  ',ABORT
*
         EndIf
*
*     Check if this is a physics event or a CODA control event.
*
         if(.not.problems) then
            if(iand(CRAW(2),'FFFF'x).eq.'10CC'x) then ! Physics event
*
*     Need to add in KB's code for selecting which event types to analyze.
*
               call g_examine_physics_event(CRAW,ABORT,err)
               problems = problems .or.ABORT
               
               if(.NOT.problems) Then
                  call G_reconstruction(CRAW,ABORT,err) !COMMONs
                  problems= problems .OR. ABORT
               endif

               If(.NOT.problems) Then
                  call G_keep_results(ABORT,err) !file away results as
                  problems= problems .OR. ABORT !specified by interface
               EndIf
            else
               call g_examine_control_event(CRAW,ABORT,err)
            EndIf
         endif
*
*
         If(ABORT .or. err.NE.' ') Then
            call G_add_path(here,err)           !only if problems
            call G_log_message(err)
         EndIf
*
*- Here is where we insert a check for an Remote Proceedure Call (RPC) 
*- from another process for CTP to interpret
*
      ENDDO                                   !found a problem
*
      IF(ABORT .or. err.NE.' ') THEN
         call G_rep_err(ABORT,err)              !report any errors or warnings
         err= ' '
      ENDIF
*
      call G_proper_shutdown(ABORT,err)         !save files, etc.
      If(ABORT .or. err.NE.' ') Then
         call G_add_path(here,err)              !report any errors or warnings
         call G_rep_err(ABORT,err)
         err= ' '
      EndIf
*
      type *
      type *,'      total number of events=',total_event_count
      type *
*
      STOP
      END
