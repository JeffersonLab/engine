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
*-    Revision 1.2  1994/02/11 18:32:06  cdaq
*-    Split off CTP variables registration from initialize routines
*-
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
      logical problems,interrupt,initOK
      integer total_event_count
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
      initOK= .NOT.ABORT
      if(ABORT.or.err.ne.' ') then
        call G_add_path(here,err)
        call G_rep_err(ABORT,err)
        If(initOK) err= ' '
      ENDIF
*
      call G_initialize(ABORT,err)              !includes a total reset
      initOK= .NOT.ABORT
      IF(ABORT.or.err.NE.' ') THEN
        call G_add_path(here,err)
        call G_rep_err(ABORT,err)
        If(initOK) err= ' '
      ENDIF
*
      IF(.NOT.ABORT) type *,'  ........G_initialize OK.............'
      type *
*
      problems= ABORT
*
      DO WHILE(.NOT.problems .and. .NOT.ABORT)
*
        call G_clear_event(ABORT,err)           !clear out old data
        problems= problems .OR. ABORT
*
        If(.NOT.problems) Then
          call G_get_next_event(ABORT,err)      !get and store 1 event 
          problems= problems .OR. ABORT 
          if(.NOT.ABORT) total_event_count= total_event_count+1
*
          type *,' event#',total_event_count,'  ',ABORT
*
        EndIf
*
        If(.NOT.problems) Then               !reconstruct event into
          call G_reconstruction(ABORT,err)      !COMMONs
          problems= problems .OR. ABORT
        EndIf
*
        If(.NOT.problems) Then
          call G_keep_results(ABORT,err)     !file away results as
          problems= problems .OR. ABORT      !specified by interface
        EndIf
*
        If(ABORT .or. err.NE.' ') Then
          call G_add_path(here,err)  !only if problems
          call G_log_message(err)
        EndIf
*
*- Here is where we insert a check for an Remote Proceedure Call (RPC) 
*- from another process for CTP to interpret
*
      ENDDO                                   !found a problem
*
      IF(ABORT .or. err.NE.' ') THEN
        call G_rep_err(ABORT,err)           !report any errors or warnings
        err= ' '
      ENDIF
*
      IF(initOK) THEN                       !only if initialized OK
        call G_proper_shutdown(ABORT,err)          !save files, etc.
        If(ABORT .or. err.NE.' ') Then
          call G_add_path(here,err)          !report any errors or warnings
          call G_rep_err(ABORT,err)
          err= ' '
        EndIf
      ENDIF
*
      type *
      type *,'      total number of events=',total_event_count
      type *
*
      STOP
      END
