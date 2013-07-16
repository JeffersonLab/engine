*--------------------------------------------------------------
*
*- standalone DISPLAY for hall C
*
* $Log: evdisplay.f,v $
* Revision 1.7  1998/12/01 21:47:10  saw
* (SAW) Put correct number of arguments in g_rep_err call
*
* Revision 1.6  1996/11/22 15:53:10  saw
* (SAW) Fix typo
*
* Revision 1.5  1996/09/04 16:40:13  saw
* (SAW) Reorder data statements for f2c compatibility
*
* Revision 1.4  1996/01/24 16:28:28  saw
* (DVW) Add code for automatic redisplay mode
*
* Revision 1.3  1996/01/17 16:30:02  cdaq
* (SAW) Adjust RPC nums for new online analyzer (DD system).
*       Add menu for view selection
*
* Revision 1.2  1995/09/18 13:47:44  cdaq
* (DVW, SAW) Reorganize
*
* Revision 1.1  1995/03/14  21:25:27  cdaq
* Initial revision
*
*--------------------------------------------------------------
      IMPLICIT NONE
*
      character*9 here
      parameter (here= 'evdisplay')
*
      INCLUDE 'gen_pawspace.cmn'
*
      INCLUDE 'gen_filenames.cmn'
*
      INCLUDE 'gen_run_info.cmn'
      INCLUDE 'gen_event_info.cmn'
*
      INCLUDE 'gen_one_ev_info.cmn'
*
      logical FAIL,QUIT
      character*800 why
      character*132 line
      logical ABORT
      character*800 err
      integer i,j
*
      integer*4 dispmode                ! to display every 10 seconds or not
      character*1 spect
      integer*4 view, newview
      integer*4 revdis_ask
      logical isdata
      INCLUDE 'gen_one_ev_info.dte'
*******************************************************************
*
      PRINT *
      PRINT *,'       Standalone DISPLAY for hall C'
      PRINT *,'     R.Ent,S.Wood, & K.Beard  Oct.1994'
      PRINT *,'       and Derek van Westrum Jul 1995'
      PRINT *
      PRINT *,'****************************************************'
      PRINT *,'*                                                  *'
      PRINT *,'*       Confused?  Don''t be.  Read the file        *'
      PRINT *,'*                                                  *'
      PRINT *,'*   ~cdaq/documents/analysis_code/evdisplay.help   *'
      PRINT *,'*                                                  *'
      PRINT *,'****************************************************'
      PRINT *
      PRINT *,' 0: Connect to offline replay'
      PRINT *,' 1: Connect online analyzer'
      PRINT *,' Other: A non-default RPC Program ID and version'
      READ(5,'(2i30)') i,j
      if(i.eq.0) then
        gen_display_server_RPCprgmID = '2c0daFF8'x   !default offline
        gen_display_server_RPCversionID = 1 ! default offline
        call getenv("HOST",gen_display_server_machine)
      else if(i.eq.1) then
c        gen_display_server_RPCprgmID = '2c0da005'x   !default online
        gen_display_server_RPCprgmID = '2c0daFF8'x   !default online
        gen_display_server_RPCversionID = 2 ! default online
        gen_display_server_machine = 'cdaq2.cebaf.gov'
      else
        gen_display_server_RPCprgmID = i
        gen_display_server_RPCversionID = j
        call getenv("HOST",gen_display_server_machine)
      endif
*
      PRINT *
      PRINT *,' 0: to choose events manually (default)'
      PRINT *,' 1:  to display every 10 seconds'
      READ(5,'(i)') dispmode
      IF (dispmode.NE.1) dispmode=0

      PRINT *
      PRINT *,' Enter the name of the machine running "engine" or CODA:'
      i=index(gen_display_server_machine,' ')
      if(i.gt.1) i = i-1
      WRITE(6,'($,a)') '[cdaq1,cdaq2,hallc1,hallc2,cebafh, ... ['//
     $     gen_display_server_machine(1:i)//']: '
      READ(5,'(a)') line
      IF(line.EQ.' '.and.gen_display_server_machine.eq.' ') THEN
        why= ':machine name must be specified!'
        call G_add_path(here,why)
        call G_rep_err(.TRUE.,why)
        STOP
      ELSE if(line.ne.' ') then
        call NO_comments(line)
        gen_display_server_machine= line
      ENDIF
*
 100  print *
      print *, 'Type "h" for the HMS, or "s" for the SOS:'
      read *, spect
      if(spect.eq.'S') spect='s'
      if(spect.eq.'H') spect='h'
      if ((spect .ne. 's') .and. (spect .ne. 'h')) then
        print*, 'Invalid option.  Please type "h" or "s".'
        goto 100
      endif
*
      print *,"Server Program #=",gen_display_server_RPCprgmID
*
      PRINT *
      PRINT *,' display type? [1= Xwindow[def.], 7878=GraphOn]'
      READ(5,'(i30)') graph_io_dev
      if(graph_io_dev.eq.0) graph_io_dev = 1
*
      call G_register_variables(FAIL,why)
      IF(FAIL) THEN
        call G_add_path(here,why)
        call G_rep_err(FAIL,why)
        STOP
      ENDIF
      PRINT *,' G_register_variables OK'
      PRINT *
*      CALL r_one_ev_io

*
*
*
*
      call revdis_init(FAIL,why)  ! Build lists of variables to get

      IF(FAIL) THEN
        call G_add_path(here,why)
        call G_rep_err(FAIL,why)
        STOP
      ELSE IF(g_config_filename.EQ.' ') THEN
        PRINT *
        PRINT *,' rpc/CTP FAILURE TO COMMUNICATE!'
        PRINT *
        STOP
      ENDIF

*
*
*     Do the initialization that g_initialize was supposed to do
*
      call GZEBRA(NGBANK)
      call hlimit (-NHBOOK)             ! init HBOOK memory
      if (spect .eq. 'h') then
        call h_initialize(ABORT,err)
      elseif (spect .eq. 's') then
        call s_initialize(ABORT,err)
      endif
*      call c_initialize(ABORT,err)
      call g_reset_event(ABORT,err)
*     
      if(graph_io_dev .ne. 0) call hplint(graph_io_dev)
      if (graph_io_dev .eq. 0) then
        call hplint(0)                  ! init graphics
        call igmeta(-8,-111)            ! init HIGZ meta junk
      endif

      if (spect .eq. 'h') then
        CALL h_uginit
      elseif (spect .eq. 's') then
        CALL s_uginit
      endif
*
      PRINT *,' Connected to a Hall C analyzer at '
     $     ,gen_display_server_machine(1:30)
      print *,' Server analyzer has the label'
      print *,' '
      print *,g_label
      print *,' '
      PRINT *,' ............begin loop......................'
      print *,' '
      print *,' '
*
      QUIT= .FALSE.

      view = 1
      isdata = .false.

      if(dispmode.ne.1) then
        DO WHILE (.NOT.QUIT)
*
          PRINT *,'Run Number = ',gen_run_number,
     $         '  Event Number = ',gen_event_ID_number
          PRINT *,' Enter a CTP condition for the next event (?=help,1=any).'
          newview = revdis_ask(view)
          if(newview.lt.0) then
            QUIT = .TRUE.
            isdata = .false.            ! Don't try to view
          else if(newview.eq.0) then    ! Get a new event
            call revdis_getev(FAIL,why)
            If(FAIL) Then
              call G_rep_err(FAIL,why)
            Endif
            write(6,'("Run",i6,", event ID",i7," sequence",i7)')
     $           gen_run_number,gen_event_ID_number, gen_event_sequence_N
            if(spect.eq.'h') then
              call h_one_ev_generate
            else if(spect.eq.'s') then
              call s_one_ev_generate
            endif
            isdata = .true.
          else
            view = newview
          endif
          if(isdata) then               ! There is an event to display
            if(spect.eq.'h') then
              call h_one_ev_display(view)
            else if(spect.eq.'s') then
              call s_one_ev_display(view)
            endif
          endif
*     
        ENDDO
      elseif (dispmode.EQ.1) then
        DO WHILE (.NOT.QUIT)
          call system('sleep 5')
*
          PRINT *,'Run Number = ',gen_run_number,
     $         '  Event Number = ',gen_event_ID_number
          PRINT *,' Enter a CTP condition for the next event (?=help,1=any).'
*            newview = revdis_ask(view)
          newview = 0
          if(newview.lt.0) then
            QUIT = .TRUE.
            isdata = .false.            ! Don't try to view
          else if(newview.eq.0) then    ! Get a new event
            call revdis_getev(FAIL,why)
            If(FAIL) Then
              call G_rep_err(FAIL,why)
            Endif
            write(6,'("Run",i6,", event ID",i7," sequence",i7)')
     $           gen_run_number,gen_event_ID_number, gen_event_sequence_N
            if(spect.eq.'h') then
              call h_one_ev_generate
            else if(spect.eq.'s') then
              call s_one_ev_generate
            endif
            isdata = .true.
          else
            view = newview
          endif
          if(isdata) then               ! There is an event to display
            if(spect.eq.'h') then
              call h_one_ev_display(view)
            else if(spect.eq.'s') then
              call s_one_ev_display(view)
            endif
          endif
*
        ENDDO
      endif
*
 99   continue
      call IGEND                        !properly terminate HIGZ and any&all metafiles
      STOP
      END
