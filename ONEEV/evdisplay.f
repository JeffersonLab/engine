*--------------------------------------------------------------
*
*- standalone DISPLAY for hall C
*
* $Log$
* Revision 1.1  1995/03/14 21:25:27  cdaq
* Initial revision
*
*--------------------------------------------------------------
      IMPLICIT NONE
*
      character*6 here
      parameter (here= 'revdis')
*
      INCLUDE 'gen_pawspace.cmn'
      INCLUDE 'one_ev_io.cmn'
*
      INCLUDE 'gen_filenames.cmn'
*
      INCLUDE 'gen_run_info.cmn'
      INCLUDE 'gen_event_info.cmn'
*
      INCLUDE 'gen_display_info.cmn'
      INCLUDE 'gen_display_info.dte'
*
      include 'params.inc'
*
      logical FAIL,QUIT
      character*800 why
      character*132 line
      logical ABORT
      character*800 err
      integer i,j
*
*******************************************************************
*
      PRINT *
      PRINT *,'    standalone DISPLAY for hall C'
      PRINT *,'     R.Ent,S.Wood, & K.Beard  Oct.1994'
      PRINT *
      PRINT *,' You need to specify the process and the machine you'
      PRINT *,' with which you want to connect this display process.'
      PRINT *,' Also, if you are using an Xwindow display'
      PRINT *,' you may need to run PAW once/session to get things to '
      PRINT *,' work correctly.'
      PRINT *
      PRINT *
      PRINT *,' Enter the name of the machine running "engine" or CODA:'
      PRINT *,' [cdaq1,cdaq2,hallc1,hallc2,cebafh, number; no default]:'
      READ(5,'(a)') line
      IF(line.EQ.' ') THEN
        why= ':machine name must be specified!'
        call G_add_path(here,why)
        call G_rep_err(why)
        STOP
      ELSE
        call NO_comments(line)
        gen_display_server_machine= line
      ENDIF
*
      PRINT *,' 0: Connect to offline replay'
      PRINT *,' 1: Connect online analyzer'
      PRINT *,' Other: A non-default RPC Program ID and version'
      READ(5,'(2i30)') i,j
      if(i.eq.0) then
        gen_display_server_RPCprgmID = '2c0daFF8'x   !default offline
        gen_display_server_RPCversionID = 1 ! default offline
      else if(i.eq.1) then
        gen_display_server_RPCprgmID = '2c0da005'x   !default online
        gen_display_server_RPCversionID = 0 ! default online
      else
        gen_display_server_RPCprgmID = i
        gen_display_server_RPCversionID = j
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
        call G_rep_err(why)
        STOP
      ENDIF
      PRINT *,' G_register_variables OK'
      PRINT *
*      CALL r_one_ev_io

      call revdis_init(FAIL,why)     ! Build lists of variables to get

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
      call hlimit (-NHBOOK)         ! init HBOOK memory
      call h_initialize(ABORT,err)
c      call s_initialize(ABORT,err)
      call c_initialize(ABORT,err)
      call g_reset_event(ABORT,err)
      CALL h_one_ev_init
*
      PRINT *,' Connected to a Hall C analyzer at '
     $     ,gen_display_server_machine(1:30)
      print *,' Server analyzer has the label'
      print *,' '
      print *,g_label
      print *,' '
      PRINT *,' ............begin loop......................'
*
      QUIT= .FALSE.

      DO WHILE (.NOT.QUIT)
*
        PRINT *,' Next CTP condition for an event (?=help,1=any)?'
        READ(5,'(a)',ERR=99,END=99) line
        IF(line.EQ.'%EXIT' .or. line.EQ.'%QUIT') THEN
          QUIT= .TRUE.
        else
          call revdis_define(line,FAIL,why)
          if(.NOT.FAIL) then
            call revdis_getev(FAIL,why)
            If(FAIL) Then
              call G_rep_err(FAIL,why)
            Else
              write(6,'("Run",i6,", event ID",i7," sequence",i7)')
     $             gen_run_number,gen_event_ID_number, gen_event_sequence_N
              call h_one_ev_display
            EndIf
          endif
        endif
*
      ENDDO
*
 99   continue
      call IGEND                !properly terminate HIGZ and any&all metafiles
      STOP
      END
