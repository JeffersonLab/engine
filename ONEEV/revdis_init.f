      subroutine revdis_init(ABORT,err)
*--------------------------------------------------------
* $Log$
* Revision 1.1  1995/03/14 21:26:16  cdaq
* Initial revision
*
*--------------------------------------------------------
*
      IMPLICIT NONE
      SAVE
*
      character*11 here
      parameter (here= 'revdis_init')
*
      logical ABORT
      character*(*) err
*
*      INCLUDE 'one_ev_io.cmn'
      INCLUDE 'params.inc'
      INCLUDE 'gen_display_cern.cmn'
*
      INCLUDE 'gen_display_info.cmn'
      INCLUDE 'gen_routines.dec'
*
      integer size
      logical wait
      real now,tstart,waitS
      integer got,get_fail

      real MAX_time
      parameter (MAX_time= 10.) !seconds

      integer MAX_failures
      parameter (MAX_failures= 100) !attempts
*
*--------------------------------------------------------
*
      err= ' '
*
      gen_display_RPCclientID= clnt_create(
     &     gen_display_server_machine,
     &     gen_display_server_RPCprgmID,
     &     gen_display_server_RPCversionID,'tcp')
*
      ABORT= gen_display_RPCclientID.EQ.0
      IF(ABORT) THEN
        write(err,'(":clnt_create ABORTed for RPC program ID# (",i10,",",i5
     $       ,") [0x",z8,",0x",z5,"] on machine ",a)')
     $       gen_display_server_RPCprgmID,gen_display_server_RPCversionID,
     $       gen_display_server_RPCprgmID,gen_display_server_RPCversionID,
     $       gen_display_server_machine
        call G_append(here,err)
        RETURN
      ELSE
        write(err,'(":clnt_create OK for RPC program ID#",i10
     $       ," [0x",z8,"] on machine ",a,)') 
     $       gen_display_server_RPCprgmID,gen_display_server_RPCprgmID
     $       ,gen_display_server_machine
        call G_add_path(here,err)
        call G_add_path('INFO--',err)
        call G_wrap_note(6,err)
        err= ' '
      ENDIF
*
      gen_display_everything= thcrlist()
      size= thaddlist(gen_display_everything,'*')  !list of everything
*      size= thremlist(gen_display_everything,'parm.ONE_EV')
*      size= thremlist(gen_display_everything,'parm.GRAPH_IO_DEV')
      ABORT= size.LE.0
      IF(ABORT) THEN
        call G_build_note(':ABORTed to get list for '//
     &           '"gen_display_everything"#$','$',
     &                  gen_display_everything,' ',0.,' ',err)
        call G_add_path(here,err)
        RETURN
      ELSE
        write(err,'(":list gen_display_everything handle=",i10,
     $       " size=",i5)') gen_display_everything,size
        call G_add_path(here,err)
        call G_add_path('INFO--',err)
        call G_wrap_note(6,err)
        err= ' '
      ENDIF
*
      gen_display_event_info= thcrlist()
      size= thaddlist(gen_display_event_info,'event.*')  !list of all event stuff
*      size= thremlist(gen_display_event_info,'parm.ONE_EV')
*      size= thremlist(gen_display_event_info,'parm.GRAPH_IO_DEV')

      ABORT= size.LE.0
      IF(ABORT) THEN
        call G_build_note(':ABORTed to get list for '//
     &           '"gen_display_event_info"#$','$',
     &                  gen_display_event_info,' ',0.,' ',err)
        call G_add_path(here,err)
        RETURN
      ELSE
        write(err,'(":list gen_display_event_info handle=",i10,
     $       " size=",i5)') gen_display_event_info,size
        call G_add_path(here,err)
        call G_add_path('INFO--',err)
        call G_wrap_note(6,err)
        err= ' '
      ENDIF
*
*     Now dowload all the variables from the server
*
      call TIMEX(tstart)
      get_fail= 0
      err= ' '
      wait= .TRUE.
*
      DO WHILE(wait) 
*
        got=
     &    thgetlist(gen_display_everything,gen_display_RPCclientID)
*
        PRINT *,here//' got=',got

        ABORT= got.lt.0
        If(ABORT) get_fail= get_fail+1
*
        call TIMEX(now)
        waitS= now-tstart   !seconds since entered this routine
*
        wait= ABORT .and. waitS.LE.MAX_time .and.
     &                                  get_fail.LT.MAX_failures
*
      ENDDO
*
      IF(ABORT) THEN
        write(err,'(":quit after",f6.1," [",f6.1,"] seconds and",i7," ["
     $       ,i7,"] thgetlist failures#",i7,a)') waitS,MAX_time,get_fail
     $       ,MAX_failures, got
        call G_append(here,err)
      ELSEIF(get_fail.GT.0) THEN        !info. message
        write(err,'(":quit after",f6.1," [",f6.1,"] seconds and",i7," ["
     $       ,i7,"] thgetlist failures#",i7,a)') waitS,MAX_time,get_fail
     $       ,MAX_failures, got
        call G_append(here,err)
      ELSE
        err= ' '        !first try success!
      ENDIF



c      call G_edisp_start(ABORT,err)
      IF(ABORT) THEN
        call G_add_path(here,err)
        RETURN
      ELSEIF(err.NE.' ') THEN
        call G_add_path(here,err)
        call G_add_path('INFO--',err)
        call G_wrap_note(6,err)
        err= ' '
      ENDIF
*
      RETURN
      END


