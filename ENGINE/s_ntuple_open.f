      subroutine s_Ntuple_open(file,ABORT,err)
*----------------------------------------------------------------------
*
*     Opens an HMS Ntuple file
*
*     Purpose : Books an HMS Ntuple
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*----------------------------------------------------------------------

      implicit none
      save

      character*13 here
      parameter (here='s_Ntuple_open')

      logical ABORT
      character*(*) err

      INCLUDE 's_ntuple.cmn'

      integer default_bank,default_recL
      parameter (default_bank= 8000)    !4 bytes/word
      parameter (default_recL= 1024)    !record length
      character*80 title,file
      character*80 directory,name
      character*1000 pat,msg
      integer status,size,io,id,bank,recL,iv(10),m
      real rv(10)

      logical HEXIST           !CERNLIB function

*--------------------------------------------------------

      err= ' '
      ABORT = .FALSE.
      IF(s_Ntuple_exists) THEN
        call s_Ntuple_shutdown(ABORT,err)
        If(ABORT) Then
          call G_add_path(here,err)
          RETURN
        EndIf
      ENDIF

*- get any free IO channel

      call g_IO_control(io,'ANY',ABORT,err)
      s_Ntuple_exists= .NOT.ABORT
      IF(ABORT) THEN
        call G_add_path(here,err)
        RETURN
      ENDIF
      s_Ntuple_IOchannel= io
      
      id= s_Ntuple_ID
      name= s_Ntuple_name
      title= s_Ntuple_title
      ABORT= HEXIST(id)
      IF(ABORT) THEN
        call g_IO_control(s_Ntuple_IOchannel,'FREE',ABORT,err)
        call G_build_note(':HBOOK id#$ already in use',
     &                                 '$',id,' ',rv,' ',err)
        call G_add_path(here,err)
        RETURN
      ENDIF

      CALL HCDIR(directory,'R')       !CERNLIB read current directory

*-open New *.rzdat file-
      recL= default_recL
      call HROPEN(io,name,file,'N',recL,status)       !CERNLIB

      ABORT= status.NE.0
      IF(ABORT) THEN
        call g_IO_control(s_Ntuple_IOchannel,'FREE',ABORT,err)
        iv(1)= status
        iv(2)= io
        pat= ':HROPEN error#$ opening IO#$ "'//file//'"'
        call G_build_note(pat,'$',iv,' ',rv,' ',err)
        call G_add_path(here,err)
        RETURN
      ENDIF
      
      size= s_Ntuple_size
      bank= default_bank
      title= s_Ntuple_title
      call HBOOKN(id,title,size,name,bank,s_Ntuple_tag)      !create Ntuple

      call HCDIR(s_Ntuple_directory,'R')      !record Ntuple directory

      CALL HCDIR(directory,' ')       !reset CERNLIB directory

      s_Ntuple_exists= HEXIST(s_Ntuple_ID)
      ABORT= .NOT.s_Ntuple_exists

      iv(1)= id
      iv(2)= io
      pat= 'Ntuple id#$ [' // s_Ntuple_directory // '/]' // 
     &                         name // ' IO#$ "' // file // '"'
      call G_build_note(pat,'$',iv,' ',rv,' ',msg)
      call sub_string(msg,' /]','/]')

      IF(ABORT) THEN
        err= ':unable to create '//msg
        call G_add_path(here,err)
      ELSE
        pat= ':created '//msg
        call G_add_path(here,pat)
        call G_log_message('INFO: '//pat)
      ENDIF

      RETURN
      END  
