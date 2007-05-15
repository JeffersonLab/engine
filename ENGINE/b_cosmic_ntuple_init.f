      subroutine b_cosmic_ntuple_init(ABORT,err)
*----------------------------------------------------------------------
*
*     Creates an Bigcal cosmic Ntuple
*
*     Purpose : 
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*----------------------------------------------------------------------
      implicit none
      save
*
      character*13 here
      parameter (here='_init')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'gen_routines.dec'
      INCLUDE 'gen_run_info.cmn'
      INCLUDE 'sos_minerva.cmn'


*
      character*80 default_name
      parameter (default_name= 'ssieventuple')
      character*80 default_title
      parameter (default_title= 'sSieveSlits')   
      integer default_bank,default_recL
      parameter (default_bank= 8000)    !4 bytes/word
      parameter (default_recL= 1024)    !record length
      character*80 title,file
      character*80 directory,name
      character*1000 pat,msg
      integer status,size,io,id,bank,recL,iv(10),m
*      parameter (id = 1)
      real rv(10)
*
      logical HEXIST           !CERNLIB function
      INCLUDE 's_sieve_ntuple.cmn'
      INCLUDE 's_sieve_ntuple.dte'
*
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*
      IF(s_sieve_Ntuple_exists) THEN    
        call s_sv_Nt_shutdown(ABORT,err)
        If(ABORT) Then
          call G_add_path(here,err)
          RETURN
        EndIf
      ENDIF
*

      s_sieve_Ntuple_ID= default_s_sieve_Ntuple_ID
      s_sieve_Ntuple_name= default_name
      s_sieve_Ntuple_title= default_title

      call NO_nulls(s_sieve_Ntuple_file)     !replace null characters with blanks
*
*-if name blank, just forget it
      IF(s_sieve_Ntuple_file.EQ.' ') RETURN   !do nothing
*
*- get any free IO channel
*
      call g_IO_control(io,'ANY',ABORT,err)
      s_sieve_Ntuple_exists= .NOT.ABORT
      IF(ABORT) THEN
        call G_add_path(here,err)
        RETURN
      ENDIF
      s_sieve_Ntuple_IOchannel= io
*
      id= s_sieve_Ntuple_ID
*

      ABORT= HEXIST(id)
      IF(ABORT) THEN
        call g_IO_control(s_sieve_Ntuple_IOchannel,'FREE',ABORT,err)
        call G_build_note(':HBOOK id#$ already in use',
     &                                 '$',id,' ',rv,' ',err)
        call G_add_path(here,err)
        RETURN
      ENDIF
*

      CALL HCDIR(directory,'R')       !CERNLIB read current directory
 
*
*
      id= s_sieve_Ntuple_ID
      name= s_sieve_Ntuple_name

      file= s_sieve_Ntuple_file
      call g_sub_run_number(file,gen_run_number)

      recL= default_recL
      io= s_sieve_Ntuple_IOchannel
*
*-open New *.rzdat file-
      call HROPEN(io,name,file,'N',recL,status)       !CERNLIB
*                                       !directory set to "//TUPLE"
      io= s_sieve_Ntuple_IOchannel
      ABORT= status.NE.0
      IF(ABORT) THEN
        call g_IO_control(s_sieve_Ntuple_IOchannel,'FREE',ABORT,err)
        iv(1)= status
        iv(2)= io
        pat= ':HROPEN error#$ opening IO#$ "'//file//'"'
        call G_build_note(pat,'$',iv,' ',rv,' ',err)
        call G_add_path(here,err)
        RETURN
      ENDIF
      s_sieve_Ntuple_file= file
*
      title= s_sieve_Ntuple_title
      IF(title.EQ.' ') THEN
        msg= name//' '//s_sieve_Ntuple_file
        call only_one_blank(msg)
        title= msg   
        s_sieve_Ntuple_title= title
      ENDIF
*

      id= s_sieve_Ntuple_ID
      io= s_sieve_Ntuple_IOchannel
      name= s_sieve_Ntuple_name
      title= s_sieve_Ntuple_title
      size= s_sieve_Ntuple_size
      file= s_sieve_Ntuple_file
      bank= default_bank

      Call Hbnt(id,name,' ') ! create CWN Ntuple
*
* define Ntuple structure
*
      call HBNAME(id,'SOS fp',s_xfp,'s_xfp:R*4,s_xpfp:R*4,s_yfp:R*4,
     & s_ypfp:R*4,s_delta:R*4')
      call HBNAME(id,'SOS_MINE',n_minerva_hits,'n_minerva_hits[0,40]:I,
     & plane(n_minerva_hits):I,fiber(n_minerva_hits):I,
     & ADC(n_minerva_hits):I')

*****      call HBOOKN(id,title,size,name,bank,s_sieve_Ntuple_tag)      !create Ntuple
*
      call HCDIR(s_sieve_Ntuple_directory,'R')      !record Ntuple directory
*
      CALL HCDIR(directory,' ')       !reset CERNLIB directory

*
      s_sieve_Ntuple_exists= HEXIST(s_sieve_Ntuple_ID)
      ABORT= .NOT.s_sieve_Ntuple_exists
*
      iv(1)= id
      iv(2)= io
      pat= 'Ntuple id#$ [' // s_sieve_Ntuple_directory // '/]' // 
     &                         name // ' IO#$ "' // file // '"'
      call G_build_note(pat,'$',iv,' ',rv,' ',msg)
      call sub_string(msg,' /]','/]')
*
      IF(ABORT) THEN
        err= ':unable to create '//msg
        call G_add_path(here,err)
      ELSE
        pat= ':created '//msg
        call G_add_path(here,pat)
        call G_log_message('INFO: '//pat)
      ENDIF
*
      RETURN
      END  
