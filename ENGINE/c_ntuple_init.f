      subroutine c_Ntuple_init(ABORT,err)
*----------------------------------------------------------------------
*
*     Creates an COIN Ntuple
*
*     Purpose : Books an COIN Ntuple; defines structure of it
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*     Created: 8-Apr-1994  K.B.Beard, Hampton Univ.
* $Log$
* Revision 1.4  1995/07/27 18:59:48  cdaq
* (SAW) Relocate data statements for f2c compatibility
*
* Revision 1.3  1995/05/11  13:55:27  cdaq
* (SAW) Allow %d for run number in filenames
*
* Revision 1.2  1994/06/17  02:32:24  cdaq
* (KBB) Upgrade
*
* Revision 1.1  1994/04/12  16:11:34  cdaq
* Initial revision
*
*
*----------------------------------------------------------------------
      implicit none
      save
*
      character*13 here
      parameter (here='c_Ntuple_init')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'c_ntuple.cmn'
      INCLUDE 'gen_routines.dec'
      include 'gen_run_info.cmn'
*
      character*80 default_name
      parameter (default_name= 'COINntuple')
      integer default_bank,default_recL
      parameter (default_bank= 8000)    !4 bytes/word
      parameter (default_recL= 1024)    !record length
      character*80 title,file
      character*80 directory,name
      character*1000 pat,msg
      integer status,size,io,id,bank,recL,iv(10),m
      real rv(10)
*
      logical HEXIST           !CERNLIB function
*
      INCLUDE 'c_ntuple.dte'
*
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*
      IF(c_Ntuple_exists) THEN
        call c_Ntuple_shutdown(ABORT,err)
        If(ABORT) Then
          call G_add_path(here,err)
          RETURN
        EndIf
      ENDIF
*
      call NO_nulls(c_Ntuple_file)     !replace null characters with blanks
*
*-if name blank, just forget it
      IF(c_Ntuple_file.EQ.' ') RETURN   !do nothing
*
*- get any free IO channel
*
      call g_IO_control(io,'ANY',ABORT,err)
      c_Ntuple_exists= .NOT.ABORT
      IF(ABORT) THEN
        call G_add_path(here,err)
        RETURN
      ENDIF
      c_Ntuple_IOchannel= io
*
      c_Ntuple_ID= default_c_Ntuple_ID
      id= c_Ntuple_ID
*
      ABORT= HEXIST(id)
      IF(ABORT) THEN
        call g_IO_control(c_Ntuple_IOchannel,'FREE',ABORT,err)
        call G_build_note(':HBOOK id#$ already in use',
     &                                 '$',id,' ',rv,' ',err)
        call G_add_path(here,err)
        RETURN
      ENDIF
*
      CALL HCDIR(directory,'R')       !CERNLIB read current directory
*
      c_Ntuple_name= default_name
*
      id= c_Ntuple_ID
      name= c_Ntuple_name

      file= c_Ntuple_file
      call g_sub_run_number(file,gen_run_number)

      recL= default_recL
      io= c_Ntuple_IOchannel
*
*-open New *.rzdat file-
      call HROPEN(io,name,file,'N',recL,status)       !CERNLIB
*                                       !directory set to "//TUPLE"
      io= c_Ntuple_IOchannel
      ABORT= status.NE.0
      IF(ABORT) THEN
        call g_IO_control(c_Ntuple_IOchannel,'FREE',ABORT,err)
        iv(1)= status
        iv(2)= io
        pat= ':HROPEN error#$ opening IO#$ "'//file//'"'
        call G_build_note(pat,'$',iv,' ',rv,' ',err)
        call G_add_path(here,err)
        RETURN
      ENDIF
      c_Ntuple_file= file
*
**********begin insert description of contents of COIN tuple ******
      m= 0
      m= m+1
      c_Ntuple_tag(m)= 'CXRAST'         ! beam X rastor
      m= m+1
      c_Ntuple_tag(m)= 'CYRAST'         ! beam Y rastor
      m= m+1
      c_Ntuple_tag(m)= 'Cmiss_MASS' !'CMISSING_MASS'!Missing mass of undetected hadron system
      m= m+1
      c_Ntuple_tag(m)= 'CmissMOM'  !'CMISSING_MOM'!Magnitude of missing momentum 
      m= m+1
      c_Ntuple_tag(m)= 'CmissXmom' !'CMISSING_MOMX'!X component of missing momentum
      m= m+1
      c_Ntuple_tag(m)= 'CmissYmom' !'CMISSING_MOMY'  ! Y component of missing momentum
      m= m+1
      c_Ntuple_tag(m)= 'CmissZmom' !'CMISSING_MOMZ'  ! Z component of missing momentum
      m= m+1
      c_Ntuple_tag(m)= 'CTIME_COIN_COR' ! Corrected Coincidence time
      m= m+1
      c_Ntuple_tag(m)= 'eventID' ! CODA event ID#
      c_Ntuple_size= m
***********end insert description of contents of COIN tuple********
*
      title= c_Ntuple_title
      IF(title.EQ.' ') THEN
        msg= name//' '//c_Ntuple_file
        call only_one_blank(msg)
        title= msg   
        c_Ntuple_title= title
      ENDIF
*
      id= c_Ntuple_ID
      io= c_Ntuple_IOchannel
      name= c_Ntuple_name
      title= c_Ntuple_title
      size= c_Ntuple_size
      file= c_Ntuple_file
      bank= default_bank
      call HBOOKN(id,title,size,name,bank,c_Ntuple_tag)      !create Ntuple
*
      call HCDIR(c_Ntuple_directory,'R')      !record Ntuple directory
*
      CALL HCDIR(directory,' ')       !reset CERNLIB directory
*
      c_Ntuple_exists= HEXIST(c_Ntuple_ID)
      ABORT= .NOT.c_Ntuple_exists
*
      iv(1)= id
      iv(2)= io
      pat= 'Ntuple id#$ [' // c_Ntuple_directory // '/]' // 
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
