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
* Revision 1.1  1994/04/12 16:11:34  cdaq
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
*
      character*80 default_name
      parameter (default_name= 'COINntuple')
      integer default_bank,default_recL
      parameter (default_bank= 8000)    !4 bytes/word
      parameter (default_recL= 1024)    !record length
      character*80 pat,msg,title
      character*80 directory,name
      character*256 pad
      integer status,size,io,id,bank,recL,iv(10),m
      real rv(10)
*
      logical HEXIST           !CERNLIB function
*
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*
      call NO_nulls(c_Ntuple_filename)     !replace null characters with blanks
*
*-if name blank, just forget it
      c_Ntuple_exists= c_Ntuple_filename.NE.' '
      IF(.NOT.c_Ntuple_exists) RETURN
*
*- get any free IO channel
*
      call g_IO_control(c_Ntuple_IOchannel,'ANY',ABORT,err)
      io= c_Ntuple_IOchannel
      c_Ntuple_exists= .NOT.ABORT
      IF(ABORT) THEN
        call G_add_path(here,err)
        RETURN
      ENDIF
*
      id= default_c_Ntuple_ID
      c_Ntuple_ID= id
*
      ABORT= HEXIST(id)
      IF(ABORT) THEN
        call G_build_note(':HBOOK id#$ already in use',
     &                                 '$',id,' ',rv,' ',err)
        call G_add_path(here,err)
        call g_IO_control(c_Ntuple_IOchannel,'FREE',ABORT,err)
        RETURN
      ENDIF
*
      CALL HCDIR(directory,'R')       !CERNLIB read current directory
*
      c_Ntuple_name= ' '
      name= default_name
      c_Ntuple_name= name
      recL= default_recL
*-open New *.rzdat file-
      call HROPEN(io,name,c_Ntuple_filename,'N',recL,status)       !CERNLIB
*                                       !directory set to "//TUPLE"
      ABORT= status.NE.0
      IF(ABORT) THEN
        call g_IO_control(c_Ntuple_IOchannel,'FREE',ABORT,err)
        iv(1)= status
        iv(2)= io
        pat= ':error#$ in HROPEN opening file LUN#$ Ntuple '//name
        call G_build_note(pat,'$',iv,' ',rv,' ',msg)
        m= G_important_length(c_Ntuple_filename)
        err= '"'//c_Ntuple_filename(1:m)//'"'
        call g_prepend(msg,err)
        call G_add_path(here,err)
        RETURN
      ENDIF
*
**********begin insert description of contents of COIN tuple ******
      c_Ntuple_size= 2
      c_Ntuple_tag(1)= 'Ntracks'
      c_Ntuple_tag(2)= 'P1'
***********end insert description of contents of COIN tuple********
*
      id= c_Ntuple_ID
      title= c_Ntuple_title
      call NO_nulls(title)
      IF(title.EQ.' ') THEN
        pad= name//' '//c_Ntuple_filename
        call only_one_blank(pad)
        title= pad   
        c_Ntuple_title= title
      ENDIF
*
      size= c_Ntuple_size
      bank= default_bank
      call HBOOKN(id,title,size,name,bank,c_Ntuple_tag)      !create Ntuple
*
      call HCDIR(c_Ntuple_directory,'R')      !record Ntuple directory
      ABORT= .NOT.HEXIST(id)
      c_Ntuple_exists= .NOT.ABORT
*
      CALL HCDIR(directory,' ')       !reset CERNLIB directory
*
      IF(ABORT) THEN
        call G_build_note(':HBOOKN creation of id#$ failed',
     &                                 '$',id,' ',rv,' ',err)
        call G_add_path(here,err)
      ENDIF
*
      RETURN
      END      
