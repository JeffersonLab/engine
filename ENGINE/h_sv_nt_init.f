      subroutine h_sv_Nt_init(ABORT,err)
*----------------------------------------------------------------------
*
*     Creates an HMS Sieve slit Ntuple
*
*     Purpose : Books an HMS Ntuple; defines structure of it
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*     Created: 1-Nov-1994  
* $Log$
* Revision 1.4  1996/04/29 19:49:21  saw
* (JRA) Add HCAL_ET
*
* Revision 1.3  1995/07/27 19:43:22  cdaq
* (SAW) Relocate data statements for f2c compatibility
*
* Revision 1.2  1995/05/11  18:58:55  cdaq
* (SAW) Allow %d for run number in filenames
*
* Revision 1.1  1995/01/27  20:05:15  cdaq
* Initial revision
*
*
*----------------------------------------------------------------------
      implicit none
      save
*
      character*13 here
      parameter (here='h_sv_Nt_init')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'h_sieve_ntuple.cmn'
      INCLUDE 'gen_routines.dec'
      include 'gen_run_info.cmn'
*
      character*80 default_name
      parameter (default_name= 'sieventuple')
      character*80 default_title
      parameter (default_title= 'SieveSlits')   
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
*
      INCLUDE 'h_sieve_ntuple.dte'
*
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*
      IF(h_sieve_Ntuple_exists) THEN    
        call h_sv_Nt_shutdown(ABORT,err)
        If(ABORT) Then
          call G_add_path(here,err)
          RETURN
        EndIf
      ENDIF
*

      h_sieve_Ntuple_ID= default_h_sieve_Ntuple_ID
      h_sieve_Ntuple_name= default_name
      h_sieve_Ntuple_title= default_title

      call NO_nulls(h_sieve_Ntuple_file)     !replace null characters with blanks
*
*-if name blank, just forget it
      IF(h_sieve_Ntuple_file.EQ.' ') RETURN   !do nothing
*
*- get any free IO channel
*
      call g_IO_control(io,'ANY',ABORT,err)
      h_sieve_Ntuple_exists= .NOT.ABORT
      IF(ABORT) THEN
        call G_add_path(here,err)
        RETURN
      ENDIF
      h_sieve_Ntuple_IOchannel= io
*
      id= h_sieve_Ntuple_ID
*

      ABORT= HEXIST(id)
      IF(ABORT) THEN
        call g_IO_control(h_sieve_Ntuple_IOchannel,'FREE',ABORT,err)
        call G_build_note(':HBOOK id#$ already in use',
     &                                 '$',id,' ',rv,' ',err)
        call G_add_path(here,err)
        RETURN
      ENDIF
*

      CALL HCDIR(directory,'R')       !CERNLIB read current directory
 
*
*
      id= h_sieve_Ntuple_ID
      name= h_sieve_Ntuple_name

      file= h_sieve_Ntuple_file
      call g_sub_run_number(file,gen_run_number)

      recL= default_recL
      io= h_sieve_Ntuple_IOchannel
*
*-open New *.rzdat file-
      call HROPEN(io,name,file,'N',recL,status)       !CERNLIB
*                                       !directory set to "//TUPLE"
      io= h_sieve_Ntuple_IOchannel
      ABORT= status.NE.0
      IF(ABORT) THEN
        call g_IO_control(h_sieve_Ntuple_IOchannel,'FREE',ABORT,err)
        iv(1)= status
        iv(2)= io
        pat= ':HROPEN error#$ opening IO#$ "'//file//'"'
        call G_build_note(pat,'$',iv,' ',rv,' ',err)
        call G_add_path(here,err)
        RETURN
      ENDIF
      h_sieve_Ntuple_file= file
*
**********begin insert description of contents of HMS tuple ******
      m= 0
*  
      m=m+1
      h_sieve_Ntuple_tag(m)= 'HSX_FP'		! X focal plane position 
      m= m+1
      h_sieve_Ntuple_tag(m)= 'HSY_FP'
      m= m+1
      h_sieve_Ntuple_tag(m)= 'HSXP_FP'
      m= m+1
      h_sieve_Ntuple_tag(m)= 'HSYP_FP'
      m= m+1
      h_sieve_Ntuple_tag(m)= 'HSDELTA'
      m= m+1
      h_sieve_Ntuple_tag(m)= 'HSX_TAR'
      m= m+1
      h_sieve_Ntuple_tag(m)= 'HSY_TAR'
      m= m+1
      h_sieve_Ntuple_tag(m)= 'HSXP_TAR'
      m= m+1
      h_sieve_Ntuple_tag(m)= 'HSYP_TAR'
      m= m+1
      h_sieve_Ntuple_tag(m)= 'HCAL_ET'
    

*
      h_sieve_Ntuple_size= m     !total size
***********end insert description of contents of HMS tuple********
*
      title= h_sieve_Ntuple_title
      IF(title.EQ.' ') THEN
        msg= name//' '//h_sieve_Ntuple_file
        call only_one_blank(msg)
        title= msg   
        h_sieve_Ntuple_title= title
      ENDIF
*

      id= h_sieve_Ntuple_ID
      io= h_sieve_Ntuple_IOchannel
      name= h_sieve_Ntuple_name
      title= h_sieve_Ntuple_title
      size= h_sieve_Ntuple_size
      file= h_sieve_Ntuple_file
      bank= default_bank
 
      call HBOOKN(id,title,size,name,bank,h_sieve_Ntuple_tag)      !create Ntuple
*
      call HCDIR(h_sieve_Ntuple_directory,'R')      !record Ntuple directory
*

      CALL HCDIR(directory,' ')       !reset CERNLIB directory

*
      h_sieve_Ntuple_exists= HEXIST(h_sieve_Ntuple_ID)
      ABORT= .NOT.h_sieve_Ntuple_exists
*
      iv(1)= id
      iv(2)= io
      pat= 'Ntuple id#$ [' // h_sieve_Ntuple_directory // '/]' // 
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
