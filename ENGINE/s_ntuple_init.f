      subroutine s_Ntuple_init(ABORT,err)
*----------------------------------------------------------------------
*
*     Creates an SOS Ntuple
*
*     Purpose : Books an SOS Ntuple; defines structure of it
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*     Created: 8-Apr-1994  K.B.Beard, Hampton Univ.
* $Log$
* Revision 1.7.6.1  2003/12/17 22:55:01  jones
*  update e01004
*
* Revision 1.7  1996/09/04 15:18:02  saw
* (JRA) Modify ntuple contents
*
* Revision 1.6  1996/01/16 16:41:14  cdaq
* (JRA) Modify ntuple contents
*
* Revision 1.5  1995/09/01 13:38:59  cdaq
* (JRA) Add Cerenkov photoelectron count to ntuple
*
* Revision 1.4  1995/07/27  19:00:31  cdaq
* (SAW) Relocate data statements for f2c compatibility
*
* Revision 1.3  1995/05/11  19:00:02  cdaq
* (SAW) Allow %d for run number in filenames
*
* Revision 1.2  1994/06/17  02:36:00  cdaq
* (KBB) Upgrade
*
* Revision 1.1  1994/04/12  16:16:18  cdaq
* Initial revision
*
*
*----------------------------------------------------------------------
      implicit none
      save
*
      character*13 here
      parameter (here='s_Ntuple_init')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 's_ntuple.cmn'
      INCLUDE 'gen_routines.dec'
      include 'gen_run_info.cmn'
*
      character*80 default_name
      parameter (default_name= 'SOSntuple')
      integer default_bank,default_recL
      parameter (default_bank= 8000)    !4 bytes/word
      parameter (default_recL= 1024)    !record length
      character*80 title
      character*80 directory,name
      character*256 file
      character*1000 pat,msg
      integer status,size,io,id,bank,recL,iv(10),m
      real rv(10)
*
      logical HEXIST           !CERNLIB function
*
      INCLUDE 's_ntuple.dte'
*
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*
      IF(s_Ntuple_exists) THEN
        call s_Ntuple_shutdown(ABORT,err)
        If(ABORT) Then
          call G_add_path(here,err)
          RETURN
        EndIf
      ENDIF
*
      call NO_nulls(s_Ntuple_file)     !replace null characters with blanks
*
*-if name blank, just forget it
      IF(s_Ntuple_file.EQ.' ') RETURN   !do nothing
*
*- get any free IO channel
*
      call g_IO_control(s_Ntuple_IOchannel,'ANY',ABORT,err)
      io= s_Ntuple_IOchannel
      s_Ntuple_exists= .NOT.ABORT
      IF(ABORT) THEN
        call G_add_path(here,err)
        RETURN
      ENDIF
*
      s_Ntuple_ID= default_s_Ntuple_ID
      id= s_Ntuple_ID
*
      ABORT= HEXIST(id)
      IF(ABORT) THEN
        call g_IO_control(s_Ntuple_IOchannel,'FREE',ABORT,err)
        call G_build_note(':HBOOK id#$ already in use',
     &                                 '$',id,' ',rv,' ',err)
        call G_add_path(here,err)
        RETURN
      ENDIF
*
      CALL HCDIR(directory,'R')       !CERNLIB read current directory
*
      s_Ntuple_name= default_name
*
      id= s_Ntuple_ID
      name= s_Ntuple_name

      file= s_Ntuple_file
      call g_sub_run_number(file,gen_run_number)

      recL= default_recL
*
*-open New *.rzdat file-
      call HROPEN(io,name,file,'N',recL,status)       !CERNLIB
*                                       !directory set to "//TUPLE"
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
*
      m= 0
      m= m+1
      s_Ntuple_tag(m)= 'scer_npe' ! cerenkov photoelectron spectrum
      m= m+1
      s_Ntuple_tag(m)= 'ssp'	! Lab momentum of chosen track in GeV/c
      m= m+1
      s_Ntuple_tag(m)= 'ssenergy'! Lab total energy of chosen track in GeV
      m= m+1
      s_Ntuple_tag(m)= 'ssdelta'	! Spectrometer delta of chosen track
      m= m+1
      s_Ntuple_tag(m)= 'sstheta'	! Lab Scattering angle in radians
      m= m+1
      s_Ntuple_tag(m)= 'ssphi'	! Lab Azymuthal angle in radians
      m= m+1
      s_Ntuple_tag(m)= 'w'	! Invariant Mass of remaing hadronic system
      m= m+1
      s_Ntuple_tag(m)= 'sszbeam'! Lab Z coordinate of intersection of beam
                                ! track with spectrometer ray
      m= m+1
      s_Ntuple_tag(m)= 'ssdedx1'  	! DEDX of chosen track in 1st scin plane
      m= m+1
      s_Ntuple_tag(m)= 'ssbeta'		! BETA of chosen track
      m= m+1
      s_Ntuple_tag(m)= 'ssshtrk' ! 'SSTRACK_ET'	! Total shower energy of chosen track
      m= m+1
      s_Ntuple_tag(m)= 'ssprtrk'!'SSTRACK_PRESHOWER_E' ! preshower of chosen track
      m= m+1
      s_Ntuple_tag(m)= 'ssxfp'		! X focal plane position 
      m= m+1
      s_Ntuple_tag(m)= 'ssyfp'
      m= m+1
      s_Ntuple_tag(m)= 'ssxpfp'
      m= m+1
      s_Ntuple_tag(m)= 'ssypfp'
      m= m+1
      s_Ntuple_tag(m)= 'ssytar'
      m= m+1
      s_Ntuple_tag(m)= 'ssxptar'
      m= m+1
      s_Ntuple_tag(m)= 'ssyptar'
      m= m+1
      s_Ntuple_tag(m)= 'eventID'
      m= m+1
      s_Ntuple_tag(m)= 'sstart'
      m= m+1
      s_Ntuple_tag(m)= 'SAER_NPE' 
      m= m+1
      s_Ntuple_tag(m)= 'gbeam_x'
      m= m+1
      s_Ntuple_tag(m)= 'gbeam_y'
      m= m+1
      s_Ntuple_tag(m)= 'MPSclock'
      m= m+1
      s_Ntuple_tag(m)= 'hplus'
      m= m+1
      s_Ntuple_tag(m)= 'hminus'
      m= m+1
      s_Ntuple_tag(m)= 'sceradc1'
      m= m+1
      s_Ntuple_tag(m)= 'sceradc2'
      m= m+1
      s_Ntuple_tag(m)= 'sceradc3'
      m= m+1
      s_Ntuple_tag(m)= 'sceradc4'
      m= m+1
      s_Ntuple_tag(m)= 'ssc1xpd'
      m= m+1
      s_Ntuple_tag(m)= 'ssc2xpd'


* Experiment dependent entries start here.


* Open ntuple.
*
      s_Ntuple_size= m     !total size
*
      title= s_Ntuple_title
      IF(title.EQ.' ') THEN
        msg= name//' '//s_Ntuple_file
        call only_one_blank(msg)
        title= msg   
        s_Ntuple_title= title
      ENDIF
*
      id= s_Ntuple_ID
      title= s_Ntuple_title
      size= s_Ntuple_size
      file= s_Ntuple_file
      bank= default_bank
      call HBOOKN(id,title,size,name,bank,s_Ntuple_tag)      !create Ntuple
*
      call HCDIR(s_Ntuple_directory,'R')      !record Ntuple directory
*
      CALL HCDIR(directory,' ')       !reset CERNLIB directory
*
      s_Ntuple_exists= HEXIST(s_Ntuple_ID)
      ABORT= .NOT.s_Ntuple_exists
*
      iv(1)= id
      iv(2)= io
      pat= 'Ntuple id#$ [' // s_Ntuple_directory // '/]' // 
     &           name // ' IO#$ "' // s_Ntuple_file // '"'
      call G_build_note(pat,'$',iv,' ',rv,' ',msg)
      call sub_string(msg,' /]','/]')
*
      IF(ABORT) THEN
        err= ':unable to create '//msg
        call G_add_path(here,err)
c      ELSE
c        pat= ':created '//msg
c        call G_add_path(here,pat)
c        call G_log_message('INFO: '//pat)
      ENDIF
*
      RETURN
      END  
