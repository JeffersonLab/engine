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
**********begin insert description of contents of SOS tuple ******
      m= 0
*  
      m= m+1
      s_Ntuple_tag(m)= 'SCER_NPE' ! cerenkov photoelectron spectrum
      m= m+1
      s_Ntuple_tag(m)= 'p'	! Lab momentum of chosen track in GeV/c
      m= m+1
      s_Ntuple_tag(m)= 'SSENERGY'! Lab total energy of chosen track in GeV
      m= m+1
      s_Ntuple_tag(m)= 'SSDELTA'	! Spectrometer delta of chosen track
      m= m+1
      s_Ntuple_tag(m)= 'SSTHETA'	! Lab Scattering angle in radians
      m= m+1
      s_Ntuple_tag(m)= 'SSPHI'	! Lab Azymuthal angle in radians
      m= m+1
      s_Ntuple_tag(m)= 'SSMINV'	! Invariant Mass of remaing hadronic system
      m= m+1
      s_Ntuple_tag(m)= 'SSZBEAM'! Lab Z coordinate of intersection of beam
                                ! track with spectrometer ray
      m= m+1
      s_Ntuple_tag(m)= 'dedx1'  	! DEDX of chosen track in 1st scin plane
c      m= m+1
c      s_Ntuple_tag(m)= 'SSDEDX2'	! DEDX of chosen track in 2nd scin plane
c      m= m+1
c      s_Ntuple_tag(m)= 'SSDEDX3'	! DEDX of chosen track in 3rd scin plane
c      m= m+1
c      s_Ntuple_tag(m)= 'SSDEDX4'	! DEDX of chosen track in 4th scin plane
      m= m+1
      s_Ntuple_tag(m)= 'beta'		! BETA of chosen track
      m= m+1
      s_Ntuple_tag(m)= 'shtrk' ! 'SSTRACK_ET'	! Total shower energy of chosen track
      m= m+1
      s_Ntuple_tag(m)= 'prtrk'!'SSTRACK_PRESHOWER_E' ! preshower of chosen track
      m= m+1
      s_Ntuple_tag(m)= 'fptime'
      m= m+1
      s_Ntuple_tag(m)= 'xfp'		! X focal plane position 
      m= m+1
      s_Ntuple_tag(m)= 'yfp'
      m= m+1
      s_Ntuple_tag(m)= 'xpfp'
      m= m+1
      s_Ntuple_tag(m)= 'ypfp'
      m= m+1
      s_Ntuple_tag(m)= 'chi2'		! CHI2 per degree of freedom of chosen track.
      m= m+1
      s_Ntuple_tag(m)= 'ndof'
      m= m+1
      s_Ntuple_tag(m)= 'ytar'
      m= m+1
      s_Ntuple_tag(m)= 'xptar'
      m= m+1
      s_Ntuple_tag(m)= 'yptar'
*
c      m= m+1
c      s_Ntuple_tag(m)= 'trknum'		! Index of focal plane track chosen
c      m= m+1
c      s_Ntuple_tag(m)= 'SSNUM_TARTRACK'	! Index of target track chosen
c      m= m+1
c      s_Ntuple_tag(m)= 'SSID_LUND'	! LUND particle ID code -- not yet filled
c      m= m+1
c      s_Ntuple_tag(m)= 'SSNFREE_FP'
*
      m= m+1
      s_Ntuple_tag(m)= 'eventID'
*
      m= m+1
      s_Ntuple_tag(m)= 'scintothits'
      m= m+1
      s_Ntuple_tag(m)= 'scinhits'
      m= m+1
      s_Ntuple_tag(m)= 'starttime'
c      m= m+1
c      s_Ntuple_tag(m)= 'fptim1'
c      m= m+1
c      s_Ntuple_tag(m)= 'fptim2'
c      m= m+1
c      s_Ntuple_tag(m)= 'fptim3'
c      m= m+1
c      s_Ntuple_tag(m)= 'fptim4'
c      m= m+1
c      s_Ntuple_tag(m)= 'scinhit1'
c      m= m+1
c      s_Ntuple_tag(m)= 'scinhit2'
c      m= m+1
c      s_Ntuple_tag(m)= 'scinhit3'
c      m= m+1
c      s_Ntuple_tag(m)= 'scinhit4'
      m= m+1
      s_Ntuple_tag(m)= 'sdc_raw_hits'
      m= m+1
      s_Ntuple_tag(m)= 'sdc_hits'
*
c      m= m+1
c      s_Ntuple_tag(m)= 'sx_sp1'
c      m= m+1
c      s_Ntuple_tag(m)= 'sy_sp1'
c      m= m+1
c      s_Ntuple_tag(m)= 'sxp_sp1'
c      m= m+1
c      s_Ntuple_tag(m)= 'sx_sp2'
c      m= m+1
c      s_Ntuple_tag(m)= 'sy_sp2'
c      m= m+1
c      s_Ntuple_tag(m)= 'sxp_sp2'
*
c      m= m+1
c      s_Ntuple_tag(m)= 'res1'
c      m= m+1
c      s_Ntuple_tag(m)= 'res2'
c      m= m+1
c      s_Ntuple_tag(m)= 'res3'
c      m= m+1
c      s_Ntuple_tag(m)= 'res4'
c      m= m+1
c      s_Ntuple_tag(m)= 'res5'
c      m= m+1
c      s_Ntuple_tag(m)= 'res6'
c      m= m+1
c      s_Ntuple_tag(m)= 'res7'
c      m= m+1
c      s_Ntuple_tag(m)= 'res8'
c      m= m+1
c      s_Ntuple_tag(m)= 'res9'
c      m= m+1
c      s_Ntuple_tag(m)= 'res10'
c      m= m+1
c      s_Ntuple_tag(m)= 'res11'
c      m= m+1
c      s_Ntuple_tag(m)= 'res12'
c      m= m+1
c      s_Ntuple_tag(m)= 'tim1'
c      m= m+1
c      s_Ntuple_tag(m)= 'tim2'
c      m= m+1
c      s_Ntuple_tag(m)= 'tim3'
c      m= m+1
c      s_Ntuple_tag(m)= 'tim4'
c      m= m+1
c      s_Ntuple_tag(m)= 'tim5'
c      m= m+1
c      s_Ntuple_tag(m)= 'tim6'
c      m= m+1
c      s_Ntuple_tag(m)= 'tim7'
c      m= m+1
c      s_Ntuple_tag(m)= 'tim8'
c      m= m+1
c      s_Ntuple_tag(m)= 'tim9'
c      m= m+1
c      s_Ntuple_tag(m)= 'tim10'
c      m= m+1
c      s_Ntuple_tag(m)= 'tim11'
c      m= m+1
c      s_Ntuple_tag(m)= 'tim12'
c      m= m+1
c      s_Ntuple_tag(m)= 'dist1'
c      m= m+1
c      s_Ntuple_tag(m)= 'dist2'
c      m= m+1
c      s_Ntuple_tag(m)= 'dist3'
c      m= m+1
c      s_Ntuple_tag(m)= 'dist4'
c      m= m+1
c      s_Ntuple_tag(m)= 'dist5'
c      m= m+1
c      s_Ntuple_tag(m)= 'dist6'
c      m= m+1
c      s_Ntuple_tag(m)= 'dist7'
c      m= m+1
c      s_Ntuple_tag(m)= 'dist8'
c      m= m+1
c      s_Ntuple_tag(m)= 'dist9'
c      m= m+1
c      s_Ntuple_tag(m)= 'dist10'
c      m= m+1
c      s_Ntuple_tag(m)= 'dist11'
c      m= m+1
c      s_Ntuple_tag(m)= 'dist12'
*
      s_Ntuple_size= m     !total size
***********end insert description of contents of SOS tuple********
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
