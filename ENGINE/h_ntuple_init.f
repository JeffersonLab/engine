      subroutine h_Ntuple_init(ABORT,err)
*----------------------------------------------------------------------
*
*     Creates an HMS Ntuple
*
*     Purpose : Books an HMS Ntuple; defines structure of it
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*     Created: 8-Apr-1994  K.B.Beard, Hampton Univ.
* $Log$
* Revision 1.10  2003/09/05 16:40:16  jones
* Merge in online03 changes (mkj)
*
* Revision 1.9.2.2  2003/09/04 21:10:33  jones
* add event type to ntuple (mkj)
*
* Revision 1.9.2.1  2003/04/04 12:54:42  cdaq
* add beam parameters to ntuple
*
* Revision 1.9  1996/09/04 14:42:44  saw
* (JRA) Some changes to ntuple contents
*
* Revision 1.8  1996/01/16 17:03:52  cdaq
* (JRA) Modify ntuple contents
*
* Revision 1.7  1995/09/01 13:38:05  cdaq
* (JRA) Add Cerenkov photoelectron count to ntuple
*
* Revision 1.6  1995/07/27  19:00:17  cdaq
* (SAW) Relocate data statements for f2c compatibility
*
* Revision 1.5  1995/05/22  20:50:46  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.4  1995/05/11  17:17:38  cdaq
* (SAW) Allow %d for run number in filenames
*
* Revision 1.3  1995/01/27  20:09:59  cdaq
* (JRA) Add Gas cerenkov to ntuple
*
* Revision 1.2  1994/06/17  02:34:12  cdaq
* (KBB) Upgrade
*
* Revision 1.1  1994/04/12  16:15:02  cdaq
* Initial revision
*
*
*----------------------------------------------------------------------
      implicit none
      save
*
      character*13 here
      parameter (here='h_Ntuple_init')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'h_ntuple.cmn'
      INCLUDE 'gen_routines.dec'
      include 'hms_data_structures.cmn'
      include 'gen_run_info.cmn'
*
      character*80 default_name
      parameter (default_name= 'HMSntuple')
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
      INCLUDE 'h_ntuple.dte'
*
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*
      IF(h_Ntuple_exists) THEN
        call h_Ntuple_shutdown(ABORT,err)
        If(ABORT) Then
          call G_add_path(here,err)
          RETURN
        EndIf
      ENDIF
*
      call NO_nulls(h_Ntuple_file)     !replace null characters with blanks
*
*-if name blank, just forget it
      IF(h_Ntuple_file.EQ.' ') RETURN   !do nothing
*
*- get any free IO channel
*
      call g_IO_control(io,'ANY',ABORT,err)
      h_Ntuple_exists= .NOT.ABORT
      IF(ABORT) THEN
        call G_add_path(here,err)
        RETURN
      ENDIF
      h_Ntuple_IOchannel= io
*
      h_Ntuple_ID= default_h_Ntuple_ID
      id= h_Ntuple_ID
*
      ABORT= HEXIST(id)
      IF(ABORT) THEN
        call g_IO_control(h_Ntuple_IOchannel,'FREE',ABORT,err)
        call G_build_note(':HBOOK id#$ already in use',
     &                                 '$',id,' ',rv,' ',err)
        call G_add_path(here,err)
        RETURN
      ENDIF
*
      CALL HCDIR(directory,'R')       !CERNLIB read current directory
*
      h_Ntuple_name= default_name
*
      id= h_Ntuple_ID
      name= h_Ntuple_name

      file= h_Ntuple_file
      call g_sub_run_number(file,gen_run_number)

      recL= default_recL
      io= h_Ntuple_IOchannel
*
*-open New *.rzdat file-
      call HROPEN(io,name,file,'N',recL,status)       !CERNLIB
*                                       !directory set to "//TUPLE"
      io= h_Ntuple_IOchannel
      ABORT= status.NE.0
      IF(ABORT) THEN
        call g_IO_control(h_Ntuple_IOchannel,'FREE',ABORT,err)
        iv(1)= status
        iv(2)= io
        pat= ':HROPEN error#$ opening IO#$ "'//file//'"'
        call G_build_note(pat,'$',iv,' ',rv,' ',err)
        call G_add_path(here,err)
        RETURN
      ENDIF
      h_Ntuple_file= file
*
      m= 0
      m= m+1
      h_Ntuple_tag(m)= 'hcer_npe' ! cerenkov photoelectron spectrum
      m= m+1
      h_Ntuple_tag(m)= 'hsp'     ! Lab momentum of chosen track in GeV/c
      m= m+1
      h_Ntuple_tag(m)= 'hse'      ! Lab total energy of chosen track in GeV
      m= m+1
      h_Ntuple_tag(m)= 'charge' ! charge
      m= m+1
      h_Ntuple_tag(m)= 'hsdelta'       ! Spectrometer delta of chosen track
      m= m+1
      h_Ntuple_tag(m)= 'hstheta'       ! Lab Scattering angle in radians
      m= m+1
      h_Ntuple_tag(m)= 'hsphi' ! Lab Azymuthal angle in radians
      m= m+1
      h_Ntuple_tag(m)= 'w'     ! Invariant Mass of remaing hadronic system
      m= m+1
      h_Ntuple_tag(m)= 'hszbeam'! Lab Z coordinate of intersection of beam
                                ! track with spectrometer ray
      m= m+1
      h_Ntuple_tag(m)= 'hsdedx1'       ! DEDX of chosen track in 1st scin plane
      m= m+1
      h_Ntuple_tag(m)= 'hsbeta'        ! BETA of chosen track
      m= m+1
      h_Ntuple_tag(m)= 'hsshtrk'  ! 'HSTRACK_ET'       ! Total shower energy of chosen track
      m= m+1
      h_Ntuple_tag(m)= 'hsprtrk'   !'HSTRACK_PRESHOWER_E' ! preshower of chosen track
      m= m+1
      h_Ntuple_tag(m)= 'hsxfp'		! X focal plane position 
      m= m+1
      h_Ntuple_tag(m)= 'hsyfp'
      m= m+1
      h_Ntuple_tag(m)= 'hsxpfp'
      m= m+1
      h_Ntuple_tag(m)= 'hsypfp'
      m= m+1
      h_Ntuple_tag(m)= 'hsytar'
      m= m+1
      h_Ntuple_tag(m)= 'hsxptar'
      m= m+1
      h_Ntuple_tag(m)= 'hsyptar'
      m= m+1
      h_Ntuple_tag(m)= 'hstart'
      m= m+1
      h_Ntuple_tag(m)= 'eventID'
      m= m+1
      h_Ntuple_tag(m)= 'evtype'

* Experiment dependent entries start here.
c
      m= m+1
      h_Ntuple_tag(m)= 'gfrx_raw'
      m= m+1
      h_Ntuple_tag(m)= 'gfry_raw'
      m= m+1
      h_Ntuple_tag(m)= 'gbeam_x'
      m= m+1
      h_Ntuple_tag(m)= 'gbeam_y'
      m= m+1
      h_Ntuple_tag(m)= 'bpma_x'
      m= m+1
      h_Ntuple_tag(m)= 'bpma_y'
      m= m+1
      h_Ntuple_tag(m)= 'bpmb_x'
      m= m+1
      h_Ntuple_tag(m)= 'bpmb_y'
      m= m+1
      h_Ntuple_tag(m)= 'bpmc_x'
      m= m+1
      h_Ntuple_tag(m)= 'bpmc_y'


* Open ntuple
*
      h_Ntuple_size= m     !total size
*
      title= h_Ntuple_title
      IF(title.EQ.' ') THEN
        msg= name//' '//h_Ntuple_file
        call only_one_blank(msg)
        title= msg   
        h_Ntuple_title= title
      ENDIF
*
      id= h_Ntuple_ID
      io= h_Ntuple_IOchannel
      name= h_Ntuple_name
      title= h_Ntuple_title
      size= h_Ntuple_size
      file= h_Ntuple_file
      bank= default_bank
      call HBOOKN(id,title,size,name,bank,h_Ntuple_tag)      !create Ntuple
*
      call HCDIR(h_Ntuple_directory,'R')      !record Ntuple directory
*
      CALL HCDIR(directory,' ')       !reset CERNLIB directory
*
      h_Ntuple_exists= HEXIST(h_Ntuple_ID)
      ABORT= .NOT.h_Ntuple_exists
*
      iv(1)= id
      iv(2)= io
      pat= 'Ntuple id#$ [' // h_Ntuple_directory // '/]' // 
     &                         name // ' IO#$ "' // file // '"'
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
