      subroutine s_Ntuple_init(ABORT,err)
* xucc comments begin
*the following has been changed:
*      s_Ntuple_tag(m)= 'ssbeta_notrk'   ! untracked BETA of chosen track
*      s_Ntuple_tag(m)= 'ssshsum' ! untracked normalized total shower energy of chosen track
*      s_Ntuple_tag(m)= 'bxbpm'
*      s_Ntuple_tag(m)= 'bybpm'
*      s_Ntuple_tag(m)= 'frx'
*      s_Ntuple_tag(m)= 'fry'

*      s_Ntuple_contents(m)= SBETA_NOTRK ! untracked BETA of chosen track
*      s_Ntuple_contents(m)= SSSHSUM    ! untracked Norm. Total shower energy of chosen track
*      s_Ntuple_contents(m)= gbpm_x(2)
*      s_Ntuple_contents(m)= gbpm_y(2)
*      s_Ntuple_contents(m)= gfrx_raw_adc
*      s_Ntuple_contents(m)= gfry_raw_adc
*     xucc added end


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
* $Log: s_ntuple_init.f,v $
* Revision 1.7.4.1  2003/03/05 22:53:26  xu
* new variables
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

c
      character*80 file
      character*80 name
      character*1000 pat,msg
      integerilo,fn_len,m
      character*1 ifile
      INCLUDE 's_ntuple.dte'
*

!      integer default_bank,default_recL
!      parameter (default_bank= 8000)    !4 bytes/word
!      parameter (default_recL= 1024)    !record length
!      character*80 title
!      character*80 directory,name
!      character*256 file
!      character*1000 pat,msg
!      integer status,size,io,id,bank,recL,iv(10),m
!      real rv(10)
*
!      logical HEXIST           !CERNLIB function
*
!      INCLUDE 's_ntuple.dte'
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
      s_Ntuple_ID= default_s_Ntuple_ID
      s_Ntuple_name= default_name
      IF(s_Ntuple_title.EQ.' ') THEN
        msg= name//' '//s_Ntuple_file
        call only_one_blank(msg)
        s_Ntuple_title= msg
      ENDIF
*
      file= s_Ntuple_file
      call g_sub_run_number(file,gen_run_number)


*     * only needed if using more than one file      
      if (s_Ntuple_max_segmentevents .gt. 0) then
       s_Ntuple_filesegments = 1

       ifile = char(ichar('0')+s_Ntuple_filesegments)
 
       fn_len = g_important_length(file)
       ilo=index(file,'.hbook')
       if ((ilo.le.1).or.(ilo.gt.fn_len-5)) then
         ilo=index(file,'.rzdat')
       endif  

       if ((ilo.gt.1).and.(ilo.lt.fn_len)) then
         file = file(1:ilo-1) // '.' // ifile // file(ilo:fn_len)
       else
         ABORT = .true.
        RETURN
       endif
       write(*,*) ' Using segmented SOS rzdat files first filename: ',file
       else
         write(*,*) ' Not using segmented SOS rzdat files first filename: ',file  
      endif

! Old code
*
*- get any free IO channel
*
!      call g_IO_control(s_Ntuple_IOchannel,'ANY',ABORT,err)
!      io= s_Ntuple_IOchannel
!      s_Ntuple_exists= .NOT.ABORT
!      IF(ABORT) THEN
!        call G_add_path(here,err)
!        RETURN
!      ENDIF
*
!      s_Ntuple_ID= default_s_Ntuple_ID
!      id= s_Ntuple_ID
*
!      ABORT= HEXIST(id)
!      IF(ABORT) THEN
!        call g_IO_control(s_Ntuple_IOchannel,'FREE',ABORT,err)
!      call G_build_note(':HBOOK id#$ already in use',
!     &                                 '$',id,' ',rv,' ',err)
!        call G_add_path(here,err)
!        RETURN
!      ENDIF
*
!     CALL HCDIR(directory,'R') !CERNLIB read current directory
*
!      s_Ntuple_name= default_name
*
!      id= s_Ntuple_ID
!      name= s_Ntuple_name

!      file= s_Ntuple_file
!      call g_sub_run_number(file,gen_run_number)

!      recL= default_recL
*
*-open New *.rzdat file-
!      call HROPEN(io,name,file,'N',recL,status)       !CERNLIB
*                                       !directory set to "//TUPLE"
!      ABORT= status.NE.0
!      IF(ABORT) THEN
!        call g_IO_control(s_Ntuple_IOchannel,'FREE',ABORT,err)
!        iv(1)= status
!      iv(2)= io
!        pat= ':HROPEN error#$ opening IO#$ "'//file//'"'
!        call G_build_note(pat,'$',iv,' ',rv,' ',err)
!        call G_add_path(here,err)
!        RETURN
!      ENDIF
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
      s_Ntuple_tag(m)= 'Q2'     ! Four Momentum Transfer
      m= m+1
      s_Ntuple_tag(m)= 'sszbeam'! Lab Z coordinate of intersection of beam
                                ! track with spectrometer ray
      m= m+1
      s_Ntuple_tag(m)= 'ssdedx1'  	! DEDX of chosen track in 1st scin plane
      m= m+1
*     xucc added begin
      s_Ntuple_tag(m)= 'ssbeta_notrk'   ! untracked BETA of chosen track
      m= m+1
*     xucc added end

      s_Ntuple_tag(m)= 'ssbeta'		! tracked BETA of chosen track
      m= m+1

*    xucc added begin
      s_Ntuple_tag(m)= 'ssshsum' ! untracked normalized total shower energy of chosen track
      m= m+1
*    xucc added end

      s_Ntuple_tag(m)= 'ssshtrk' ! 'SSTRACK_ET'	! Total shower energy of chosen track
*  here ssshtrk means "tracked normalized total shower energy of chosen track"

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
      s_Ntuple_tag(m)= 'evtype'
      m= m+1
      s_Ntuple_tag(m)= 'scal_et'
      m= m+1
      s_Ntuple_tag(m)= 'sntrks'
      m= m+1
      s_Ntuple_tag(m)= 'sgdscht'
      m= m+1
      s_Ntuple_tag(m)= 'sstart'
      m= m+1
      s_Ntuple_tag(m)= 'SAER_NPE' 


* Experiment dependent entries start here.
* xucc  added begin
      m= m+1
      s_Ntuple_tag(m)= 'bxbpm'
      m= m+1
      s_Ntuple_tag(m)= 'bybpm'
      m= m+1
      s_Ntuple_tag(m)= 'sctimer'
      m= m+1
      s_Ntuple_tag(m)= 'hctimer'
      m= m+1
      s_Ntuple_tag(m)= 'sELHI'
      m= m+1
      s_Ntuple_tag(m)= 'sELLO'
      m= m+1
      s_Ntuple_tag(m)= 'sPRHI'
      m= m+1
      s_Ntuple_tag(m)= 'sPRLO'
      m= m+1
      s_Ntuple_tag(m)= 'sSHLO'
      m= m+1
      s_Ntuple_tag(m)= 'sSTOF'
      m= m+1
      s_Ntuple_tag(m)= 'sPIPRE'
      m= m+1
      s_Ntuple_tag(m)= 'sSCIN'
      m= m+1
      s_Ntuple_tag(m)= 'sELREAL'
      m= m+1
      s_Ntuple_tag(m)= 'sELCLEAN'
      m= m+1
      s_Ntuple_tag(m)= 'sCER'
      m= m+1
      s_Ntuple_tag(m)= 'sPION'
      m= m+1
      s_Ntuple_tag(m)= 'frx'
      m= m+1
      s_Ntuple_tag(m)= 'fry'
      m= m+1
      s_Ntuple_tag(m)= 'sPRE'
      m= m+1
      s_Ntuple_tag(m)= 'sTRIG'
      m= m+1
      s_Ntuple_tag(m)= 'sPREHMS'
      m= m+1
      s_Ntuple_tag(m)= 'sTRIGHMS'

*  xucc added end



* Open ntuple.
*
      s_Ntuple_size= m     !total size
*
* Open ntuple

      call s_Ntuple_open(file,ABORT,err)      

      IF(ABORT) THEN
        err= ':unable to create SOS Ntuple'
        call G_add_path(here,err)
      ELSE
        pat= ':created SOS Ntuple'
        call G_add_path(here,pat)
        call G_log_message('INFO: '//pat)
      ENDIF
c
      RETURN
      END  

* Open ntuple.
*
!      s_Ntuple_size= m     !total size
*
!      title= s_Ntuple_title
!      IF(title.EQ.' ') THEN
!        msg= name//' '//s_Ntuple_file
!        call only_one_blank(msg)
!        title= msg   
!        s_Ntuple_title= title
!      ENDIF
*
!      id= s_Ntuple_ID
!      title= s_Ntuple_title
!    size= s_Ntuple_size
!      file= s_Ntuple_file
!      bank= default_bank
!      call HBOOKN(id,title,size,name,bank,s_Ntuple_tag)      !create Ntuple
*
!      call HCDIR(s_Ntuple_directory,'R')      !record Ntuple directory
*
!      CALL HCDIR(directory,' ')       !reset CERNLIB directory
*
!      s_Ntuple_exists= HEXIST(s_Ntuple_ID)
!      ABORT= .NOT.s_Ntuple_exists
*
!      iv(1)= id
!      iv(2)= io
!      pat= 'Ntuple id#$ [' // s_Ntuple_directory // '/]' // 
!     &           name // ' IO#$ "' // s_Ntuple_file // '"'
!      call G_build_note(pat,'$',iv,' ',rv,' ',msg)
!      call sub_string(msg,' /]','/]')
*
!      IF(ABORT) THEN
!        err= ':unable to create '//msg
!        call G_add_path(here,err)
c      ELSE
c        pat= ':created '//msg
c        call G_add_path(here,pat)
c        call G_log_message('INFO: '//pat)
!      ENDIF
*
!      RETURN
!      END  
