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
* Revision 1.8.2.1  2004/05/13 22:06:30  jones
* Change ntuple variables
*
* Revision 1.8  2004/02/17 17:26:34  jones
* Changes to enable possiblity of segmenting rzdat files
*
* Revision 1.7.2.3  2003/08/12 17:35:32  cdaq
* Add variables for e00-108 (hamlet)
*
* Revision 1.7.2.2  2003/06/26 12:39:52  cdaq
* changes for e01-001  (mkj)
*
* Revision 1.7.2.1  2003/04/04 12:54:42  cdaq
* add beam parameters to ntuple
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
      INCLUDE 'gen_scalers.cmn'
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
*
      m= 0
      m= m+1
      s_Ntuple_tag(m)= 'omega' ! 
      m= m+1
      s_Ntuple_tag(m)= 'q2' ! 
      m= m+1
      s_Ntuple_tag(m)= 'xbj' ! 
      m= m+1
      s_Ntuple_tag(m)= 'qabs' ! 
      m= m+1
      s_Ntuple_tag(m)= 'W2' ! 
      m= m+1
      s_Ntuple_tag(m)= 'ssthet_g' ! 
*
      m= m+1
      s_Ntuple_tag(m)= 'scer_npe' ! cerenkov photoelectron spectrum
      m= m+1
      s_Ntuple_tag(m)= 'ssp'	! Lab momentum of chosen track in GeV/c
*      m= m+1
*      s_Ntuple_tag(m)= 'ssenergy'! Lab total energy of chosen track in GeV
      m= m+1
      s_Ntuple_tag(m)= 'ssdelta'	! Spectrometer delta of chosen track
      m= m+1
      s_Ntuple_tag(m)= 'sstheta'	! Lab Scattering angle in radians
      m= m+1
      s_Ntuple_tag(m)= 'ssphi'	! Lab Azymuthal angle in radians
*      m= m+1
*      s_Ntuple_tag(m)= 'w'	! Invariant Mass of remaing hadronic system
*      m= m+1
*      s_Ntuple_tag(m)= 'sszbeam'! Lab Z coordinate of intersection of beam
                                ! track with spectrometer ray
*      m= m+1
*      s_Ntuple_tag(m)= 'ssdedx1'  	! DEDX of chosen track in 1st scin plane
      m= m+1
      s_Ntuple_tag(m)= 'ssbeta'		! BETA of chosen track
      m= m+1
      s_Ntuple_tag(m)= 'ssshtrk' ! 'SSTRACK_ET'	! Total shower energy of chosen track
      m= m+1
      s_Ntuple_tag(m)= 'ssshsum' ! 'SSSHSUM'	! Total shower energy 
*
      m= m+1
      s_Ntuple_tag(m)= 'ssprtrk'!'SSTRACK_PRESHOWER_E' ! preshower of chosen track
      m=m+1
      s_Ntuple_tag(m)= 'scal_x'
      m=m+1
      s_ntuple_tag(m)= 'scal_y'
*
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
      s_Ntuple_tag(m)= 'ssztar'
      m= m+1
      s_Ntuple_tag(m)= 'ssxptar'
      m= m+1
      s_Ntuple_tag(m)= 'ssyptar'
      m= m+1
      s_Ntuple_tag(m)= 'eventID'
      m= m+1
      s_Ntuple_tag(m)= 'evtype'
      m= m+1
      s_Ntuple_tag(m)= 'sstart'
*
      m= m+1
      s_Ntuple_tag(m)= 'charge' ! Charge of last scaler event
*
* Experiment dependent entries start here.
*      m= m+1
*      s_Ntuple_tag(m)= 'gfrx_raw'
*      m= m+1
*      s_Ntuple_tag(m)= 'gfry_raw'
      m= m+1
      s_Ntuple_tag(m)= 'bpmx'         ! Beam X Position
      m= m+1
      s_Ntuple_tag(m)= 'bpmy'         ! Beam Y Position
*      m= m+1
*      s_Ntuple_tag(m)= 'gbeam_x'
*      m= m+1
*      s_Ntuple_tag(m)= 'gbeam_y'
c
*      m= m+1
*      s_Ntuple_tag(m)= 'sceradc1'
*      m= m+1
*      s_Ntuple_tag(m)= 'sceradc2'
*      m= m+1
*      s_Ntuple_tag(m)= 'sceradc3'
*      m= m+1
*      s_Ntuple_tag(m)= 'sceradc4'
*      m= m+1
*      s_Ntuple_tag(m)= 'hminus'
*      m= m+1
*      s_Ntuple_tag(m)= 'scal_e1'
*      m= m+1
*      s_Ntuple_tag(m)= 'scal_e2'
*      m= m+1
*      s_Ntuple_tag(m)= 'scal_e3'
*      m= m+1
*      s_Ntuple_tag(m)= 'scal_e4'


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







