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
* Revision 1.11.18.5.2.1  2008/11/04 18:59:21  cdaq
* Added Slow Raster
*
* Revision 1.11.18.5  2007/12/12 15:54:17  cdaq
* added focal plane time to HMS ntuple
*
* Revision 1.11.18.4  2007/10/29 21:59:41  cdaq
* Modifications to HMS ntuple for beam raster/bpm information (MKJ)
*
* Revision 1.11.18.3  2007/10/28 01:59:30  cdaq
* *** empty log message ***
*
* Revision 1.11.18.2  2007/10/26 16:49:21  cdaq
* added number of hdc hits to HMS ntuple
*
* Revision 1.11.18.1  2007/10/16 20:20:31  cdaq
* *** empty log message ***
*
* Revision 1.11  2004/02/17 17:26:34  jones
* Changes to enable possiblity of segmenting rzdat files
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
c
      character*80 file
      character*80 name
      character*1000 pat,msg
      integerilo,fn_len,m
      character*1 ifile

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
      h_Ntuple_ID= default_h_Ntuple_ID
      h_Ntuple_name= default_name
      IF(h_Ntuple_title.EQ.' ') THEN
        msg= name//' '//h_Ntuple_file
        call only_one_blank(msg)
        h_Ntuple_title= msg
      ENDIF

      file= h_Ntuple_file
      call g_sub_run_number(file,gen_run_number)


*     * only needed if using more than one file      
      if (h_Ntuple_max_segmentevents .gt. 0) then
       h_Ntuple_filesegments = 1

       ifile = char(ichar('0')+h_Ntuple_filesegments)
 
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
       write(*,*) ' Using segmented hms rzdat files first filename: ',file
       else
         write(*,*) ' Not using segmented hms rzdat files.'  
      endif
*
      m= 0
      m= m+1
      h_Ntuple_tag(m)= 'hcer_npe' ! 1 cerenkov photoelectron spectrum
      m= m+1
      h_Ntuple_tag(m)= 'hsp'     ! 2 Lab momentum of chosen track in GeV/c
      m= m+1
      h_Ntuple_tag(m)= 'hse'      ! 3 Lab total energy of chosen track in GeV
      m= m+1
      h_Ntuple_tag(m)= 'charge' ! 4 charge
      m= m+1
      h_Ntuple_tag(m)= 'hsdelta'       ! 5 Spectrometer delta of chosen track
      m= m+1
      h_Ntuple_tag(m)= 'hstheta'       ! 6 Lab Scattering angle in radians
      m= m+1
      h_Ntuple_tag(m)= 'hsphi' ! 7 Lab Azymuthal angle in radians
      m= m+1
      h_Ntuple_tag(m)= 'w'     ! 8 Invariant Mass of remaing hadronic system
      m= m+1
      h_Ntuple_tag(m)= 'hszbeam'! 9 Lab Z coordinate of intersection of beam
                                ! track with spectrometer ray
      m= m+1
      h_Ntuple_tag(m)= 'hsdedx1'       ! 10 DEDX of chosen track in 1st scin plane
      m= m+1
      h_Ntuple_tag(m)= 'hsbeta'        ! 11 BETA of chosen track
      m= m+1
      h_Ntuple_tag(m)= 'hsshtrk'  ! 12 'HSTRACK_ET'       ! Total shower energy of chosen track
      m= m+1
      h_Ntuple_tag(m)= 'hsprtrk'   ! 13 'HSTRACK_PRESHOWER_E' ! preshower of chosen track
      m= m+1
      h_Ntuple_tag(m)= 'hsxfp'		! 14 X focal plane position 
      m= m+1
      h_Ntuple_tag(m)= 'hsyfp'  ! 15
      m= m+1
      h_Ntuple_tag(m)= 'hsxpfp' !16
      m= m+1
      h_Ntuple_tag(m)= 'hsypfp'!17
      m= m+1
      h_Ntuple_tag(m)= 'hsytar' !18
      m= m+1
      h_Ntuple_tag(m)= 'hsxptar' !19
      m= m+1
      h_Ntuple_tag(m)= 'hsyptar' !20
      m= m+1
      h_Ntuple_tag(m)= 'hstart' !21
      m= m+1
      h_Ntuple_tag(m)= 'hsfptime' !22
      m= m+1
      h_Ntuple_tag(m)= 'eventID' !23
      m= m+1
      h_Ntuple_tag(m)= 'ev_type' !24

* Experiment dependent entries start here.
c
      m= m+1
      h_Ntuple_tag(m)= 'S0X1padc' !25
      m= m+1
      h_Ntuple_tag(m)= 'S0X1nadc' !26
      m= m+1
      h_Ntuple_tag(m)= 'S0X2padc' !27
      m= m+1
      h_Ntuple_tag(m)= 'S0X2nadc' !28
      m= m+1
      h_Ntuple_tag(m)= 'S0X1ptdc' !29
      m= m+1
      h_Ntuple_tag(m)= 'S0X1ntdc' !30
      m= m+1
      h_Ntuple_tag(m)= 'S0X2ptdc' !31
      m= m+1
      h_Ntuple_tag(m)= 'S0X2ntdc' !32
      m= m+1
      h_Ntuple_tag(m)= 'rast_y' !33
      m= m+1
      h_Ntuple_tag(m)= 'rast_x' !34
      m= m+1
      h_Ntuple_tag(m)= 'hdchits1' !35
      m= m+1
      h_Ntuple_tag(m)= 'hdchits2'!36
      m= m+1
      h_Ntuple_tag(m)= 'srast_y' ! 37
      m= m+1
      h_Ntuple_tag(m)= 'srast_x' ! 38

      h_Ntuple_size= m     !total size
* Open ntuple

      call h_Ntuple_open(file,ABORT,err)      

      IF(ABORT) THEN
        err= ':unable to create HMS Ntuple'
        call G_add_path(here,err)
      ELSE
        pat= ':created HMS Ntuple'
        call G_add_path(here,pat)
        call G_log_message('INFO: '//pat)
      ENDIF

      RETURN
      END  
