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
      h_Ntuple_tag(m)= 'ev_type'

* Experiment dependent entries start here.
c
      m= m+1
      h_Ntuple_tag(m)= 'S0X1padc'
      m= m+1
      h_Ntuple_tag(m)= 'S0X1nadc'
      m= m+1
      h_Ntuple_tag(m)= 'S0X2padc'
      m= m+1
      h_Ntuple_tag(m)= 'S0X2nadc'
      m= m+1
      h_Ntuple_tag(m)= 'S0X1ptdc'
      m= m+1
      h_Ntuple_tag(m)= 'S0X1ntdc'
      m= m+1
      h_Ntuple_tag(m)= 'S0X2ptdc'
      m= m+1
      h_Ntuple_tag(m)= 'S0X2ntdc'
      m= m+1
      h_Ntuple_tag(m)= 'bpmc_x'
      m= m+1
      h_Ntuple_tag(m)= 'bpmc_y'
      m= m+1
      h_Ntuple_tag(m)= 'hdchits1'
      m= m+1
      h_Ntuple_tag(m)= 'hdchits2'

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
