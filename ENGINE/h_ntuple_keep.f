      subroutine h_Ntuple_keep(ABORT,err)
*----------------------------------------------------------------------
*
*     Purpose : Add entry to the HMS Ntuple
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*     Created: 11-Apr-1994  K.B.Beard, Hampton U.
* $Log$
* Revision 1.10.18.5.2.4  2009/06/29 20:00:16  jones
* add hsxtar
* set units for hszbeam,hsytar and hsxtar to cm
*
* Revision 1.10.18.5.2.3  2008/11/06 14:35:38  cdaq
* Removed S0, added helicte
*
* Revision 1.10.18.5.2.1  2008/10/11 15:03:54  cdaq
* slow raster
*
* Revision 1.10.18.5  2007/12/12 15:53:53  cdaq
* added focal plane time to ntuple
*
* Revision 1.10.18.4  2007/10/29 21:59:41  cdaq
* Modifications to HMS ntuple for beam raster/bpm information (MKJ)
*
* Revision 1.10.18.3  2007/10/28 01:59:24  cdaq
* *** empty log message ***
*
* Revision 1.10.18.2  2007/10/26 16:48:14  cdaq
* Added number of chamber hits to HMS ntuple
*
* Revision 1.10.18.1  2007/08/22 19:09:17  frw
* added FPP
*
* Revision 1.11  2006/06/22 frw
* added FPP entries
*
* Revision 1.10  2004/02/17 17:26:34  jones
* Changes to enable possiblity of segmenting rzdat files
*
* Revision 1.8.2.1  2003/04/04 12:55:11  cdaq
* add beam quantities to ntuple (MKJ)
*
* Revision 1.8  1996/09/04 14:43:17  saw
* (JRA) Modify ntuple contents
*
* Revision 1.7  1996/01/16 17:01:55  cdaq
* (JRA) Modify ntuple contents
*
* Revision 1.6  1995/09/01 13:38:28  cdaq
* (JRA) Add Cerenkov photoelectron count to ntuple
*
* Revision 1.5  1995/05/22  20:50:46  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.4  1995/05/11  17:37:13  cdaq
* (SAW) Change HSDEDXn vars to an array.
*
* Revision 1.3  1995/01/27  20:10:27  cdaq
* (JRA) Add Gas cerenkov to ntuple
*
* Revision 1.2  1994/06/17  02:44:38  cdaq
* (KBB) Upgrade
*
* Revision 1.1  1994/04/12  16:15:21  cdaq
* Initial revision
*
*
*----------------------------------------------------------------------
      implicit none
      save
*
      character*13 here
      parameter (here='h_Ntuple_keep')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'hms_scin_parms.cmn'
      INCLUDE 'h_ntuple.cmn'
      INCLUDE 'gen_data_structures.cmn'
      INCLUDE 'gen_event_info.cmn'
      INCLUDE 'hms_tracking.cmn'
      INCLUDE 'hms_physics_sing.cmn'
      INCLUDE 'hms_scin_tof.cmn'
      INCLUDE 'gen_scalers.cmn'
      INCLUDE 'hms_track_histid.cmn'  !temp junk.
*
      logical HEXIST	!CERNLIB function
*
      integer m
c
      integer pln,cnt,ihit
      real s0x1padc,s0x1nadc,s0x2nadc,s0x2padc
      real s0x1ptdc,s0x1ntdc,s0x2ntdc,s0x2ptdc
c
      real proton_mass
      parameter ( proton_mass = 0.93827247 ) ! [GeV/c^2]
*
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*
      IF(.NOT.h_Ntuple_exists) RETURN       !nothing to do
c
      if (h_Ntuple_max_segmentevents .gt. 0) then
       if (h_Ntuple_segmentevents .gt. h_Ntuple_max_segmentevents) then
        call h_ntuple_change(ABORT,err)
       else
        h_Ntuple_segmentevents = h_Ntuple_segmentevents +1
       endif
      endif
*
************************************************
      m= 0
*  
      m= m+1
      h_Ntuple_contents(m)= HCER_NPE_SUM ! cerenkov photoelectron spectrum
      m= m+1
      h_Ntuple_contents(m)= HSP ! Lab momentum of chosen track in GeV/c
      m= m+1
      h_Ntuple_contents(m)= HSENERGY ! Lab total energy of chosen track in GeV
      m= m+1
      h_Ntuple_contents(m)= gbcm1_charge ! Charge of last scaler event
      m= m+1
      h_Ntuple_contents(m)= HSDELTA ! Spectrometer delta of chosen track
      m= m+1
      h_Ntuple_contents(m)= HSTHETA ! Lab Scattering angle in radians
      m= m+1
      h_Ntuple_contents(m)= HSPHI ! Lab Azymuthal angle in radians
      m= m+1
      h_Ntuple_contents(m)= HINVMASS ! Invariant Mass of remaing hadronic system
      m= m+1
      h_Ntuple_contents(m)= HSZBEAM*100 ! Lab Z coordinate of intersection of beam
c                                ! track with spectrometer ray
      m= m+1
      h_Ntuple_contents(m)= HSDEDX(1) ! DEDX of chosen track in 1st scin plane
      m= m+1
      h_Ntuple_contents(m)= HSBETA ! BETA of chosen track
      m= m+1
      h_Ntuple_contents(m)= HSTRACK_ET ! Total shower energy of chosen track
      m= m+1
      h_Ntuple_contents(m)= HSTRACK_PRESHOWER_E	! preshower of chosen track
      m= m+1
      h_Ntuple_contents(m)= HSX_FP ! X focal plane position 
      m= m+1
      h_Ntuple_contents(m)= HSY_FP
      m= m+1
      h_Ntuple_contents(m)= HSXP_FP
      m= m+1
      h_Ntuple_contents(m)= HSYP_FP
      m= m+1	
      h_Ntuple_contents(m)= HSX_TAR*100	
      m= m+1
      h_Ntuple_contents(m)= HSY_TAR*100
      m= m+1
      h_Ntuple_contents(m)= HSXP_TAR
      m= m+1
      h_Ntuple_contents(m)= HSYP_TAR
      m= m+1
      h_Ntuple_contents(m)= hstart_time
      m= m+1
      h_Ntuple_contents(m)= hstime_at_fp
      m= m+1
      h_Ntuple_contents(m)= float(gen_event_ID_number)
      m= m+1
      h_Ntuple_contents(m)= float(gen_event_type)
      m= m+1
      h_Ntuple_contents(m)= gfry_raw_adc
      m= m+1
      h_Ntuple_contents(m)= gfrx_raw_adc
      m= m+1
      h_Ntuple_contents(m)= hncham_hits(1)
      m= m+1
      h_Ntuple_contents(m)= hncham_hits(2)
      m= m+1
      h_Ntuple_contents(m)= gsry_raw_adc
      m= m+1
      h_Ntuple_contents(m)= gsrx_raw_adc
      m=m+1
      h_ntuple_contents(m) = float(gbeam_helicity)
c      write(6,'(i8,3f8.1)')gbeam_helicity,
c     >  h_ntuple_contents(m),float(gbeam_helicity),
c     >  h_ntuple_contents(m-1)
 
* Experiment dependent entries start here.


* Fill ntuple for this event
      ABORT= .NOT.HEXIST(h_Ntuple_ID)
      IF(ABORT) THEN
        call G_build_note(':Ntuple ID#$ does not exist',
     &                        '$',h_Ntuple_ID,' ',0.,' ',err)
        call G_add_path(here,err)
      ELSE
        call HFN(h_Ntuple_ID,h_Ntuple_contents)
      ENDIF
*
      RETURN
      END
