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
* Revision 1.10.18.8  2009/03/31 18:08:38  puckett
* added xtar and fry variables to hms ntuple
*
* Revision 1.10.18.7  2008/07/29 16:06:35  puckett
* added option to store all track hit info in HMS ntuple (huge files)
*
* Revision 1.10.18.6  2008/04/23 18:02:31  cdaq
* *** empty log message ***
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
      integer plane,wire(12) ! choose only one wire per plane 
      real wirecent(12),wirepos(12),trackpos(12) ! choose only one wire per plane 

      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'hms_scin_parms.cmn'
      INCLUDE 'h_ntuple.cmn'
      INCLUDE 'gen_data_structures.cmn'
      include 'gep_data_structures.cmn'
      include 'bigcal_data_structures.cmn'
      INCLUDE 'gen_event_info.cmn'
      INCLUDE 'hms_tracking.cmn'
      INCLUDE 'hms_physics_sing.cmn'
      INCLUDE 'hms_scin_tof.cmn'
      INCLUDE 'gen_scalers.cmn'
      include 'gen_constants.par'
      INCLUDE 'hms_track_histid.cmn'  !temp junk.
*
      logical HEXIST	!CERNLIB function
*
      integer m
      logical oneperplane
c
      integer pln,cnt,ihit,hitnum
      real s0x1padc,s0x1nadc,s0x2nadc,s0x2padc
      real s0x1ptdc,s0x1ntdc,s0x2ntdc,s0x2ptdc
c
      real proton_mass
      parameter ( proton_mass = 0.93827247 ) ! [GeV/c^2]

      real*4 tdrift,ddrift

      real*4 h_drift_dist_calc
      external h_drift_dist_calc
*
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*
      IF(.NOT.h_Ntuple_exists) RETURN       !nothing to do
c
c     if we set this flag then we only keep events where there was exactly one hit per plane. 
c     don't know how good of statistics we can get per run this way.

      if(h_keep_only_good.ne.0) then
         oneperplane = .true.
         do plane=1,hdc_num_planes
            if(hdc_hits_per_plane(plane).gt.1) oneperplane = .false.
         enddo
         
         if(.not.oneperplane) return
      endif
      
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
      h_Ntuple_contents(m)= HSP	        ! Lab momentum of chosen track in GeV/c
      m= m+1
      h_Ntuple_contents(m)= HSENERGY    ! Lab total energy of chosen track in GeV
      m= m+1
      h_Ntuple_contents(m)= gbcm1_charge ! Charge of last scaler event
      m= m+1
      h_Ntuple_contents(m)= HSDELTA	! Spectrometer delta of chosen track
      m= m+1
      h_Ntuple_contents(m)= HSTHETA	! Lab Scattering angle in radians
      m= m+1
      h_Ntuple_contents(m)= HSPHI	! Lab Azymuthal angle in radians
      m= m+1
      h_Ntuple_contents(m)= HINVMASS	! Invariant Mass of remaing hadronic system
      m= m+1
      h_Ntuple_contents(m)= HSZBEAM! Lab Z coordinate of intersection of beam
c                                ! track with spectrometer ray
      m= m+1
      h_Ntuple_contents(m)= HSDEDX(1)	! DEDX of chosen track in 1st scin plane
      m= m+1
      h_Ntuple_contents(m)= HSBETA	! BETA of chosen track
      m= m+1
      h_Ntuple_contents(m)= HSTRACK_ET	! Total shower energy of chosen track
      m= m+1
      h_Ntuple_contents(m)= HSTRACK_PRESHOWER_E	! preshower of chosen track
      m= m+1
      h_Ntuple_contents(m)= HSX_FP		! X focal plane position 
      m= m+1
      h_Ntuple_contents(m)= HSY_FP
      m= m+1
      h_Ntuple_contents(m)= HSXP_FP
      m= m+1
      h_Ntuple_contents(m)= HSYP_FP
      m= m+1
      h_Ntuple_contents(m)= HSY_TAR
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
      m=m+1
      if(gen_event_type.eq.6) then
         if(gen_event_trigtype(5).eq.1) then
            h_ntuple_contents(m) = 5.
         else if(gen_event_trigtype(4).eq.1) then
            h_ntuple_contents(m) = 4.
         endif
      else
         if(gen_event_trigtype(2).eq.1) then
            h_ntuple_contents(m) = 2.
         else if(gen_event_trigtype(1).eq.1) then
            h_ntuple_contents(m) = 1.
         endif
      endif
c
      do ihit=1,hscin_all_tot_hits
         pln=hscin_all_plane_num(ihit)
         cnt=hscin_all_counter_num(ihit)
         if ( pln .eq. 3) then
            if (cnt .eq. 1) then
               s0x1nadc = hscin_all_adc_neg(ihit)
     >              -hscin_all_ped_neg(pln,cnt)
               s0x1padc = hscin_all_adc_pos(ihit)
     > -hscin_all_ped_neg(pln,cnt)
               s0x1ntdc = FLOAT(hscin_all_tdc_neg(ihit)) 
               s0x1ptdc = FLOAT(hscin_all_tdc_pos(ihit)) 
            endif
            if (cnt .eq. 2) then
               s0x2nadc = hscin_all_adc_neg(ihit)
     >              -hscin_all_ped_neg(pln,cnt)
               s0x2padc = hscin_all_adc_pos(ihit)
     >              -hscin_all_ped_neg(pln,cnt)
               s0x2ntdc = FLOAT(hscin_all_tdc_neg(ihit)) 
               s0x2ptdc = FLOAT(hscin_all_tdc_pos(ihit)) 
            endif
         endif
      enddo
c
      m= m+1
      h_Ntuple_contents(m)= s0x1padc
      m= m+1
      h_Ntuple_contents(m)= s0x1nadc
      m= m+1
      h_Ntuple_contents(m)= s0x2padc
      m= m+1
      h_Ntuple_contents(m)= s0x2nadc
      m= m+1
      h_Ntuple_contents(m)= s0x1ptdc
      m= m+1
      h_Ntuple_contents(m)= s0x1ntdc
      m= m+1
      h_Ntuple_contents(m)= s0x2ptdc
      m= m+1
      h_Ntuple_contents(m)= s0x2ntdc
      m= m+1
      h_Ntuple_contents(m)= gfry_raw_adc
      m= m+1
      h_Ntuple_contents(m)= gfrx_raw_adc
      m= m+1
      h_Ntuple_contents(m)= hncham_hits(1)
      m= m+1
      h_Ntuple_contents(m)= hncham_hits(2)
      m=m+1
      h_ntuple_contents(m) = hntracks_fp
      m=m+1
      h_ntuple_contents(m) = hntrack_hits(hsnum_fptrack,1)
      m=m+1
      h_ntuple_contents(m) = hschi2perdeg
      m=m+1
      h_ntuple_contents(m) = gfry
      m=m+1
      h_ntuple_contents(m) = -gfry - hsy_tar * (coshthetas / tan(htheta_lab*degree - atan(hsyp_tar)) + sinhthetas) * 
     $     coshthetas * hsxp_tar

c     get wire number of hit on track at each plane choose only one per plane
c     if more than one per plane, last will be chosen!!!
c     also get wire center, measured position, and track position at each plane

      if(h_ntup_include_trackhits.ne.0) then

         m=m+1
         h_ntuple_contents(m) = htrack_t0best(hsnum_fptrack)

         do ihit=1,12
            wire(ihit) = 0
            wirecent(ihit) = 0.
            wirepos(ihit) = 0.
            trackpos(ihit) = 0.
         enddo

         do ihit=1,hntrack_hits(hsnum_fptrack,1)
            hitnum = hntrack_hits(hsnum_fptrack,ihit+1)
            plane = hdc_plane_num(hitnum)
            wire(plane) = hdc_wire_num(hitnum)
            wirecent(plane) = hdc_wire_center(hitnum)
c     wirepos(plane) = hdc_wire_coord(hitnum) ! don't use hdc_wire_coord, use the following:

            ddrift = hdc_drift_dis(hitnum)

            if(htrack_t0best(hsnum_fptrack).ne.0.) then
               tdrift = hdc_drift_time(hitnum) + htrack_t0best(hsnum_fptrack)
               ddrift = h_drift_dist_calc(plane,wire(plane),tdrift)
            endif

            wirepos(plane) = wirecent(plane) + htrack_leftright(hsnum_fptrack,ihit) * 
     $           ddrift

            trackpos(plane) = hsdc_track_coord(plane)
         
         enddo
         
         do plane = 1,12
            m=m+1
            h_ntuple_contents(m) = float(wire(plane))
         enddo
      
         do plane = 1,12
            m=m+1
            h_ntuple_contents(m) = wirecent(plane)
         enddo
      
         do plane = 1,12
            m=m+1
            h_ntuple_contents(m) = wirepos(plane)
         enddo

         do plane = 1,12
            m=m+1
            h_ntuple_contents(m) = trackpos(plane)
         enddo

         m=m+1
         h_ntuple_contents(m) = hsx_sp1
         m=m+1
         h_ntuple_contents(m) = hsy_sp1
         m=m+1
         h_ntuple_contents(m) = hsxp_sp1
         m=m+1
         h_ntuple_contents(m) = hsx_sp2
         m=m+1
         h_ntuple_contents(m) = hsy_sp2
         m=m+1
         h_ntuple_contents(m) = hsxp_sp2
      endif

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
