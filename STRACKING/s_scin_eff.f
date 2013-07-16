      SUBROUTINE S_SCIN_EFF(ABORT,errmsg)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Analyze scintillator information for each track 
*-
*-      Required Input BANKS     SOS_SCIN_TOF
*-                               GEN_DATA_STRUCTURES
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
* author: John Arrington
* created: 2/13/95
*
* s_scin_eff calculates efficiencies for the hodoscope.
*
* $Log: s_scin_eff.f,v $
* Revision 1.8  2003/09/05 20:01:02  jones
* Merge in online03 changes (mkj)
*
* Revision 1.7.2.1  2003/04/02 22:27:03  cdaq
* added some extra scint. effic calculations (from oct 1999 online) - JRA
*
* Revision 1.7  1996/01/17 18:59:15  cdaq
* (JRA) Fix typos
*
* Revision 1.6  1995/08/31 15:08:15  cdaq
* (JRA) Fill dpos (pos. track - pos. hit) histograms
*
* Revision 1.5  1995/07/20  19:00:29  cdaq
* (SAW) Put nint around some things for Ultrix compat.  Put h in front of
*       various *good variables.
*
* Revision 1.4  1995/05/22  19:45:54  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.3  1995/05/11  21:17:23  cdaq
* (JRA) Add position calibration variables
*
* Revision 1.2  1995/04/06  19:43:37  cdaq
* (JRA) Fix some latent HMS variable names
*
* Revision 1.1  1995/02/23  15:42:08  cdaq
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
*
      character*50 here
      parameter (here= 'S_SCIN_EFF')
*
      logical ABORT
      character*(*) errmsg
*
      INCLUDE 'sos_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      include 'sos_scin_parms.cmn'
      include 'sos_scin_tof.cmn'
      include 'sos_statistics.cmn'
      include 'sos_id_histid.cmn'

      integer pln,cnt,pln2
      integer hit_cnt(snum_scin_planes)
      integer nhit
      real dist, histval
      real hit_pos(snum_scin_planes),hit_dist(snum_scin_planes)
      real xatback,yatback

      logical good_tdc_oneside(snum_scin_planes)
      logical good_tdc_bothsides(snum_scin_planes)
      logical otherthreehit

      save

* find counters on track, and distance from center.

      if (sschi2perdeg.le.sstat_maxchisq) sstat_numevents=sstat_numevents+1

      hit_pos(1)=ssx_fp + ssxp_fp*(sscin_1x_zpos+0.5*sscin_1x_dzpos)
      hit_cnt(1)=nint((hit_pos(1)-shodo_center(1,1))/sscin_1x_spacing)+1
      hit_cnt(1)=max(min(hit_cnt(1),nint(snum_scin_counters(1))),1)
      hit_dist(1)=hit_pos(1)-(sscin_1x_spacing*(hit_cnt(1)-1)+shodo_center(1,1))

      hit_pos(2)=ssy_fp + ssyp_fp*(sscin_1y_zpos+0.5*sscin_1y_dzpos)
      hit_cnt(2)=nint((shodo_center(2,1)-hit_pos(2))/sscin_1y_spacing)+1
      hit_cnt(2)=max(min(hit_cnt(2),nint(snum_scin_counters(2))),1)
      hit_dist(2)=hit_pos(2)-(shodo_center(2,1)-sscin_1y_spacing*(hit_cnt(2)-1))

      hit_pos(3)=ssx_fp + ssxp_fp*(sscin_2x_zpos+0.5*sscin_2x_dzpos)
      hit_cnt(3)=nint((hit_pos(3)-shodo_center(3,1))/sscin_2x_spacing)+1
      hit_cnt(3)=max(min(hit_cnt(3),nint(snum_scin_counters(3))),1)
      hit_dist(3)=hit_pos(3)-(sscin_2x_spacing*(hit_cnt(3)-1)+shodo_center(3,1))

      hit_pos(4)=ssy_fp + ssyp_fp*(sscin_2y_zpos+0.5*sscin_2y_dzpos)
      hit_cnt(4)=nint((shodo_center(4,1)-hit_pos(4))/sscin_2y_spacing)+1
      hit_cnt(4)=max(min(hit_cnt(4),nint(snum_scin_counters(4))),1)
      hit_dist(4)=hit_pos(4)-(shodo_center(4,1)-sscin_2y_spacing*(hit_cnt(4)-1))

      do pln=1,snum_scin_planes
        good_tdc_oneside(pln) = .false.
        good_tdc_bothsides(pln) = .false.
      enddo


*   Fill dpos (pos. track - pos. hit) histograms
      do nhit=1,sscin_tot_hits
        pln=sscin_plane_num(nhit)
        histval = shodo_center(pln,sscin_counter_num(nhit))-hit_pos(pln)
        call hf1(sidscindpos(pln),histval,1.)
      enddo

*   Record position differences between track and center of scin. and
*   increment 'should have hit' counters
      do pln=1,snum_scin_planes
        cnt=hit_cnt(pln)
        dist=hit_dist(pln)
        if(abs(dist).le.sstat_slop .and.    !hit in middle of scin.
     &           sschi2perdeg.le.sstat_maxchisq) then
          sstat_trk(pln,hit_cnt(pln))=sstat_trk(pln,hit_cnt(pln))+1
        endif
      enddo

      do nhit=1,sscin_tot_hits
        cnt=sscin_counter_num(nhit)
        pln=sscin_plane_num(nhit)

*  Record the hits if track is near center of track and the chisquared of the 
*  track is good.
        if(abs(hit_dist(pln)).le.sstat_slop .and. cnt.eq.hit_cnt(pln) .and. 
     &          sschi2perdeg.le.sstat_maxchisq) then

          if (sgood_tdc_pos(ssnum_fptrack,nhit)) then
            if (sgood_tdc_neg(ssnum_fptrack,nhit)) then    !both fired
              sstat_poshit(pln,hit_cnt(pln))=sstat_poshit(pln,hit_cnt(pln))+1
              sstat_neghit(pln,hit_cnt(pln))=sstat_neghit(pln,hit_cnt(pln))+1
              sstat_andhit(pln,hit_cnt(pln))=sstat_andhit(pln,hit_cnt(pln))+1
              sstat_orhit(pln,hit_cnt(pln))=sstat_orhit(pln,hit_cnt(pln))+1
            else                            !pos fired
              sstat_poshit(pln,hit_cnt(pln))=sstat_poshit(pln,hit_cnt(pln))+1
              sstat_orhit(pln,hit_cnt(pln))=sstat_orhit(pln,hit_cnt(pln))+1
            endif
          else   !no pos tdc
            if (sgood_tdc_neg(ssnum_fptrack,nhit)) then    !neg fired
              sstat_neghit(pln,hit_cnt(pln))=sstat_neghit(pln,hit_cnt(pln))+1
              sstat_orhit(pln,hit_cnt(pln))=sstat_orhit(pln,hit_cnt(pln))+1
            endif       !if neg tdc fired.
          endif       !if pos tdc fired.

        endif       !if hit was on good track.


*   Increment pos/neg/both fired.  Track indepenant, so no chisquared cut (but
*   note that only scintillators on the track are examined.

        if (sgood_tdc_pos(ssnum_fptrack,nhit)) then
          if (sgood_tdc_neg(ssnum_fptrack,nhit)) then    !both fired
            sbothgood(pln,cnt)=sbothgood(pln,cnt)+1
          else                            !pos fired
            sposgood(pln,cnt)=sposgood(pln,cnt)+1
          endif
        else
          if (sgood_tdc_neg(ssnum_fptrack,nhit)) then    !neg fired
            sneggood(pln,cnt)=sneggood(pln,cnt)+1
          endif
        endif

*  Determine if one or both PMTs had a good tdc.
        if (sgood_tdc_pos(ssnum_fptrack,nhit) .and. 
     &      sgood_tdc_neg(ssnum_fptrack,nhit) ) good_tdc_bothsides(pln)=.true.
        if (sgood_tdc_pos(ssnum_fptrack,nhit) .or. 
     &      sgood_tdc_neg(ssnum_fptrack,nhit) ) good_tdc_oneside(pln)=.true.

      enddo                 !loop over ssnum_pmt_hit


*  For each plane, see of other 3 fired.  This means that they were enough
*  to form a 3/4 trigger, and so the fraction of times this plane fired is
*  the plane trigger efficiency.  NOTE: we only require a TDC hit, not a
*  TDC hit within the SCIN 3/4 trigger window, so high rates will make
*  this seem better than it is.  Also, make sure we're not near the edge
*  of the hodoscope (at the last plane), using the same shodo_slop param. as for h_tof.f
*  NOTE ALSO: to make this check simpler, we are assuming that all planes
*  have identical active areas.  y_scin = y_cent + y_offset, so shift track
*  position by offset for comparing to edges.

      xatback = ssx_fp+ssxp_fp*sscin_2y_zpos - sscin_2x_offset
      yatback = ssy_fp+ssyp_fp*sscin_2y_zpos - sscin_2y_offset

      if ( xatback.lt.(sscin_2y_bot  -2.*shodo_slop(3))  .and.
     &     xatback.gt.(sscin_2y_top  +2.*shodo_slop(3))  .and.
     &     yatback.lt.(sscin_2x_left -2.*shodo_slop(3))  .and.
     &     yatback.gt.(sscin_2x_right+2.*shodo_slop(3))) then

        do pln=1,snum_scin_planes
          otherthreehit=.true.
          do pln2=1,snum_scin_planes         !see of one of the others missed or pln2=pln
            if (.not.(good_tdc_bothsides(pln2) .or. pln2.eq.pln)) then
              otherthreehit=.false.
            endif
          enddo
          if (otherthreehit) then
            strig_hodoshouldflag(pln) = .true.
            if (good_tdc_bothsides(pln)) then
              strig_hododidflag(pln) = .true.
            else
              strig_hododidflag(pln) = .false.
            endif
          else
            strig_hodoshouldflag(pln) = .false.
            strig_hododidflag(pln) = .false.
          endif
        enddo

      else            !outside of fiducial region
        do pln=1,snum_scin_planes
          strig_hodoshouldflag(pln) = .false.
          strig_hododidflag(pln) = .false.
        enddo
      endif
      return
      end
