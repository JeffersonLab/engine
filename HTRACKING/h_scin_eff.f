      SUBROUTINE H_SCIN_EFF(ABORT,errmsg)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Analyze scintillator information for each track 
*-
*-      Required Input BANKS     HMS_SCIN_TOF
*-                               GEN_DATA_STRUCTURES
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
* author: John Arrington
* created: 2/13/95
*
* h_scin_eff calculates efficiencies for the hodoscope.
*
* $Log$
* Revision 1.7.2.2  2003/04/03 14:02:13  cdaq
* Remove extra enddo (JRA)
*
* Revision 1.7.2.1  2003/04/02 22:26:55  cdaq
* added some extra scint. effic calculations (from oct 1999 online) - JRA
*
* Revision 1.7  2002/10/02 13:42:43  saw
* Check that user hists are defined before filling
*
* Revision 1.6  1996/01/16 21:56:40  cdaq
* (JRA) Fix typos
*
* Revision 1.5  1995/08/31 14:44:42  cdaq
* (JRA) Fill dpos (pos. track - pos. hit) histograms
*
* Revision 1.4  1995/07/19  19:03:27  cdaq
* (SAW) Put nint around some things for Ultrix compat.  Put h in front of
*       various *good variables.
*
* Revision 1.3  1995/05/22  19:39:26  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.2  1995/05/11  20:27:21  cdaq
* (JRA) Add position calibration variables
*
* Revision 1.1  1995/02/23  13:31:41  cdaq
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
*
      character*50 here
      parameter (here= 'H_SCIN_EFF')
*
      logical ABORT
      character*(*) errmsg
*
      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      include 'hms_scin_parms.cmn'
      include 'hms_scin_tof.cmn'
      include 'hms_statistics.cmn'
      include 'hms_id_histid.cmn'

      integer pln,cnt,pln2
      integer hit_cnt(hnum_scin_planes)
      integer nhit
      real dist, histval
      real hit_pos(hnum_scin_planes),hit_dist(hnum_scin_planes)
      real xatback,yatback

      logical good_tdc_oneside(hnum_scin_planes)
      logical good_tdc_bothsides(hnum_scin_planes)
      logical otherthreehit

      save

* find counters on track, and distance from center.

      if (hschi2perdeg.le.hstat_maxchisq) hstat_numevents=hstat_numevents+1

      hit_pos(1)=hsx_fp + hsxp_fp*(hscin_1x_zpos+0.5*hscin_1x_dzpos)
      hit_cnt(1)=nint((hit_pos(1)-hhodo_center(1,1))/hscin_1x_spacing)+1
      hit_cnt(1)=max(min(hit_cnt(1),nint(hnum_scin_counters(1))),1)
      hit_dist(1)=hit_pos(1)-(hscin_1x_spacing*(hit_cnt(1)-1)+hhodo_center(1,1))

      hit_pos(2)=hsy_fp + hsyp_fp*(hscin_1y_zpos+0.5*hscin_1y_dzpos)
      hit_cnt(2)=nint((hhodo_center(2,1)-hit_pos(2))/hscin_1y_spacing)+1
      hit_cnt(2)=max(min(hit_cnt(2),nint(hnum_scin_counters(2))),1)
      hit_dist(2)=hit_pos(2)-(hhodo_center(2,1)-hscin_1y_spacing*(hit_cnt(2)-1))

      hit_pos(3)=hsx_fp + hsxp_fp*(hscin_2x_zpos+0.5*hscin_2x_dzpos)
      hit_cnt(3)=nint((hit_pos(3)-hhodo_center(3,1))/hscin_2x_spacing)+1
      hit_cnt(3)=max(min(hit_cnt(3),nint(hnum_scin_counters(3))),1)
      hit_dist(3)=hit_pos(3)-(hscin_2x_spacing*(hit_cnt(3)-1)+hhodo_center(3,1))

      hit_pos(4)=hsy_fp + hsyp_fp*(hscin_2y_zpos+0.5*hscin_2y_dzpos)
      hit_cnt(4)=nint((hhodo_center(4,1)-hit_pos(4))/hscin_2y_spacing)+1
      hit_cnt(4)=max(min(hit_cnt(4),nint(hnum_scin_counters(4))),1)
      hit_dist(4)=hit_pos(4)-(hhodo_center(4,1)-hscin_2y_spacing*(hit_cnt(4)-1))

      do pln=1,hnum_scin_planes
        good_tdc_oneside(pln) = .false.
        good_tdc_bothsides(pln) = .false.
      enddo


*   Fill dpos (pos. track - pos. hit) histograms
      do nhit=1,hscin_tot_hits
        pln=hscin_plane_num(nhit)
        histval = hhodo_center(pln,hscin_counter_num(nhit))-hit_pos(pln)
        if(hidscindpos(pln).gt.0) call hf1(hidscindpos(pln),histval,1.)
      enddo

*   Record position differences between track and center of scin. and
*   increment 'should have hit' counters
      do pln=1,hnum_scin_planes
        cnt=hit_cnt(pln)
        dist=hit_dist(pln)
        if(abs(dist).le.hstat_slop .and.    !hit in middle of scin.
     &           hschi2perdeg.le.hstat_maxchisq) then
          hstat_trk(pln,cnt)=hstat_trk(pln,cnt)+1
        endif
      enddo

      do nhit=1,hscin_tot_hits
        cnt=hscin_counter_num(nhit)
        pln=hscin_plane_num(nhit)

*  Record the hits if track is near center of track and the chisquared of the 
*  track is good.
        if(abs(hit_dist(pln)).le.hstat_slop .and. cnt.eq.hit_cnt(pln) .and. 
     &          hschi2perdeg.le.hstat_maxchisq) then

          if (hgood_tdc_pos(hsnum_fptrack,nhit)) then
            if (hgood_tdc_neg(hsnum_fptrack,nhit)) then    !both fired
              hstat_poshit(pln,hit_cnt(pln))=hstat_poshit(pln,hit_cnt(pln))+1
              hstat_neghit(pln,hit_cnt(pln))=hstat_neghit(pln,hit_cnt(pln))+1
              hstat_andhit(pln,hit_cnt(pln))=hstat_andhit(pln,hit_cnt(pln))+1
              hstat_orhit(pln,hit_cnt(pln))=hstat_orhit(pln,hit_cnt(pln))+1
            else                            !pos fired
              hstat_poshit(pln,hit_cnt(pln))=hstat_poshit(pln,hit_cnt(pln))+1
              hstat_orhit(pln,hit_cnt(pln))=hstat_orhit(pln,hit_cnt(pln))+1
            endif
          else   !no pos tdc
            if (hgood_tdc_neg(hsnum_fptrack,nhit)) then    !neg fired
              hstat_neghit(pln,hit_cnt(pln))=hstat_neghit(pln,hit_cnt(pln))+1
              hstat_orhit(pln,hit_cnt(pln))=hstat_orhit(pln,hit_cnt(pln))+1
            endif       !if neg tdc fired.
          endif       !if pos tdc fired.

        endif       !if hit was on good track.


*   Increment pos/neg/both fired.  Track indepenant, so no chisquared cut (but
*   note that only scintillators on the track are examined.

        if (hgood_tdc_pos(hsnum_fptrack,nhit)) then
          if (hgood_tdc_neg(hsnum_fptrack,nhit)) then    !both fired
            hbothgood(pln,cnt)=hbothgood(pln,cnt)+1
          else                            !pos fired
            hposgood(pln,cnt)=hposgood(pln,cnt)+1
          endif
        else
          if (hgood_tdc_neg(hsnum_fptrack,nhit)) then    !neg fired
            hneggood(pln,cnt)=hneggood(pln,cnt)+1
          endif
        endif

*  Determine if one or both PMTs had a good tdc.
        if (hgood_tdc_pos(hsnum_fptrack,nhit) .and. 
     &      hgood_tdc_neg(hsnum_fptrack,nhit) ) good_tdc_bothsides(pln)=.true.
        if (hgood_tdc_pos(hsnum_fptrack,nhit) .or. 
     &      hgood_tdc_neg(hsnum_fptrack,nhit) ) good_tdc_oneside(pln)=.true.

      enddo                 !loop over hsnum_pmt_hit

*  For each plane, see of other 3 fired.  This means that they were enough
*  to form a 3/4 trigger, and so the fraction of times this plane fired is
*  the plane trigger efficiency.  NOTE: we only require a TDC hit, not a
*  TDC hit within the SCIN 3/4 trigger window, so high rates will make
*  this seem better than it is.  Also, make sure we're not near the edge
*  of the hodoscope (at the last plane), using the same hhodo_slop param. as for h_tof.f
*  NOTE ALSO: to make this check simpler, we are assuming that all planes
*  have identical active areas.  y_scin = y_cent + y_offset, so shift track
*  position by offset for comparing to edges.

      xatback = hsx_fp+hsxp_fp*hscin_2y_zpos - hscin_2x_offset
      yatback = hsy_fp+hsyp_fp*hscin_2y_zpos - hscin_2y_offset

      if ( xatback.lt.(hscin_2y_bot  -2.*hhodo_slop(3))  .and.
     &     xatback.gt.(hscin_2y_top  +2.*hhodo_slop(3))  .and.
     &     yatback.lt.(hscin_2x_left -2.*hhodo_slop(3))  .and.
     &     yatback.gt.(hscin_2x_right+2.*hhodo_slop(3))) then

        do pln = 1,hnum_scin_planes
          otherthreehit = .true.
          do pln2 = 1,hnum_scin_planes	!see of one of the others missed or pln2=pln
	    if (.not.(good_tdc_bothsides(pln2) .or. pln2.eq.pln)) then
	      otherthreehit = .false.
	    endif
	  enddo
	  if (otherthreehit) then
	    htrig_hodoshouldflag(pln) = .true.
	    if (good_tdc_bothsides(pln)) then
              htrig_hododidflag(pln) = .true.
	    else
              htrig_hododidflag(pln) = .false.
	    endif
          else
	    htrig_hodoshouldflag(pln) = .false.
	    htrig_hododidflag(pln) = .false.
	  endif
	enddo

      else		!outside of fiducial region
        do pln=1,hnum_scin_planes
          htrig_hodoshouldflag(pln) = .false.
          htrig_hododidflag(pln) = .false.
        enddo
      endif

      return
      end
