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
* Revision 1.1  1995/02/23 13:31:41  cdaq
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
      INCLUDE 'gen_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      include 'hms_scin_parms.cmn'
      include 'hms_scin_tof.cmn'
      include 'hms_statistics.cmn'

      integer pln,cnt
      integer bothgood(hnum_scin_planes,hnum_scin_elements)
      integer posgood(hnum_scin_planes,hnum_scin_elements)
      integer neggood(hnum_scin_planes,hnum_scin_elements)
      integer hit_cnt(hnum_scin_planes)
      integer nhit
      real    hit_pos(hnum_scin_planes),hit_dist(hnum_scin_planes)
      save

* find counters on track, and distance from center.

      if (hschi2perdeg.le.hstat_maxchisq) hstat_numevents=hstat_numevents+1

      hit_pos(1)=hsx_fp + hsxp_fp*(hscin_1x_zpos+0.5*hscin_1x_dzpos)
      hit_cnt(1)=nint((hit_pos(1)-hhodo_center(1,1))/hscin_1x_spacing)+1
      hit_cnt(1)=max(min(hit_cnt(1),hnum_scin_counters(1)),1)
      hit_dist(1)=hit_pos(1)-(hscin_1x_spacing*(hit_cnt(1)-1)+hhodo_center(1,1))

      hit_pos(2)=hsy_fp + hsyp_fp*(hscin_1y_zpos+0.5*hscin_1y_dzpos)
      hit_cnt(2)=nint((hhodo_center(2,1)-hit_pos(2))/hscin_1y_spacing)+1
      hit_cnt(2)=max(min(hit_cnt(2),hnum_scin_counters(1)),1)
      hit_dist(2)=hit_pos(2)-(hhodo_center(2,1)-hscin_1y_spacing*(hit_cnt(2)-1))

      hit_pos(3)=hsx_fp + hsxp_fp*(hscin_2x_zpos+0.5*hscin_2x_dzpos)
      hit_cnt(3)=nint((hit_pos(3)-hhodo_center(3,1))/hscin_2x_spacing)+1
      hit_cnt(3)=max(min(hit_cnt(3),hnum_scin_counters(1)),1)
      hit_dist(3)=hit_pos(3)-(hscin_2x_spacing*(hit_cnt(3)-1)+hhodo_center(3,1))

      hit_pos(4)=hsy_fp + hsyp_fp*(hscin_2y_zpos+0.5*hscin_2y_dzpos)
      hit_cnt(4)=nint((hhodo_center(4,1)-hit_pos(4))/hscin_2y_spacing)+1
      hit_cnt(4)=max(min(hit_cnt(4),hnum_scin_counters(1)),1)
      hit_dist(4)=hit_pos(4)-(hhodo_center(4,1)-hscin_2y_spacing*(hit_cnt(4)-1))

*   increment 'should have hit' counters
      do pln=1,hnum_scin_planes
        if(abs(hit_dist(pln)).le.hstat_slop .and.    !hit in middle of scin.
     &           hschi2perdeg.le.hstat_maxchisq) then
          hstat_trk(pln,hit_cnt(pln))=hstat_trk(pln,hit_cnt(pln))+1
        endif
      enddo

c      do ind=1,hsnum_scin_hit
c        nhit=hscin_hit(hsnum_fptrack,ind)
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
            bothgood(pln,cnt)=bothgood(pln,cnt)+1
          else                            !pos fired
            posgood(pln,cnt)=posgood(pln,cnt)+1
          endif
        else
          if (hgood_tdc_neg(hsnum_fptrack,nhit)) then    !neg fired
            neggood(pln,cnt)=neggood(pln,cnt)+1
          endif
        endif

      enddo                 !loop over hsnum_pmt_hit


! fill sums over counters
c      do pln=1,hnum_scin_planes
c          hstat_trksum(pln)=0
c          hstat_possum(pln)=0
c          hstat_negsum(pln)=0
c          hstat_andsum(pln)=0
c          hstat_orsum(pln)=0
c        do cnt=1,hnum_scin_counters(pln)
c          hstat_trksum(pln)=hstat_trksum(pln)+hstat_trk(pln,cnt)
c          hstat_possum(pln)=hstat_possum(pln)+hstat_poshit(pln,cnt)
c          hstat_negsum(pln)=hstat_negsum(pln)+hstat_neghit(pln,cnt)
c          hstat_andsum(pln)=hstat_andsum(pln)+hstat_andhit(pln,cnt)
c          hstat_orsum(pln)=hstat_orsum(pln)+hstat_orhit(pln,cnt)
c        enddo
c      enddo

      return
      end
