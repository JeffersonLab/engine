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
* $Log$
* Revision 1.1  1995/02/23 15:42:08  cdaq
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
      INCLUDE 'gen_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      include 'sos_scin_parms.cmn'
      include 'sos_scin_tof.cmn'
      include 'sos_statistics.cmn'

      integer pln,cnt
      integer bothgood(snum_scin_planes,snum_scin_elements)
      integer posgood(snum_scin_planes,snum_scin_elements)
      integer neggood(snum_scin_planes,snum_scin_elements)
      integer hit_cnt(snum_scin_planes)
      integer nhit
      real    hit_pos(snum_scin_planes),hit_dist(snum_scin_planes)
      save

* find counters on track, and distance from center.

      if (sschi2perdeg.le.sstat_maxchisq) sstat_numevents=sstat_numevents+1

      hit_pos(1)=hsx_fp + hsxp_fp*(sscin_1x_zpos+0.5*sscin_1x_dzpos)
      hit_cnt(1)=nint((hit_pos(1)-shodo_center(1,1))/sscin_1x_spacing)+1
      hit_cnt(1)=max(min(hit_cnt(1),snum_scin_counters(1)),1)
      hit_dist(1)=hit_pos(1)-(sscin_1x_spacing*(hit_cnt(1)-1)+shodo_center(1,1))

      hit_pos(2)=hsy_fp + hsyp_fp*(sscin_1y_zpos+0.5*sscin_1y_dzpos)
      hit_cnt(2)=nint((shodo_center(2,1)-hit_pos(2))/sscin_1y_spacing)+1
      hit_cnt(2)=max(min(hit_cnt(2),snum_scin_counters(1)),1)
      hit_dist(2)=hit_pos(2)-(shodo_center(2,1)-sscin_1y_spacing*(hit_cnt(2)-1))

      hit_pos(3)=hsx_fp + hsxp_fp*(sscin_2x_zpos+0.5*sscin_2x_dzpos)
      hit_cnt(3)=nint((hit_pos(3)-shodo_center(3,1))/sscin_2x_spacing)+1
      hit_cnt(3)=max(min(hit_cnt(3),snum_scin_counters(1)),1)
      hit_dist(3)=hit_pos(3)-(sscin_2x_spacing*(hit_cnt(3)-1)+shodo_center(3,1))

      hit_pos(4)=hsy_fp + hsyp_fp*(sscin_2y_zpos+0.5*sscin_2y_dzpos)
      hit_cnt(4)=nint((shodo_center(4,1)-hit_pos(4))/sscin_2y_spacing)+1
      hit_cnt(4)=max(min(hit_cnt(4),snum_scin_counters(1)),1)
      hit_dist(4)=hit_pos(4)-(shodo_center(4,1)-sscin_2y_spacing*(hit_cnt(4)-1))

*   increment 'should have hit' counters
      do pln=1,snum_scin_planes
        if(abs(hit_dist(pln)).le.sstat_slop .and.    !hit in middle of scin.
     &           sschi2perdeg.le.sstat_maxchisq) then
          sstat_trk(pln,hit_cnt(pln))=sstat_trk(pln,hit_cnt(pln))+1
        endif
      enddo

c      do ind=1,ssnum_scin_hit
c        nhit=sscin_hit(ssnum_fptrack,ind)
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
            bothgood(pln,cnt)=bothgood(pln,cnt)+1
          else                            !pos fired
            posgood(pln,cnt)=posgood(pln,cnt)+1
          endif
        else
          if (sgood_tdc_neg(ssnum_fptrack,nhit)) then    !neg fired
            neggood(pln,cnt)=neggood(pln,cnt)+1
          endif
        endif

      enddo                 !loop over ssnum_pmt_hit


! fill sums over counters
c      do pln=1,snum_scin_planes
c          sstat_trksum(pln)=0
c          sstat_possum(pln)=0
c          sstat_negsum(pln)=0
c          sstat_andsum(pln)=0
c          sstat_orsum(pln)=0
c        do cnt=1,snum_scin_counters(pln)
c          sstat_trksum(pln)=sstat_trksum(pln)+sstat_trk(pln,cnt)
c          sstat_possum(pln)=sstat_possum(pln)+sstat_poshit(pln,cnt)
c          sstat_negsum(pln)=sstat_negsum(pln)+sstat_neghit(pln,cnt)
c          sstat_andsum(pln)=sstat_andsum(pln)+sstat_andhit(pln,cnt)
c          sstat_orsum(pln)=sstat_orsum(pln)+sstat_orhit(pln,cnt)
c        enddo
c      enddo

      return
      end
