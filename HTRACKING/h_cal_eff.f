      SUBROUTINE H_CAL_EFF(ABORT,errmsg)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Analyze calorimeter statistics for each track 
*-
*-      Required Input BANKS     HMS_CALORIMETER
*-                               GEN_DATA_STRUCTURES
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
* author: John Arrington
* created: 2/17/95
*
* h_cal_eff calculates efficiencies for the hodoscope.
*
* $Log: h_cal_eff.f,v $
* Revision 1.8  2002/10/02 13:42:42  saw
* Check that user hists are defined before filling
*
* Revision 1.7  1999/02/03 21:13:23  saw
* Code for new Shower counter tubes
*
* Revision 1.6  1999/01/29 17:33:56  saw
* Cosmetic changes
*
* Revision 1.5  1998/12/17 22:02:38  saw
* Support extra set of tubes on HMS shower counter
*
* Revision 1.4  1996/08/30 19:52:12  saw
* (JRA) Require more than one photoelectron
*
* Revision 1.3  1995/08/31 14:55:37  cdaq
* (JRA) Fill dpos (pos. track - pos. hit) histograms
*
* Revision 1.2  1995/05/22  19:39:05  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.1  1995/02/23  13:31:51  cdaq
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
*
      character*9 here
      parameter (here= 'H_CAL_EFF')
*
      logical ABORT
      character*(*) errmsg
*
      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      include 'hms_calorimeter.cmn'
      include 'hms_statistics.cmn'
      include 'hms_id_histid.cmn'

      integer col,row,blk
      integer hit_row(hmax_cal_columns)
      integer nhit
      real    adc_pos, adc_neg
      real    hit_pos(hmax_cal_columns),hit_dist(hmax_cal_columns)
      real    histval
      save

* find counters on track, and distance from center.

      if (hschi2perdeg.le.hstat_cal_maxchisq .and. hcer_npe_sum.ge.1.)
     &       hstat_cal_numevents=hstat_cal_numevents+1

      hit_pos(1)=hsx_fp + hsxp_fp*(hcal_1pr_zpos+0.5*hcal_1pr_thick)
      hit_row(1)=nint((hit_pos(1)-hcal_block_xc(1))
     &          /hcal_block_xsize)+1
      hit_row(1)=max(min(hit_row(1),hmax_cal_rows),1)
      hit_dist(1)=hit_pos(1)-(hcal_block_xsize*(hit_row(1)-1)
     &           +hcal_block_xc(1))

      hit_pos(2)=hsx_fp + hsxp_fp*(hcal_2ta_zpos+0.5*hcal_2ta_thick)
      hit_row(2)=nint((hit_pos(2)-hcal_block_xc(hmax_cal_rows+1))
     &          /hcal_block_xsize)+1
      hit_row(2)=max(min(hit_row(2),hmax_cal_rows),1)
      hit_dist(2)=hit_pos(2)-(hcal_block_xsize*(hit_row(2)-1)
     &           +hcal_block_xc(hmax_cal_rows+1))

      hit_pos(3)=hsx_fp + hsxp_fp*(hcal_3ta_zpos+0.5*hcal_3ta_thick)
      hit_row(3)=nint((hit_pos(3)-hcal_block_xc(2*hmax_cal_rows+1))
     &          /hcal_block_xsize)+1
      hit_row(3)=max(min(hit_row(3),hmax_cal_rows),1)
      hit_dist(3)=hit_pos(3)-(hcal_block_xsize*(hit_row(3)-1)
     &           +hcal_block_xc(2*hmax_cal_rows+1))

      hit_pos(4)=hsx_fp + hsxp_fp*(hcal_4ta_zpos+0.5*hcal_4ta_thick)
      hit_row(4)=nint((hit_pos(4)-hcal_block_xc(3*hmax_cal_rows+1))
     &          /hcal_block_xsize)+1
      hit_row(4)=max(min(hit_row(4),hmax_cal_rows),1)
      hit_dist(4)=hit_pos(3)-(hcal_block_xsize*(hit_row(4)-1)
     &           +hcal_block_xc(3*hmax_cal_rows+1))

*   increment 'should have hit' counters
      do col=1,hmax_cal_columns
        if(abs(hit_dist(col)).le.hstat_cal_slop .and.    !hit in middle of blk.
     &           hschi2perdeg.le.hstat_cal_maxchisq .and. hcer_npe_sum.ge.1.) then
          hstat_cal_trk(col,hit_row(col))=hstat_cal_trk(col,hit_row(col))+1
        endif
      enddo

      do nhit=1,hcal_num_hits
        row=hcal_rows(nhit)
        col=hcal_cols(nhit)
* We don't actually do anything with the following values?
        adc_pos=hcal_adcs_pos(nhit)  ! Do we want hcal_adc_pos or hcal_adcs_pos
        adc_neg=hcal_adcs_neg(nhit)
        blk=row+hmax_cal_rows*(col-1)

* fill the dpos histograms.
        if (col .eq. 1) then
          histval=(hcal_block_xc(1)+hcal_block_xsize*(row-1))-hit_pos(1)
          if(hidcaldpos.gt.0) call hf1(hidcaldpos,histval,1.)
        endif

*  Record the hits if track is near center of block and the chisquared of the 
*  track is good
        if(abs(hit_dist(col)).le.hstat_cal_slop .and. row.eq.hit_row(col)) then
          if (hschi2perdeg.le.hstat_cal_maxchisq .and. hcer_npe_sum.ge.1.) then
            hstat_cal_hit(col,hit_row(col))=hstat_cal_hit(col,hit_row(col))+1
          endif     !was it a good track.
        endif     !if hit was on track.
      enddo

      return
      end
