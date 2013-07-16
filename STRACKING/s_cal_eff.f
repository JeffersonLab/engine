      SUBROUTINE S_CAL_EFF(ABORT,errmsg)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Analyze calorimeter statistics for each track 
*-
*-      Required Input BANKS     SOS_CALORIMETER
*-                               GEN_DATA_STRUCTURES
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
* author: John Arrington
* created: 2/17/95
*
* s_cal_eff calculates efficiencies for the hodoscope.
*
* $Log: s_cal_eff.f,v $
* Revision 1.7  2002/07/31 20:20:57  saw
* Only try to fill user hists that are defined
*
* Revision 1.6  1999/01/29 17:34:56  saw
* Add variables for second tubes on shower counter
*
* Revision 1.5  1996/09/04 20:17:05  saw
* (JRA) Require more than one photoelectron
*
* Revision 1.4  1995/08/31 15:06:45  cdaq
* (JRA) Fill dpos (pos. track - pos. hit) histograms
*
* Revision 1.3  1995/05/22  19:45:31  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.2  1995/04/01  20:39:32  cdaq
* (SAW) Fix typos
*
* Revision 1.1  1995/02/23  15:42:27  cdaq
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
*
      character*9 here
      parameter (here= 'S_CAL_EFF')
*
      logical ABORT
      character*(*) errmsg
*
      INCLUDE 'sos_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      include 'sos_calorimeter.cmn'
      include 'sos_statistics.cmn'
      include 'sos_id_histid.cmn'

      integer col,row,blk
      integer hit_row(smax_cal_columns)
      integer nhit
      real    adc_pos, adc_neg
      real    hit_pos(smax_cal_columns),hit_dist(smax_cal_columns)
      real    histval
      save

* find counters on track, and distance from center.

      if (sschi2perdeg.le.sstat_cal_maxchisq .and. scer_npe_sum.ge.1.)
     &       sstat_cal_numevents=sstat_cal_numevents+1

      hit_pos(1)=ssx_fp + ssxp_fp*(scal_1pr_zpos+0.5*scal_1pr_thick)
      hit_row(1)=nint((hit_pos(1)-scal_block_xc(1))
     &          /scal_block_xsize)+1
      hit_row(1)=max(min(hit_row(1),smax_cal_rows),1)
      hit_dist(1)=hit_pos(1)-(scal_block_xsize*(hit_row(1)-1)
     &           +scal_block_xc(1))

      hit_pos(2)=ssx_fp + ssxp_fp*(scal_2ta_zpos+0.5*scal_2ta_thick)
      hit_row(2)=nint((hit_pos(2)-scal_block_xc(smax_cal_rows+1))
     &          /scal_block_xsize)+1
      hit_row(2)=max(min(hit_row(2),smax_cal_rows),1)
      hit_dist(2)=hit_pos(2)-(scal_block_xsize*(hit_row(2)-1)
     &           +scal_block_xc(smax_cal_rows+1))

      hit_pos(3)=ssx_fp + ssxp_fp*(scal_3ta_zpos+0.5*scal_3ta_thick)
      hit_row(3)=nint((hit_pos(3)-scal_block_xc(2*smax_cal_rows+1))
     &          /scal_block_xsize)+1
      hit_row(3)=max(min(hit_row(3),smax_cal_rows),1)
      hit_dist(3)=hit_pos(3)-(scal_block_xsize*(hit_row(3)-1)
     &           +scal_block_xc(2*smax_cal_rows+1))

      hit_pos(4)=ssx_fp + ssxp_fp*(scal_4ta_zpos+0.5*scal_4ta_thick)
      hit_row(4)=nint((hit_pos(4)-scal_block_xc(3*smax_cal_rows+1))
     &          /scal_block_xsize)+1
      hit_row(4)=max(min(hit_row(4),smax_cal_rows),1)
      hit_dist(4)=hit_pos(3)-(scal_block_xsize*(hit_row(4)-1)
     &           +scal_block_xc(3*smax_cal_rows+1))

*   increment 'should have hit' counters
      do col=1,smax_cal_columns
        if(abs(hit_dist(col)).le.sstat_cal_slop .and.    !hit in middle of blk.
     &           sschi2perdeg.le.sstat_cal_maxchisq .and. scer_npe_sum.ge.1.) then
          sstat_cal_trk(col,hit_row(col))=sstat_cal_trk(col,hit_row(col))+1
        endif
      enddo

      do nhit=1,scal_num_hits
        row=scal_rows(nhit)
        col=scal_cols(nhit)
* We don't actually do anything with the following values?
        adc_pos=scal_adcs_pos(nhit)
        adc_neg=scal_adcs_neg(nhit)
        blk=row+smax_cal_rows*(col-1)

* fill the dpos histograms.
        if (col .eq. 1) then
          histval=(scal_block_xc(1)+scal_block_xsize*(row-1))-hit_pos(1)
          if (sidcaldpos.gt.0)
     $         call hf1(sidcaldpos,histval,1.)
        endif

*  Record the hits if track is near center of block and the chisquared of the 
*  track is good
        if(abs(hit_dist(col)).le.sstat_cal_slop .and. row.eq.hit_row(col)) then
          if (sschi2perdeg.le.sstat_cal_maxchisq .and. scer_npe_sum.ge.1.) then
            sstat_cal_hit(col,hit_row(col))=sstat_cal_hit(col,hit_row(col))+1
          endif     !was it a good track.
        endif     !if hit was on track.
      enddo

      return
      end
