      SUBROUTINE S_CAL_EFF_SHUTDOWN(ABORT,errmsg)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Summary of calorimeter efficiencies.
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
* s_cal_eff calculates efficiencies for the calorimeter.
* s_cal_eff_shutdown does some final manipulation of the numbers.
*
* $Log: s_cal_eff_shutdown.f,v $
* Revision 1.5  1999/02/23 18:55:22  csa
* (JRA) Remove sdebugcalcpeds stuff
*
* Revision 1.4  1995/10/09 20:09:10  cdaq
* (JRA) Add bypass switch around writing of pedestal data
*
* Revision 1.3  1995/08/31 18:04:19  cdaq
* (JRA) Calculate and printout pedestals
*
* Revision 1.2  1995/05/22  19:45:32  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.1  1995/02/23  15:42:42  cdaq
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
*
      character*18 here
      parameter (here= 'S_CAL_EFF_SHUTDOWN')
*
      logical ABORT
      character*(*) errmsg
*
      INCLUDE 'sos_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      include 'sos_calorimeter.cmn'
      include 'sos_statistics.cmn'
      include 'sos_tracking.cmn'

      integer col,row,blk
      real ave,ave2,num
      save

! fill sums over counters
      do col=1,smax_cal_columns
        sstat_cal_trksum(col)=0
        sstat_cal_hitsum(col)=0
        do row=1,smax_cal_rows
          sstat_cal_eff(col,row)=sstat_cal_hit(col,row)
     $         /max(.01,float(sstat_cal_trk(col,row))) 
          sstat_cal_trksum(col)=sstat_cal_trksum(col)+sstat_cal_trk(col,row)
          sstat_cal_hitsum(col)=sstat_cal_hitsum(col)+sstat_cal_hit(col,row)
        enddo
        sstat_cal_effsum(col)=sstat_cal_hitsum(col)
     $       /max(.01,float(sstat_cal_trksum(col)))
      enddo

      do blk=1,smax_cal_blocks
        num=float(max(1,scal_zero_num(blk)))
        ave=float(scal_zero_sum(blk))/num
        ave2=float(scal_zero_sum2(blk))/num
        scal_zero_ave(blk)=ave
        scal_zero_sig(blk)=sqrt(max(0.,ave2-ave*ave))
        scal_zero_thresh(blk)=min(50.,max(20.,3*scal_zero_sig(blk)))
      enddo

      return
      end
