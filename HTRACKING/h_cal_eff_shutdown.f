      SUBROUTINE H_CAL_EFF_SHUTDOWN(ABORT,errmsg)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Summary of calorimeter efficiencies.
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
* h_cal_eff calculates efficiencies for the calorimeter.
* h_cal_eff_shutdown does some final manipulation of the numbers.
*
* $Log: h_cal_eff_shutdown.f,v $
* Revision 1.5  1999/02/23 18:35:24  csa
* (JRA) Remove hdebugcalcpeds stuff
*
* Revision 1.4  1995/10/09 20:09:37  cdaq
* (JRA) Add bypass switch around writing of pedestal data
*
* Revision 1.3  1995/08/31 14:57:36  cdaq
* (JRA) Calculate and printout pedestals
*
* Revision 1.2  1995/05/22  19:39:05  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.1  1995/02/23  13:32:00  cdaq
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
*
      character*18 here
      parameter (here= 'H_CAL_EFF_SHUTDOWN')
*
      logical ABORT
      character*(*) errmsg
*
      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      include 'hms_calorimeter.cmn'
      include 'hms_statistics.cmn'
      include 'hms_tracking.cmn'

      integer col,row,blk
      real ave,ave2,num
      save

! fill sums over counters
      do col=1,hmax_cal_columns
        hstat_cal_trksum(col)=0
        hstat_cal_hitsum(col)=0
        do row=1,hmax_cal_rows
          hstat_cal_eff(col,row)=hstat_cal_hit(col,row)/max(.01,float(hstat_cal_trk(col,row)))
          hstat_cal_trksum(col)=hstat_cal_trksum(col)+hstat_cal_trk(col,row)
          hstat_cal_hitsum(col)=hstat_cal_hitsum(col)+hstat_cal_hit(col,row)
        enddo
        hstat_cal_effsum(col)=hstat_cal_hitsum(col)/max(.01,float(hstat_cal_trksum(col)))
      enddo

      do blk=1,hmax_cal_blocks
        num=float(max(1,hcal_zero_num(blk)))
        ave=float(hcal_zero_sum(blk))/num
        ave2=float(hcal_zero_sum2(blk))/num
        hcal_zero_ave(blk)=ave
        hcal_zero_sig(blk)=sqrt(max(0.,ave2-ave*ave))
        hcal_zero_thresh(blk)=min(50.,max(20.,3*hcal_zero_sig(blk)))
      enddo

      return
      end
