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
* $Log$
* Revision 1.1  1995/02/23 13:32:00  cdaq
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
      include 'hms_calorimeter.cmn'
      include 'hms_statistics.cmn'

      integer col,row
      save

! fill sums over counters
      do col=1,hmax_cal_columns
        hstat_cal_trksum(col)=0
        hstat_cal_hitsum(col)=0
        do row=1,hmax_cal_rows
          hstat_cal_trksum(col)=hstat_cal_trksum(col)+hstat_cal_trk(col,row)
          hstat_cal_hitsum(col)=hstat_cal_hitsum(col)+hstat_cal_hit(col,row)
ccc      write(6,*) row,col,hstat_cal_trksum(col)
        enddo
        hstat_cal_eff(col)=hstat_cal_hitsum(col)/max(.01,float(hstat_cal_trksum(col)))
ccc      write(6,*) hstat_cal_eff(col) 
      enddo

      return
      end
