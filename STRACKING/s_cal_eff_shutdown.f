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
* $Log$
* Revision 1.1  1995/02/23 15:42:42  cdaq
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
      include 'sos_calorimeter.cmn'
      include 'sos_statistics.cmn'

      integer col,row
      save

! fill sums over counters
      do col=1,smax_cal_columns
        sstat_cal_trksum(col)=0
        sstat_cal_hitsum(col)=0
        do row=1,smax_cal_rows
          sstat_cal_trksum(col)=sstat_cal_trksum(col)+sstat_cal_trk(col,row)
          sstat_cal_hitsum(col)=sstat_cal_hitsum(col)+sstat_cal_hit(col,row)
ccc      write(6,*) row,col,sstat_cal_trksum(col)
        enddo
        sstat_cal_eff(col)=sstat_cal_hitsum(col)/max(.01,float(sstat_cal_trksum(col)))
ccc      write(6,*) sstat_cal_eff(col) 
      enddo

      return
      end
