      SUBROUTINE S_CER_EFF(ABORT,errmsg)

*--------------------------------------------------------
*
*  Purpose and Methods : Analyze cerenkov information for the "best
*                        track" as selected in s_select_best_track
*  Required Input BANKS: sos_cer_parms
*                        SOS_DATA_STRUCTURES
* 
*                Output: ABORT           - success or failure
*                      : err             - reason for failure, if any
*
*
* author: Chris Cothran
* created: 5/25/95
* $Log$
* Revision 1.2  1995/10/09 20:14:50  cdaq
* (JRA) Move calculation of hit position on mirror to s_physics
*
* Revision 1.1  1995/08/31 15:04:48  cdaq
* Initial revision
*
*--------------------------------------------------------

      IMPLICIT NONE
*
      character*9 here
      parameter (here= 'S_CER_EFF')
*
      logical ABORT
      character*(*) errmsg
*
      include 'sos_data_structures.cmn'
      include 'sos_cer_parms.cmn'
      include 'sos_physics_sing.cmn'

      integer*4 nr
*
* test for a good electron
*
      if (sntracks_fp .eq. 1
     &  .and. sschi2perdeg .gt. 0. 
     &  .and. sschi2perdeg .lt. scer_chi2max
     &  .and. ssbeta .gt. scer_beta_min
     &  .and. ssbeta .lt. scer_beta_max
     &  .and. sstrack_et .gt. scer_et_min
     &  .and. sstrack_et .lt. scer_et_max) then

        do nr = 1, scer_num_regions
*
* hit must be inside the region in order to continue
*
          if (abs(scer_region(nr,1)-ssx_cer).lt.scer_region(nr,5)
     >  .and. abs(scer_region(nr,2)-ssy_cer).lt.scer_region(nr,6)
     >  .and. abs(scer_region(nr,3)-ssxp_fp).lt.scer_region(nr,7)
     >  .and. abs(scer_region(nr,4)-ssyp_fp).lt.scer_region(nr,8))
     >    then
*
* increment the 'should have fired' counters
*
            scer_track_counter(nr) = scer_track_counter(nr) + 1
*
* increment the 'did fire' counters
*
            if (SCER_NPE_SUM.gt.scer_threshold) then
              scer_fired_counter(nr) = scer_fired_counter(nr) + 1
            endif
          endif
        enddo
      endif

      return
      end
