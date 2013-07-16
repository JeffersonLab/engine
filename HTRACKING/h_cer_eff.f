      SUBROUTINE H_CER_EFF(ABORT,errmsg)

*--------------------------------------------------------
*
*  Purpose and Methods : Analyze cerenkov information for the "best
*                        track" as selected in h_select_best_track
*  Required Input BANKS: hms_cer_parms
*                        HMS_DATA_STRUCTURES
* 
*                Output: ABORT           - success or failure
*                      : err             - reason for failure, if any
*
*
* author: Chris Cothran
* created: 5/25/95
* $Log: h_cer_eff.f,v $
* Revision 1.4  1999/02/10 18:19:06  csa
* Changed hscer_et test to use momentum-normalized variable
*
* Revision 1.3  1999/02/03 21:13:23  saw
* Code for new Shower counter tubes
*
* Revision 1.2  1995/10/09 20:15:08  cdaq
* (JRA) Move calculation of hit position on mirror to s_physics
*
* Revision 1.1  1995/08/31 14:54:09  cdaq
* Initial revision
*
*--------------------------------------------------------

      IMPLICIT NONE
*
      character*9 here
      parameter (here= 'H_CER_EFF')
*
      logical ABORT
      character*(*) errmsg
*
      include 'hms_data_structures.cmn'
      include 'hms_cer_parms.cmn'
      include 'hms_physics_sing.cmn'
      include 'hms_calorimeter.cmn'

      integer*4 nr
*
*     test for a good electron. Use normalized, tracked shower counter
*     variable (hsshtrk).
*
      if (hntracks_fp .eq. 1
     &  .and. hschi2perdeg .gt. 0. 
     &  .and. hschi2perdeg .lt. hcer_chi2max
     &  .and. hsbeta .gt. hcer_beta_min
     &  .and. hsbeta .lt. hcer_beta_max
     &  .and. hsshtrk .gt. hcer_et_min
     &  .and. hsshtrk .lt. hcer_et_max) then

        do nr = 1, hcer_num_regions
*
*     hit must be inside the region in order to continue.
*
          if (abs(hcer_region(nr,1)-hsx_cer).lt.hcer_region(nr,5)
     >         .and. abs(hcer_region(nr,2)-hsy_cer).lt.hcer_region(nr,6)
     >         .and. abs(hcer_region(nr,3)-hsxp_fp).lt.hcer_region(nr,7)
     >         .and. abs(hcer_region(nr,4)-hsyp_fp).lt.hcer_region(nr,8))
     >         then
*
* increment the 'should have fired' counters
*
            hcer_track_counter(nr) = hcer_track_counter(nr) + 1
*
* increment the 'did fire' counters
*
            if (HCER_NPE_SUM.gt.hcer_threshold) then
              hcer_fired_counter(nr) = hcer_fired_counter(nr) + 1
            endif
          endif
        enddo
      endif

      return
      end
