      SUBROUTINE H_SELECT_BEST_TRACK(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Select the best track through the HMS
*-                              
*-
*-      Required Input BANKS
*-
*-      Output BANKS
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
*- $Log$
*- Revision 1.1  1995/01/31 21:33:54  cdaq
*- Initial revision
*-
*-
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*50 here
      parameter (here= 'H_SELECT_BEST_TRACK')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'gen_data_structures.cmn'
      INCLUDE 'gen_routines.dec'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      INCLUDE 'hms_physics_sing.cmn'
      INCLUDE 'mc_structures.cmn'
      INCLUDE 'hms_calorimeter.cmn'
      INCLUDE 'hms_scin_tof.cmn'
      INCLUDE 'hms_scin_parms.cmn'
*
*     local variables 
      integer*4 goodtrack,track
      real*4 chi2perdeg,chi2min
*--------------------------------------------------------
*
      ABORT= .FALSE.
      err= ' '
*     Need to test to chose the best track
      HSNUM_FPTRACK = 0
      HSNUM_TARTRACK = 0
      if( HNTRACKS_FP.GT. 0) then
        chi2min= 1e10
        goodtrack = 0
        do track = 1, HNTRACKS_FP

          if( HNFREE_FP(track).ge. hsel_ndegreesmin) then
            chi2perdeg = HCHI2_FP(track)/FLOAT(HNFREE_FP(track))
            if(chi2perdeg .lt. chi2min) then
*     simple particle id tests
              if(  ( HDEDX(track,1) .gt. hsel_dedx1min)  .and.
     &             ( HDEDX(track,1) .lt. hsel_dedx1max)  .and.
     &             ( HBETA(track)   .gt. hsel_betamin)   .and.
     &             ( HBETA(track)   .lt. hsel_betamax)   .and.
     &             ( HTRACK_ET(track) .gt. hsel_etmin)   .and.
     &             ( HTRACK_ET(track) .lt. hsel_etmax)) then
                goodtrack = track
              endif                     ! end test on track id
            endif                       ! end test on lower chisq
          endif                         ! end test on minimum number of degrees of freedom
        enddo                           ! end loop on track
        HSNUM_TARTRACK = goodtrack
        HSNUM_FPTRACK  = goodtrack
        if(goodtrack.eq.0) return       ! return if no valid tracks
      endif

      return
      end
