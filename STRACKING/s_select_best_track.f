      SUBROUTINE S_SELECT_BEST_TRACK(ABORT,err)
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
*- Revision 1.3  1995/05/22 19:45:55  cdaq
*- (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*-
c Revision 1.2  1995/04/06  19:44:04  cdaq
c (JRA) Fix some latent HMS variable names
c
c Revision 1.1  1995/02/23  13:29:49  cdaq
c Initial revision
c
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*50 here
      parameter (here= 'S_SELECT_BEST_TRACK')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'sos_data_structures.cmn'
      INCLUDE 'gen_routines.dec'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      INCLUDE 'sos_physics_sing.cmn'
      INCLUDE 'sos_calorimeter.cmn'
      INCLUDE 'sos_scin_parms.cmn'
      INCLUDE 'sos_scin_tof.cmn'
*
*     local variables 
      integer*4 goodtrack,track
      real*4 chi2perdeg,chi2min
*--------------------------------------------------------
*
      ABORT= .FALSE.
      err= ' '
*     Need to test to chose the best track
      SSNUM_FPTRACK = 0
      SSNUM_TARTRACK = 0
      if( SNTRACKS_FP.GT. 0) then
        chi2min= 1e10
        goodtrack = 0
        do track = 1, SNTRACKS_FP

          if( SNFREE_FP(track).ge. ssel_ndegreesmin) then
            chi2perdeg = SCHI2_FP(track)/FLOAT(SNFREE_FP(track))
            if(chi2perdeg .lt. chi2min) then
*     simple particle id tests
              if( ( SDEDX(track,1) .gt. ssel_dedx1min)  .and.
     &             ( SDEDX(track,1) .lt. ssel_dedx1max)  .and.
     &             ( SBETA(track)   .gt. ssel_betamin)   .and.
     &             ( SBETA(track)   .lt. ssel_betamax)   .and.
     &             ( STRACK_ET(track) .gt. ssel_etmin)   .and.
     &             ( STRACK_ET(track) .lt. ssel_etmax)) then
                goodtrack = track
              endif                     ! end test on track id
            endif                       ! end test on lower chisq
          endif                         ! end test on minimum number of degrees of freedom
        enddo                           ! end loop on track
        SSNUM_TARTRACK = goodtrack
        SSNUM_FPTRACK  = goodtrack
        if(goodtrack.eq.0) return       ! return if no valid tracks
      endif

      return
      end
