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
*- $Log: h_select_best_track.f,v $
*- Revision 1.5  2004/02/26 22:23:17  jones
*- Add if statement to use subroutine h_select_best_track_using_scin.f
*- when hsel_using_scin .eq. 1 . Otherwise picks the best track the old
*- way.
*-
*- Revision 1.4  1995/07/19 19:12:22  cdaq
*- (CC) Fix bug in best chisq finding
*-
* Revision 1.3  1995/05/22  19:39:27  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.2  1995/02/23  13:31:01  cdaq
* (JRA) Adjust include file ordering
*
* Revision 1.1  1995/01/31  21:33:54  cdaq
* Initial revision
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
      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'gen_routines.dec'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      INCLUDE 'hms_physics_sing.cmn'
      INCLUDE 'hms_calorimeter.cmn'
      INCLUDE 'hms_scin_parms.cmn'
      INCLUDE 'hms_scin_tof.cmn'
      INCLUDE 'hms_tracking.cmn'
c
*
*     local variables 
      integer*4 goodtrack,track,trk,savegood
      logical first
      real*4 chi2perdeg,chi2min
c
      integer*4 i,j
      data first /.true./
*--------------------------------------------------------
*
      ABORT= .FALSE.
      err= ' '
*     Need to test to chose the best track
      HSNUM_FPTRACK = 0
      HSNUM_TARTRACK = 0
        
c
      if ( hsel_using_prune.eq. 1) then
         if (first) write(*,*) ' HMS track selection using Pruning'
         first = .false.
         call H_SELECT_BEST_TRACK_PRUNE(ABORT,err)
         return
      endif

      if ( hsel_using_scin .eq. 1) then
         if (first) write(*,*) ' HMS track selection using scintillators'
         first = .false.
         call H_SELECT_BEST_TRACK_USING_SCIN(ABORT,err)
      else
c
      if( HNTRACKS_FP.GT. 0) then
         if (first) write(*,*) ' HMS track selection using chi-squared'
         first = .false.
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
                chi2min = chi2perdeg
              endif                     ! end test on track id
            endif                       ! end test on lower chisq
          endif                         ! end test on minimum number of degrees of freedom
        enddo                           ! end loop on track
        HSNUM_TARTRACK = goodtrack
        HSNUM_FPTRACK  = goodtrack
        if(goodtrack.eq.0) return       ! return if no valid tracks
      endif
c
      endif
c
      return
      end
