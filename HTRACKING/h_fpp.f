      SUBROUTINE h_fpp(ABORT,err)
*--------------------------------------------------------
*    Hall C  HMS Focal Plane Polarimeter Code
*
*  Purpose: analyze FPP portion of HMS event
* 
*  Created by Frank R. Wesselmann,  February 2004
*
*--------------------------------------------------------

      IMPLICIT NONE

      include 'gen_detectorids.par'
      include 'gen_decode_common.cmn'
      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'hms_fpp_params.cmn'
      INCLUDE 'hms_fpp_event.cmn'

      character*13 here
      parameter (here= 'h_fpp')

      logical ABORT
      character*(*) err

      integer*4 iset


      ABORT= .FALSE.
      err= ' '

c      write(*,*)'In h_fpp.f with hsnum_fptrack =',hsnum_fptrack
      if (hsnum_fptrack.le.0) return    ! No good HMS track
*     * note that the above value is determined in h_select_best_track
*     * so we have to wait until after it is called before we do the FPP!


*     * do tracking in each set of chambers separately
      do iset=1, H_FPP_N_DCSETS
c        write(*,*)'Calling fpp_tracking ->'
c        write(*,*)'iset,layers,min = ',iset,HFPP_Nlayershit_set(iset),
c     &   HFPP_minsethits
        if (HFPP_Nlayershit_set(iset) .ge. HFPP_minsethits) then
          call h_fpp_tracking(iset,ABORT,err)
          if (ABORT) then
            call g_add_path(here,err)
            return
          endif

          
       endif
      enddo !iset

c     if tracks were found and flag is set, do best track selection
c     in each FPP for the gep ntuple. Loop over filled track arrays 
c     and set the variable hfpp_best_track(fpp1/2)
      if(hselectfpptrackprune.ne.0.and.(hfpp_n_tracks(1).gt.0.or.
     $     hfpp_n_tracks(2).gt.0) ) then 
c     choose best FPP track using prune tests and Sitnik's method for selection
c     using a combination of chi2 and sclose:
         call h_fpp_select_best_track_prune(abort,err)
         if (ABORT) then
            call g_add_path(here,err)
            return
         endif
      endif

*     * do statistical analysis, e.g. efficiencies
      call h_fpp_statistics(ABORT,err)
      if (ABORT) then
        call g_add_path(here,err)
        return
      endif


      RETURN
      END
