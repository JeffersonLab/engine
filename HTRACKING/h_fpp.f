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


      if (hsnum_fptrack.le.0) return    ! No good HMS track
*     * note that the above value is determined in h_select_best_track
*     * so we have to wait until after it is called before we do the FPP!


*     * do tracking in each set of chambers separately
      do iset=1, H_FPP_N_DCSETS
        if (HFPP_Nlayershit_set(iset) .ge. HFPP_minsethits) then
c          write(*,*)'Calling fpp_tracking ->'
c          write(*,*)'iset,layers = ',iset,HFPP_Nlayershit_set(iset)
          call h_fpp_tracking(iset,ABORT,err)
          if (ABORT) then
            call g_add_path(here,err)
            return
          endif

        endif
      enddo !iset


*     * do statistical analysis, e.g. efficiencies
      call h_fpp_statistics(ABORT,err)
      if (ABORT) then
        call g_add_path(here,err)
        return
      endif


*     * fill our histogramms -- Ntuple is filled in non-FPP HMS code
      call h_fill_fpp(ABORT,err)
      if (ABORT) then
        call g_add_path(here,err)
        return
      endif


      RETURN
      END
