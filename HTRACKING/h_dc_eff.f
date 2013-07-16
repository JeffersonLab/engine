      SUBROUTINE H_DC_EFF(ABORT,errmsg)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Analyze scintillator information for each track 
*-
*-      Required Input BANKS     HMS_STATISTICS
*-                               GEN_DATA_STRUCTURES
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
* author: John Arrington
* created: 8/17/95
*
* h_dc_eff calculates efficiencies for the drift chambers.
*
* $Log: h_dc_eff.f,v $
* Revision 1.1  1995/08/31 14:59:48  cdaq
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
*
      character*8 here
      parameter (here= 'H_DC_EFF')
*
      logical ABORT
      character*(*) errmsg
*
      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      include 'hms_statistics.cmn'
      include 'hms_tracking.cmn'

      integer*4 ind

      save

      hdc_tot_events = hdc_tot_events + 1
      do ind = 1 , hdc_num_planes
        if (hdc_hits_per_plane(ind).gt.0) hdc_events(ind)=hdc_events(ind)+1
      enddo

      if (hdc_hits_per_plane(1)+hdc_hits_per_plane(2)+hdc_hits_per_plane(3)
     &   +hdc_hits_per_plane(4)+hdc_hits_per_plane(5)+hdc_hits_per_plane(6)
     &    .ne. 0)   hdc_cham_hits(1) = hdc_cham_hits(1) + 1

      if (hdc_hits_per_plane( 7)+hdc_hits_per_plane( 8)+hdc_hits_per_plane( 9)
     &   +hdc_hits_per_plane(10)+hdc_hits_per_plane(11)+hdc_hits_per_plane(12)
     &    .ne. 0)   hdc_cham_hits(2) = hdc_cham_hits(2) + 1

      return
      end
