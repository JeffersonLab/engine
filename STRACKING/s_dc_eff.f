      SUBROUTINE S_DC_EFF(ABORT,errmsg)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Analyze scintillator information for each track 
*-
*-      Required Input BANKS     SOS_STATISTICS
*-                               GEN_DATA_STRUCTURES
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
* author: John Arrington
* created: 8/17/95
*
* s_dc_eff calculates efficiencies for the drift chambers.
*
* $Log: s_dc_eff.f,v $
* Revision 1.1  1995/08/31 15:07:28  cdaq
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
*
      character*8 here
      parameter (here= 'S_DC_EFF')
*
      logical ABORT
      character*(*) errmsg
*
      INCLUDE 'sos_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      include 'sos_statistics.cmn'
      include 'sos_tracking.cmn'

      integer*4 ind

      save

      sdc_tot_events = sdc_tot_events + 1
      do ind = 1 , sdc_num_planes
        if (sdc_hits_per_plane(ind).gt.0) sdc_events(ind)=sdc_events(ind)+1
      enddo

      if (sdc_hits_per_plane(1)+sdc_hits_per_plane(2)+sdc_hits_per_plane(3)
     &   +sdc_hits_per_plane(4)+sdc_hits_per_plane(5)+sdc_hits_per_plane(6)
     &    .ne. 0)   sdc_cham_hits(1) = sdc_cham_hits(1) + 1

      if (sdc_hits_per_plane( 7)+sdc_hits_per_plane( 8)+sdc_hits_per_plane( 9)
     &   +sdc_hits_per_plane(10)+sdc_hits_per_plane(11)+sdc_hits_per_plane(12)
     &    .ne. 0)   sdc_cham_hits(2) = sdc_cham_hits(2) + 1

      return
      end
