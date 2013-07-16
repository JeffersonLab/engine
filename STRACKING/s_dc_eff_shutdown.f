      SUBROUTINE S_DC_EFF_SHUTDOWN(lunout,ABORT,errmsg)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Analyze and report drift chamber efficiencies.
*-
*-      Required Input BANKS     SOS_STATISTICS
*-                               GEN_DATA_STRUCTURES
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
* author: John Arrington
* created: 2/15/95
*
* s_dc_eff calculates efficiencies for the hodoscope.
* s_dc_eff_shutdown does some final manipulation of the numbers.
*
* $Log: s_dc_eff_shutdown.f,v $
* Revision 1.2  1996/09/05 13:29:49  saw
* (JRA) Cosmetic
*
* Revision 1.1  1995/08/31 15:07:37  cdaq
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
*
      character*17 here
      parameter (here= 'S_DC_EFF_SHUTDOWN')
*
      logical ABORT
      character*(*) errmsg
*
      INCLUDE 'sos_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      include 'sos_statistics.cmn'
      include 'sos_tracking.cmn'

      logical written_header

      integer*4 lunout
      integer*4 ind
      real*4 num         ! real version of #/events (aviod repeated floats)
      save

      written_header = .false.

      num = float(max(1,sdc_tot_events))
      do ind = 1 , sdc_num_planes
        sdc_plane_eff(ind) = float(sdc_events(ind))/num
        if (sdc_plane_eff(ind) .le. sdc_min_eff(ind) .and. num.ge.1000) then
          if (.not.written_header) then
            write(lunout,*)
            write(lunout,'(a,f6.3)') ' SOS DC planes with low raw hit (hits/trig)efficiencies'
            written_header = .true.
          endif
          write(lunout,'(5x,a,i2,a,f5.3,a,f5.3)') 'eff. for plane #',ind,' is ',
     &       sdc_plane_eff(ind),',   warning level is ',sdc_min_eff(ind)
        endif
      enddo

      do ind = 1 , sdc_num_chambers
        sdc_cham_eff(ind) = float(sdc_cham_hits(ind))/num
      enddo

      return
      end
