      SUBROUTINE H_DC_EFF_SHUTDOWN(lunout,ABORT,errmsg)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Analyze and report drift chamber efficiencies.
*-
*-      Required Input BANKS     HMS_STATISTICS
*-                               GEN_DATA_STRUCTURES
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
* author: John Arrington
* created: 2/15/95
*
* h_dc_eff calculates efficiencies for the hodoscope.
* h_dc_eff_shutdown does some final manipulation of the numbers.
*
* $Log$
* Revision 1.2  1996/08/30 19:54:11  saw
* (JRA) Cosmetic
*
* Revision 1.1  1995/08/31 14:59:56  cdaq
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
*
      character*17 here
      parameter (here= 'H_DC_EFF_SHUTDOWN')
*
      logical ABORT
      character*(*) errmsg
*
      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      include 'hms_statistics.cmn'
      include 'hms_tracking.cmn'

      logical written_header

      integer*4 lunout
      integer*4 ind
      real*4 num         ! real version of #/events (aviod repeated floats)
      save

      written_header = .false.

      num = float(max(1,hdc_tot_events))
      do ind = 1 , hdc_num_planes
        hdc_plane_eff(ind) = float(hdc_events(ind))/num
        if (hdc_plane_eff(ind) .le. hdc_min_eff(ind) .and. num.ge.1000) then
          if (.not.written_header) then
            write(lunout,*)
            write(lunout,'(a,f6.3)') ' HMS DC planes with low raw hit (hits/trig) efficiencies'
            written_header = .true.
          endif
          write(lunout,'(5x,a,i2,a,f5.3,a,f5.3)') 'eff. for plane #',ind,' is ',
     &       hdc_plane_eff(ind),',   warning level is ',hdc_min_eff(ind)
        endif
      enddo

      do ind = 1 , hdc_num_chambers
        hdc_cham_eff(ind) = float(hdc_cham_hits(ind))/num
      enddo

      return
      end
