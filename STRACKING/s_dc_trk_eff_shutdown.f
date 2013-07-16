      SUBROUTINE S_DC_TRK_EFF_SHUTDOWN(lunout,ABORT,errmsg)
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
* created: 9/5/95
*
* s_dc_trk_eff calculates efficiencies for the chambers (using tracking)
* s_dc_trk_eff_shutdown does some final manipulation of the numbers.
*
* $Log: s_dc_trk_eff_shutdown.f,v $
* Revision 1.1  1995/10/09 20:05:32  cdaq
* Initial revision
*
*
*--------------------------------------------------------
      IMPLICIT NONE
*
      character*21 here
      parameter (here= 'S_DC_TRK_EFF_SHUTDOWN')
*
      logical ABORT
      character*(*) errmsg
*
      INCLUDE 'sos_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      include 'sos_tracking.cmn'
      include 'sos_geometry.cmn'

      logical written_header

      integer*4 lunout
      integer*4 pln,wire
      real*4 wireeff,planeeff
      real*4 num         ! real version of #/events (aviod repeated floats)
      save

      written_header = .false.

      do pln = 1 , sdc_num_planes
        sdc_didsum(pln)=0
        sdc_shouldsum(pln)=0
        do wire = 1 , sdc_nrwire(pln)
          sdc_shouldsum(pln) = sdc_shouldsum(pln) + 1
          sdc_didsum(pln) = sdc_didsum(pln) + 1
          num = float(max(1,sdc_shouldhit(pln,wire)))
          wireeff = float(sdc_didhit(pln,wire)) / num
          if (num.gt.50 .and. wireeff.lt.sdc_min_wire_eff) then
            write(lunout,111) '       SOS pln=',pln,',  wire=',wire,
     &         ',  effic=',wireeff,' = ',sdc_didhit(pln,wire),'/',
     &         sdc_shouldhit(pln,wire)

          endif
        enddo
      enddo
111   format (a,i3,a,i4,a,f4.2,a,i6,a,i6)

      do pln = 1 , sdc_num_planes
        planeeff=float(sdc_didsum(pln))/float(max(1,sdc_shouldsum(pln)))
        if   (sdc_shouldsum(pln).gt.1000 .and. 
     &        planeeff.gt.sdc_min_plane_eff(pln)) then
          write(lunout,112) 'ave. effic for plane',pln,' is ',
     &        planeeff,' = ',sdc_didsum(pln),'/',sdc_shouldsum(pln)
        endif
      enddo
112   format (a,i3,a,f4.2,a,i7,a,i7)

      return
      end
