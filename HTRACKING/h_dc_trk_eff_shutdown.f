      SUBROUTINE H_DC_TRK_EFF_SHUTDOWN(lunout,ABORT,errmsg)
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
* created: 9/5/95
*
* h_dc_trk_eff calculates efficiencies for the chambers (using tracking)
* h_dc_trk_eff_shutdown does some final manipulation of the numbers.
*
* $Log: h_dc_trk_eff_shutdown.f,v $
* Revision 1.1  1995/10/09 20:04:23  cdaq
* Initial revision
*
*
*--------------------------------------------------------
      IMPLICIT NONE
*
      character*21 here
      parameter (here= 'H_DC_TRK_EFF_SHUTDOWN')
*
      logical ABORT
      character*(*) errmsg
*
      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      include 'hms_tracking.cmn'
      include 'hms_geometry.cmn'

      logical written_header

      logical was_already_dead
      integer*4 lunout
      integer*4 pln,wire
      integer*4 ind
      real*4 wireeff,planeeff
      real*4 num         ! real version of #/events (aviod repeated floats)
      save

      written_header = .false.

      do pln = 1 , hdc_num_planes
        hdc_didsum(pln)=0
        hdc_shouldsum(pln)=0
        do wire = 1 , hdc_nrwire(pln)
          hdc_shouldsum(pln) = hdc_shouldsum(pln) + 1
          hdc_didsum(pln) = hdc_didsum(pln) + 1
          num = float(max(1,hdc_shouldhit(pln,wire)))
          wireeff = float(hdc_didhit(pln,wire)) / num
          if (num.gt.50 .and. wireeff.lt.hdc_min_wire_eff) then
            was_already_dead = .false.
            do ind=1,hdc_num_deadwires
              if (pln  .eq. hdc_deadwire_plane(ind) .and.
     &            wire .eq. hdc_deadwire_num(ind)) was_already_dead=.true.
            enddo
            if (.not.was_already_dead) write(lunout,111) '       HMS pln=',pln,
     &         ',  wire=',wire,',  effic=',wireeff,' = ',hdc_didhit(pln,wire),
     &         '/',hdc_shouldhit(pln,wire)
          endif
        enddo
      enddo
111   format (a,i3,a,i4,a,f4.2,a,i6,a,i6)

      do pln = 1 , hdc_num_planes
        planeeff=float(hdc_didsum(pln))/float(max(1,hdc_shouldsum(pln)))
        if   (hdc_shouldsum(pln).gt.1000 .and. 
     &        planeeff.gt.hdc_min_plane_eff(pln)) then
          write(lunout,112) 'ave. effic for plane',pln,' is ',
     &        planeeff,' = ',hdc_didsum(pln),'/',hdc_shouldsum(pln)
        endif
      enddo
112   format (a,i3,a,f4.2,a,i7,a,i7)

      return
      end
