      subroutine h_uginit
*
* Do the GEANT initalization
*
* August, 1994, Pat Welch, Oregon State University, tpw@physics.orst.edu
*
* $Log: h_uginit.f,v $
* Revision 1.1  1995/03/14 21:27:13  cdaq
* Initial revision
*

      implicit none

      call ginit                        ! init GEANT
      call gzinit                       ! init GEANT data structs
      call gdinit                       ! init GEANT drawing package

      call gpart                        ! init particle structures
      call gmate                        ! init materials structures
      call gsrotm(1,90.,0.,90.,90.,0.,0.)
      call gsrotm(2,90.,90.,90.,0.,0.,0.)
      call gsrotm(3,90.,15.,90.,105.,0.,0.)
      call gsrotm(4,90.,345.,90.,75.,0.,0.)

      call h_one_ev_geometry            ! define the geometry
      call h_one_ev_detectors           ! define sensitive detectors
      call ggclos                       ! close geometry structures

      call gphysi                       ! init physics interaction vars

      end
