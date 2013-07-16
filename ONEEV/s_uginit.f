      subroutine s_uginit
*
* Do the GEANT initalization
*
* August, 1994, Pat Welch, Oregon State University, tpw@physics.orst.edu
*
* Modified from HMS version, h_uginit, March 1995 by
* Derek van Westrum (vanwestr@cebaf.gov)
*
* $Log: s_uginit.f,v $
* Revision 1.1  1995/07/31 15:15:28  cdaq
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
*      call gsrotm(3,90.,15.,90.,105.,0.,0.)
      call gsrotm(3,90.,-60.,90.,-150.,0.,0.)
      call gsrotm(4,90.,60.,90.,150.,0.,0.)
*      call gsrotm(3,90.,-30.,90.,-120.,0.,0.)
*      call gsrotm(4,90.,30.,90.,120.,0.,0.)

      call s_one_ev_geometry            ! define the geometry
      call s_one_ev_detectors           ! define sensitive detectors
      call ggclos                       ! close geometry structures

      call gphysi                       ! init physics interaction vars

      end
