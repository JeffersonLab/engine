      subroutine h_one_ev_display(iview)
*
* This routine will store digitized hits for use in the one event display for
* Hall C
*
* August, 1994, Pat Welch, Oregon State University, tpw@physics.orst.edu
* Modified by Derek van Westrum (vanwestr@cebaf.gov) Jul 1995
* $Log: h_one_ev_display.f,v $
* Revision 1.5  1996/01/17 16:31:18  cdaq
* (DVW) Add iview argument, make improvements.
*
* Revision 1.4  1995/09/18 14:43:05  cdaq
* (DVW) Improvements
*
* Revision 1.3  1995/09/14 15:18:55  cdaq
* (??) Updates
*
* Revision 1.2  1995/05/22  18:59:09  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts
*
* Revision 1.1  1995/03/14  21:26:49  cdaq
* Initial revision
*
      implicit none

      integer iview

      call ixclrwi
      if (iview.le.1) then
        call h_one_ev_persp_view        !draw the perspective view
      elseif (iview.eq.2) then
        call h_one_ev_topside_view      !draw the two side views
      elseif (iview.ge.3) then
        call h_one_ev_head_view         !draw the head on view
      endif

      end

