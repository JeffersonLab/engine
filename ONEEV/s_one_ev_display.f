      subroutine s_one_ev_display(iview)
*
* This routine will store digitized hits for use in the one event display for
* Hall C
*
* August, 1994, Pat Welch, Oregon State University, tpw@physics.orst.edu
*
* Modified from HMS version, h_one_ev_display, on March 1995 by
* Derek van Westrum (vanwestr@cebaf.gov)
*
* $Log: s_one_ev_display.f,v $
* Revision 1.3  1996/01/17 16:31:33  cdaq
* (DVW) Add iview argument, make improvements.
*
* Revision 1.2  1995/09/18 14:43:20  cdaq
* (DVW) Improvements
*
* Revision 1.1  1995/07/31 15:25:20  cdaq
* Initial revision
*

      implicit none

      integer iview

      call ixclrwi
      if (iview.le.1) then
        call s_one_ev_persp_view        !draw the perspective view
      elseif (iview.eq.2) then
        call s_one_ev_topside_view      !draw the two side views
      elseif (iview.ge.3) then
        call s_one_ev_head_view         !draw the head on view
      endif

      end
