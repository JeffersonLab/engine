      subroutine s_one_ev_detectors
*
* Define geant detector sets
*
* August, 1994, Pat Welch, Oregon State University, tpw@physics.orst.edu
*
* Modified from HMS version, h_one_ev_detectors, March 1995 by
* Derek van Westrum (vanwestr@cebaf.gov)
*
* $Log$
* Revision 1.1  1995/07/31 15:23:05  cdaq
* Initial revision
*

      implicit none

      include 'sos_one_ev.par'
      include 's_one_ev_hodo.inc'
      include 's_one_ev_cal.inc'
      include 's_one_ev_wc.inc'

      integer iset, idet
      character*4 varinames(3) /'x', 'y', 'z'/
      integer     varibits(3)  /32, 32, 32/
      real origin(3) /SHUT_HEIGHT, SHUT_HEIGHT, SHUT_HEIGHT/
      real factor(3) /1e3, 1e3, 1e3/

      call s_one_ev_hodo
      call s_one_ev_cal
      call s_one_ev_wc

      end
