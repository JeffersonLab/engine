      subroutine s_one_ev_detectors
*
* Define geant detector sets
*
* August, 1994, Pat Welch, Oregon State University, tpw@physics.orst.edu
*
* Modified from HMS version, h_one_ev_detectors, March 1995 by
* Derek van Westrum (vanwestr@cebaf.gov)
*
* $Log: s_one_ev_detectors.f,v $
* Revision 1.2  1995/09/18 14:38:09  cdaq
* (SAW) Remove unneeded declartions
*
* Revision 1.1  1995/07/31 15:23:05  cdaq
* Initial revision
*

      call s_one_ev_hodo
      call s_one_ev_cal
      call s_one_ev_wc

      end
