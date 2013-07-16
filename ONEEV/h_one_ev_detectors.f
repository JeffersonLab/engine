      subroutine h_one_ev_detectors
*
* Define geant detector sets
*
* August, 1994, Pat Welch, Oregon State University, tpw@physics.orst.edu
*
* $Log: h_one_ev_detectors.f,v $
* Revision 1.2  1995/09/18 14:38:33  cdaq
* (SAW) Remove unneeded declartions
*
* Revision 1.1  1995/03/14  21:26:57  cdaq
* Initial revision
*

      call h_one_ev_hodo
      call h_one_ev_cal
      call h_one_ev_wc

      return
      end

