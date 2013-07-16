      subroutine s_init_scin(ABORT,err)

*-------------------------------------------------------------------
* author: John Arrington
* created: 2/22/94
*
* s_init_scin initializes the corrections and parameters
* for the scintillators.  Corrections are read from data files
* or the database.  Arrays used by the tof fitting routines
* are filled from the CTP variables input from the sos_positions
* parameter file.
*
* modifications:
*       23 March 1993   DFG
*            Remove /nolist from include statement. UNIX doesn't like it.
* $Log: s_init_scin.f,v $
* Revision 1.6  1996/04/30 17:32:20  saw
* (JRA) Calculate expected particle velocity
*
* Revision 1.5  1995/05/22 19:45:41  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.4  1995/02/23  13:36:31  cdaq
* * (JRA) Remove _coord fro shodo_center array.  Edge coordinates replaced by
* * center locations.
*
* Revision 1.3  1994/11/22  21:12:11  cdaq
* (SPB) Recopied from hms file and modified names for SOS
*
* Revision 1.2  1994/06/01  15:37:05  cdaq
* (SAW) Add Abort and err arguments
*
* Revision 1.1  1994/04/13  18:19:01  cdaq
* Initial revision
*
*-------------------------------------------------------------------

      implicit none

      include 'sos_data_structures.cmn'
      include 'sos_scin_parms.cmn'
      include 'sos_scin_tof.cmn'
      include 'sos_statistics.cmn'

      logical abort
      character*(*) err
      character*20 here
      parameter (here='s_init_scin')

      integer*4 plane,counter
      save
* 
*
* initialize some position parameters.
      snum_scin_counters(1) = sscin_1x_nr
      snum_scin_counters(2) = sscin_1y_nr
      snum_scin_counters(3) = sscin_2x_nr
      snum_scin_counters(4) = sscin_2y_nr

      sstat_numevents=0

      do plane = 1 , snum_scin_planes
        do counter = 1 , snum_scin_counters(plane)

* initialize tof parameters.

          if (plane .eq. 1) then
            shodo_width(plane,counter) = sscin_1x_size
            shodo_center(plane,counter) =
     1           sscin_1x_center(counter) + sscin_1x_offset
          else if (plane .eq. 2) then
            shodo_width(plane,counter) = sscin_1y_size
            shodo_center(plane,counter) =
     1           sscin_1y_center(counter) + sscin_1y_offset
          else if (plane .eq. 3) then
            shodo_width(plane,counter) = sscin_2x_size
            shodo_center(plane,counter) =
     1           sscin_2x_center(counter) + sscin_2x_offset
          else if (plane .eq. 4) then
            shodo_width(plane,counter) = sscin_2y_size
            shodo_center(plane,counter) =
     1           sscin_2y_center(counter) + sscin_2y_offset
          else                          ! Error in plane number
            abort = .true.
            write(err,*) 'Trying to init. sos hodoscope plane',plane
            call g_prepend(here,err)
            return
          endif

          sstat_trk(plane,counter)=0
          sstat_poshit(plane,counter)=0
          sstat_neghit(plane,counter)=0
          sstat_andhit(plane,counter)=0
          sstat_orhit(plane,counter)=0

        enddo                           !loop over counters
      enddo                             !loop over planes

* need expected particle velocity for start time calculation.
      sbeta_pcent = spcentral/sqrt(spcentral*spcentral+spartmass*spartmass)

      return
      end
