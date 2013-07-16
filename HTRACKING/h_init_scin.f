      subroutine h_init_scin(ABORT,err)

*-------------------------------------------------------------------
* author: John Arrington
* created: 2/22/94
*
* h_init_scin initializes the corrections and parameters
* for the scintillators.  Corrections are read from data files
* or the database.  Arrays used by the tof fitting routines
* are filled from the CTP variables input from the hms_positions
* parameter file.
*
* modifications:
*       23 March 1993   DFG
*            Remove /nolist from include statement. UNIX doesn't like it.
* $Log: h_init_scin.f,v $
* Revision 1.7  1996/04/30 12:44:35  saw
* (JRA) Calculate expected particle velocity
*
* Revision 1.6  1995/05/22 19:39:14  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.5  1995/02/23  13:35:34  cdaq
* (JRA) Remove _coord fro hhodo_center array.  Edge coordinates by
* center locations.
*
* Revision 1.4  1994/09/13  19:40:10  cdaq
* (JRA) Remove some unused variables
*
* Revision 1.3  1994/06/14  03:58:10  cdaq
* (DFG) remove hard wired numbers
*
* Revision 1.2  1994/06/01  15:36:20  cdaq
* (SAW) Add Abort and err arguments
*
* Revision 1.1  1994/04/13  15:39:39  cdaq
* Initial revision
*
*-------------------------------------------------------------------

      implicit none

      include 'hms_data_structures.cmn'
      include 'hms_scin_parms.cmn'
      include 'hms_scin_tof.cmn'
      include 'hms_statistics.cmn'

      logical abort
      character*(*) err
      character*20 here
      parameter (here='h_init_scin')

      integer*4 plane,counter
      save
* 
*
*     initialize some position parameters.
      hnum_scin_counters(1) = hscin_1x_nr
      hnum_scin_counters(2) = hscin_1y_nr
      hnum_scin_counters(3) = hscin_2x_nr
      hnum_scin_counters(4) = hscin_2y_nr

      hstat_numevents=0

      do plane = 1 , hnum_scin_planes
        do counter = 1 , hnum_scin_counters(plane)

* initialize tof parameters.

          if (plane .eq. 1) then
            hhodo_width(plane,counter) = hscin_1x_size
            hhodo_center(plane,counter) =
     1           hscin_1x_center(counter) + hscin_1x_offset
          else if (plane .eq. 2) then
            hhodo_width(plane,counter) = hscin_1y_size
            hhodo_center(plane,counter) =
     1           hscin_1y_center(counter) + hscin_1y_offset
          else if (plane .eq. 3) then
            hhodo_width(plane,counter) = hscin_2x_size
            hhodo_center(plane,counter) =
     1           hscin_2x_center(counter) + hscin_2x_offset
          else if (plane .eq. 4) then
            hhodo_width(plane,counter) = hscin_2y_size
            hhodo_center(plane,counter) =
     1           hscin_2y_center(counter) + hscin_2y_offset
          else                          ! Error in plane number
            abort = .true.
            write(err,*) 'Trying to init. hms hodoscope plane',plane
            call g_prepend(here,err)
            return
          endif
            
          hstat_trk(plane,counter)=0
          hstat_poshit(plane,counter)=0
          hstat_neghit(plane,counter)=0
          hstat_andhit(plane,counter)=0
          hstat_orhit(plane,counter)=0
          
        enddo                           !loop over counters
      enddo                             !loop over planes
      
* need expected particle velocity for start time calculation.
      hbeta_pcent = hpcentral/sqrt(hpcentral*hpcentral+hpartmass*hpartmass)


      return
      end
