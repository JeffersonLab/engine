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
* $Log$
* Revision 1.3  1994/06/14 03:58:10  cdaq
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

      include 'gen_data_structures.cmn'
      include 'hms_scin_parms.cmn'

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
      hhodo_zpos(1) = hscin_1x_zpos
      hhodo_zpos(2) = hscin_1y_zpos
      hhodo_zpos(3) = hscin_2x_zpos
      hhodo_zpos(4) = hscin_2y_zpos

      do plane = 1 , hnum_scin_planes
         do counter = 1 , hnum_scin_counters(plane)

* initialize tof parameters.

            if (plane .eq. 1) then
               hhodo_width(plane,counter) = hscin_1x_size
               hhodo_center_coord(plane,counter) =
     1              (hscin_1x_top(counter) + hscin_1x_size/2.)
            else if (plane .eq. 2) then
               hhodo_width(plane,counter) = hscin_1y_size
               hhodo_center_coord(plane,counter) =
     1              (hscin_1y_left(counter) - hscin_1y_size/2.)
            else if (plane .eq. 3) then
               hhodo_width(plane,counter) = hscin_2x_size
               hhodo_center_coord(plane,counter) =
     1              (hscin_2x_top(counter) + hscin_2x_size/2.)
            else if (plane .eq. 4) then
               hhodo_width(plane,counter) = hscin_2y_size
               hhodo_center_coord(plane,counter) =
     1              (hscin_2y_left(counter) - hscin_2y_size/2.)
            else                        ! Error in plane number
               abort = .true.
               write(err,*) 'Trying to init. hms hodoscope plane',plane
               call g_prepend(here,err)
               return
            endif

         enddo                          !loop over counters
      enddo                             !loop over planes
*     check I make it here

      return
      end
