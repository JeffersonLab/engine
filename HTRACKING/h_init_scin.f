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
* Revision 1.2  1994/06/01 15:36:20  cdaq
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
* Clear arrays since some some entries left blank (array up to 16, only 10
* elements in some planes
        do plane = 1 , hnum_scin_planes
          do counter = 1 , hnum_scin_elements
            hhodo_center_coord(plane,counter) = 0.
            hhodo_width(plane,counter) = 0.
            hhodo_pos_coord(plane,counter) = 0.
            hhodo_neg_coord(plane,counter) = 0.
            hhodo_vel_light(plane,counter) = 0.
            hhodo_pos_sigma(plane,counter) = 0.
            hhodo_neg_sigma(plane,counter) = 0.
            hhodo_pos_phc_coeff(plane,counter) = 0.
            hhodo_neg_phc_coeff(plane,counter) = 0.
            hhodo_pos_time_offset(plane,counter) = 0.
            hhodo_neg_time_offset(plane,counter) = 0.
          enddo
        enddo

* initialize some position parameters.
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

**********************************************************
* TEMPORARY SECTION: These will be read from parm file
            hhodo_slop(plane) = 3.0
            hhodo_vel_light(plane,counter) = 17.0      !get from parm file.
            hhodo_pos_sigma(plane,counter) = 0.3       !assume .3 ns for now.
            hhodo_neg_sigma(plane,counter) = 0.3
            hhodo_pos_phc_coeff(plane,counter) = 0.0   !get from parm file.
            hhodo_neg_phc_coeff(plane,counter) = 0.0
            hhodo_pos_time_offset(plane,counter) = 0.0 !get from parm file.
            hhodo_neg_time_offset(plane,counter) = 0.0
* TEMPORARY SECTION: These will be read from parm file
**********************************************************

            if (plane .eq. 1) then
              hhodo_width(plane,counter) = hscin_1x_size
              hhodo_center_coord(plane,counter) =
     1        (hscin_1x_top(counter) + hscin_1x_size/2.)
            else if (plane .eq. 2) then
              hhodo_width(plane,counter) = hscin_1y_size
              hhodo_center_coord(plane,counter) =
     1        (hscin_1y_left(counter) - hscin_1y_size/2.)
            else if (plane .eq. 3) then
              hhodo_width(plane,counter) = hscin_2x_size
              hhodo_center_coord(plane,counter) =
     1        (hscin_2x_top(counter) + hscin_2x_size/2.)
            else if (plane .eq. 4) then
              hhodo_width(plane,counter) = hscin_2y_size
              hhodo_center_coord(plane,counter) =
     1        (hscin_2y_left(counter) - hscin_2y_size/2.)
            else                          ! Error in plane number
              abort = .true.
              write(err,*) 'Trying to init. hms hodoscope plane',plane
              call g_prepend(here,err)
              return
            endif

          enddo    !loop over counters
        enddo      !loop over planes
*       check I make it here

        return
        end
