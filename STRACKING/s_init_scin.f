        subroutine s_init_scin

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
* $Log$
* Revision 1.1  1994/04/13 18:19:01  cdaq
* Initial revision
*
*-------------------------------------------------------------------

        implicit none

        include 'gen_data_structures.cmn'
        include 'sos_scin_parms.cmn'

        logical abort
        character*1024 errmsg
        character*20 here
        parameter (here='s_init_scin')

        integer*4 plane,counter
        save
* 
*
* Clear arrays since some some entries left blank (array up to 16, only 10
* elements in some planes
        do plane = 1 , snum_scin_planes
          do counter = 1 , snum_scin_elements
            shodo_center_coord(plane,counter) = 0.
            shodo_width(plane,counter) = 0.
            shodo_pos_coord(plane,counter) = 0.
            shodo_neg_coord(plane,counter) = 0.
            shodo_vel_light(plane,counter) = 0.
            shodo_pos_sigma(plane,counter) = 0.
            shodo_neg_sigma(plane,counter) = 0.
            shodo_pos_phc_coeff(plane,counter) = 0.
            shodo_neg_phc_coeff(plane,counter) = 0.
            shodo_pos_time_offset(plane,counter) = 0.
            shodo_neg_time_offset(plane,counter) = 0.
          enddo
        enddo

* initialize some position parameters.
        snum_scin_counters(1) = sscin_1x_nr
        snum_scin_counters(2) = sscin_1y_nr
        snum_scin_counters(3) = sscin_2x_nr
        snum_scin_counters(4) = sscin_2y_nr
        shodo_zpos(1) = sscin_1x_zpos
        shodo_zpos(2) = sscin_1y_zpos
        shodo_zpos(3) = sscin_2x_zpos
        shodo_zpos(4) = sscin_2y_zpos

        do plane = 1 , snum_scin_planes
          do counter = 1 , snum_scin_counters(plane)

* initialize tof parameters.

**********************************************************
* TEMPORARY SECTION: These will be read from parm file
            shodo_slop(plane) = 3.0
            shodo_vel_light(plane,counter) = 17.0      !get from parm file.
            shodo_pos_sigma(plane,counter) = 0.3       !assume .3 ns for now.
            shodo_neg_sigma(plane,counter) = 0.3
            shodo_pos_phc_coeff(plane,counter) = 0.0   !get from parm file.
            shodo_neg_phc_coeff(plane,counter) = 0.0
            shodo_pos_time_offset(plane,counter) = 0.0 !get from parm file.
            shodo_neg_time_offset(plane,counter) = 0.0
* TEMPORARY SECTION: These will be read from parm file
**********************************************************

            if (plane .eq. 1) then
              shodo_width(plane,counter) = sscin_1x_size
              shodo_center_coord(plane,counter) =
     1        (sscin_1x_top(counter) + sscin_1x_size/2.)
            else if (plane .eq. 2) then
              shodo_width(plane,counter) = sscin_1y_size
              shodo_center_coord(plane,counter) =
     1        (sscin_1y_left(counter) - sscin_1y_size/2.)
            else if (plane .eq. 3) then
              shodo_width(plane,counter) = sscin_2x_size
              shodo_center_coord(plane,counter) =
     1        (sscin_2x_top(counter) + sscin_2x_size/2.)
            else if (plane .eq. 4) then
              shodo_width(plane,counter) = sscin_2y_size
              shodo_center_coord(plane,counter) =
     1        (sscin_2y_left(counter) - sscin_2y_size/2.)
            else                          ! Error in plane number
              abort = .true.
              write(errmsg,*) 'Trying to init. sos hodoscope plane',plane
              call g_prepend(here,errmsg)
              return
            endif

          enddo    !loop over counters
        enddo      !loop over planes
*       check I make it here

        return
        end
