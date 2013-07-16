      subroutine h_tof_init(abort,err)

*-------------------------------------------------------------------
* author: John Arrington
* created: 2/22/94
*
* h_tof_init sets up the track independant parameters
* for fitting the tof of the particle.
*
* modifications: 31 Mar 1994    DFG  Check for 0 hits
* $Log: h_tof_init.f,v $
* Revision 1.6  1995/05/22 19:39:30  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.5  1995/02/21  16:57:28  cdaq
* (JRA) Change hhodo_center_coord to hhodo_center
*
* Revision 1.4  1995/02/02  16:12:50  cdaq
* (JRA) Make minph variables into per pmt constants
*
* Revision 1.3  1994/09/13  21:30:01  cdaq
* (JRA) Add staggering of scintillator counters
*
* Revision 1.2  1994/06/01  15:39:34  cdaq
* (SAW) Change declaration of err to *(*)
*
* Revision 1.1  1994/04/13  16:29:31  cdaq
* Initial revision
*
*-------------------------------------------------------------------

      implicit none

      include 'hms_data_structures.cmn'
      include 'hms_scin_parms.cmn'
      include 'hms_scin_tof.cmn'

      logical abort
      character*(*) err
      character*20 here
      parameter (here = 'h_tof_init')

      integer*4 ihit,plane,counter
      save

      if(hscin_tot_hits.gt.0) then
        do ihit = 1 , hscin_tot_hits

          plane = hscin_plane_num(ihit) !from h_raw_scin common block.
          counter = hscin_counter_num(ihit)

          hscin_slop(ihit) = hhodo_slop(plane)
          hscin_pos_sigma(ihit) = hhodo_pos_sigma(plane,counter)
          hscin_neg_sigma(ihit) = hhodo_neg_sigma(plane,counter)
          hscin_center_coord(ihit) = hhodo_center(plane,counter)
          hscin_vel_light(ihit) = hhodo_vel_light(plane,counter)
          hscin_pos_phc_coeff(ihit) = hhodo_pos_phc_coeff(plane,counter)
          hscin_neg_phc_coeff(ihit) = hhodo_neg_phc_coeff(plane,counter)
          hscin_pos_time_offset(ihit) = hhodo_pos_time_offset(plane,counter)
          hscin_neg_time_offset(ihit) = hhodo_neg_time_offset(plane,counter)
          hscin_pos_minph(ihit) = hhodo_pos_minph(plane,counter)
          hscin_neg_minph(ihit) = hhodo_neg_minph(plane,counter)

          if (plane .eq. 1) then        !1x
            hscin_zpos(ihit) = hscin_1x_zpos
            if (2*int(float(counter)/2.) .eq. counter) then !even tube, in back.
              hscin_zpos(ihit) = hscin_zpos(ihit) + hscin_1x_dzpos
            endif
            hscin_pos_coord(ihit) = hscin_1x_left
            hscin_neg_coord(ihit) = hscin_1x_right
            hscin_width(ihit) = hscin_1x_size
          else if (plane .eq. 2) then   !1y
            hscin_zpos(ihit) = hscin_1y_zpos
            if (2*int(float(counter)/2.) .eq. counter) then !even tube, in back.
              hscin_zpos(ihit) = hscin_zpos(ihit) + hscin_1y_dzpos
            endif
            hscin_pos_coord(ihit) = hscin_1y_bot
            hscin_neg_coord(ihit) = hscin_1y_top
            hscin_width(ihit) = hscin_1y_size
          else if (plane .eq. 3) then   !2x
            hscin_zpos(ihit) = hscin_2x_zpos
            if (2*int(float(counter)/2.) .eq. counter) then !even tube, in back.
              hscin_zpos(ihit) = hscin_zpos(ihit) + hscin_2x_dzpos
            endif
            hscin_pos_coord(ihit) = hscin_2x_left
            hscin_neg_coord(ihit) = hscin_2x_right
            hscin_width(ihit) = hscin_2x_size
          else if (plane .eq. 4) then   !2y
            hscin_zpos(ihit) = hscin_2y_zpos
            if (2*int(float(counter)/2.) .eq. counter) then !even tube, in back.
              hscin_zpos(ihit) = hscin_zpos(ihit) + hscin_2y_dzpos
            endif
            hscin_pos_coord(ihit) = hscin_2y_bot
            hscin_neg_coord(ihit) = hscin_2y_top
            hscin_width(ihit) = hscin_2y_size
          else
            abort = .true.
            write(err,*) 'Trying to init. hms hodoscope plane',plane
            call g_prepend(here,err)
            return
          endif

        enddo
      endif                             ! end test on zero hits
      return
      end
