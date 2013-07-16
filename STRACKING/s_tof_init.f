      subroutine s_tof_init(abort,err)

*-------------------------------------------------------------------
* author: John Arrington
* created: 2/22/94
*
* s_tof_init sets up the track independant parameters
* for fitting the tof of the particle.
*
* modifications: 31 Mar 1994    DFG  Check for 0 hits
* $Log: s_tof_init.f,v $
* Revision 1.5  1995/05/22 19:46:00  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.4  1995/02/23  15:58:54  cdaq
* (JRA)  Change shodo_center_coord to shodo_center.
*        Make minph variables into per pmt constants.
*
* Revision 1.3  1994/11/23  14:23:05  cdaq
* (SPB) Recopied from hms file and modified names for SOS
*
* Revision 1.2  1994/06/01  15:40:08  cdaq
* (SAW) Change declaration of err to *(*)
*
* Revision 1.1  1994/04/13  18:45:03  cdaq
* Initial revision
*
*-------------------------------------------------------------------

      implicit none

      include 'sos_data_structures.cmn'
      include 'sos_scin_parms.cmn'
      include 'sos_scin_tof.cmn'

      logical abort
      character*(*) err
      character*20 here
      parameter (here = 's_tof_init')

      integer*4 ihit,plane,counter
      save

      if(sscin_tot_hits.gt.0) then
        do ihit = 1 , sscin_tot_hits

          plane = sscin_plane_num(ihit) !from s_raw_scin common block.
          counter = sscin_counter_num(ihit)
          
          sscin_slop(ihit) = shodo_slop(plane)
          sscin_pos_sigma(ihit) = shodo_pos_sigma(plane,counter)
          sscin_neg_sigma(ihit) = shodo_neg_sigma(plane,counter)
          sscin_center_coord(ihit) = shodo_center(plane,counter)
          sscin_vel_light(ihit) = shodo_vel_light(plane,counter)
          sscin_pos_phc_coeff(ihit) = shodo_pos_phc_coeff(plane,counter)
          sscin_neg_phc_coeff(ihit) = shodo_neg_phc_coeff(plane,counter)
          sscin_pos_time_offset(ihit) = shodo_pos_time_offset(plane,counter)
          sscin_neg_time_offset(ihit) = shodo_neg_time_offset(plane,counter)
          sscin_pos_minph(ihit) = shodo_pos_minph(plane,counter)
          sscin_neg_minph(ihit) = shodo_neg_minph(plane,counter)

          if (plane .eq. 1) then        !1x
            sscin_zpos(ihit) = sscin_1x_zpos
            if (2*int(float(counter)/2.) .eq. counter) then !even tube, in back.
              sscin_zpos(ihit) = sscin_zpos(ihit) + sscin_1x_dzpos
            endif
            sscin_pos_coord(ihit) = sscin_1x_left
            sscin_neg_coord(ihit) = sscin_1x_right
            sscin_width(ihit) = sscin_1x_size
          else if (plane .eq. 2) then   !1y
            sscin_zpos(ihit) = sscin_1y_zpos
            if (2*int(float(counter)/2.) .eq. counter) then !even tube, in back.
              sscin_zpos(ihit) = sscin_zpos(ihit) + sscin_1y_dzpos
            endif
            sscin_pos_coord(ihit) = sscin_1y_bot
            sscin_neg_coord(ihit) = sscin_1y_top
            sscin_width(ihit) = sscin_1y_size
          else if (plane .eq. 3) then   !2x
            sscin_zpos(ihit) = sscin_2x_zpos
            if (2*int(float(counter)/2.) .eq. counter) then !even tube, in back.
              sscin_zpos(ihit) = sscin_zpos(ihit) + sscin_2x_dzpos
            endif
            sscin_pos_coord(ihit) = sscin_2x_left
            sscin_neg_coord(ihit) = sscin_2x_right
            sscin_width(ihit) = sscin_2x_size
          else if (plane .eq. 4) then   !2y
            sscin_zpos(ihit) = sscin_2y_zpos
            if (2*int(float(counter)/2.) .eq. counter) then !even tube, in back.
              sscin_zpos(ihit) = sscin_zpos(ihit) + sscin_2y_dzpos
            endif
            sscin_pos_coord(ihit) = sscin_2y_bot
            sscin_neg_coord(ihit) = sscin_2y_top
            sscin_width(ihit) = sscin_2y_size
          else
            abort = .true.
            write(err,*) 'Trying to init. sos hodoscope plane',plane
            call g_prepend(here,err)
            return
          endif

        enddo
      endif                             ! end test on zero hits
      return
      end
