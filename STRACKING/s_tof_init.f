        subroutine s_tof_init(abort,errmsg)

*-------------------------------------------------------------------
* author: John Arrington
* created: 2/22/94
*
* s_tof_init sets up the track independant parameters
* for fitting the tof of the particle.
*
* modifications: 31 Mar 1994    DFG  Check for 0 hits
* $Log$
* Revision 1.1  1994/04/13 18:45:03  cdaq
* Initial revision
*
*-------------------------------------------------------------------

        implicit none

        include 'gen_data_structures.cmn'
        include 'sos_scin_parms.cmn'
        include 'sos_scin_tof.cmn'

        logical abort
        character*1024 errmsg
        character*20 here
        parameter (here = 's_tof_init')

        integer*4 ihit,plane,counter
        save

        if(sscin_tot_hits.gt.0) then
        do ihit = 1 , sscin_tot_hits

          plane = sscin_plane_num(ihit)     !from s_raw_scin common block.
          counter = sscin_counter_num(ihit)

          sscin_pos_sigma(ihit) = shodo_pos_sigma(plane,counter)
          sscin_neg_sigma(ihit) = shodo_neg_sigma(plane,counter)
          sscin_center_coord(ihit) = shodo_center_coord(plane,counter)
          sscin_vel_light(ihit) = shodo_vel_light(plane,counter)
          sscin_pos_phc_coeff(ihit) = shodo_pos_phc_coeff(plane,counter)
          sscin_neg_phc_coeff(ihit) = shodo_neg_phc_coeff(plane,counter)
          sscin_pos_time_offset(ihit) = shodo_pos_time_offset(plane,counter)
          sscin_neg_time_offset(ihit) = shodo_neg_time_offset(plane,counter)

          if (plane .eq. 1) then                   !1x
            sscin_zpos(ihit) = sscin_1x_zpos
            sscin_pos_coord(ihit) = sscin_1x_left
            sscin_neg_coord(ihit) = sscin_1x_right
            sscin_width(ihit) = sscin_1x_size
          else if (plane .eq. 2) then              !1y
            sscin_zpos(ihit) = sscin_1y_zpos
            sscin_pos_coord(ihit) = sscin_1y_bot
            sscin_neg_coord(ihit) = sscin_1y_top
            sscin_width(ihit) = sscin_1y_size
          else if (plane .eq. 3) then              !2x
            sscin_zpos(ihit) = sscin_2x_zpos
            sscin_pos_coord(ihit) = sscin_2x_left
            sscin_neg_coord(ihit) = sscin_2x_right
            sscin_width(ihit) = sscin_2x_size
          else if (plane .eq. 4) then              !2y
            sscin_zpos(ihit) = sscin_2y_zpos
            sscin_pos_coord(ihit) = sscin_2y_bot
            sscin_neg_coord(ihit) = sscin_2y_top
            sscin_width(ihit) = sscin_2y_size
          else
              abort = .true.
              write(errmsg,*) 'Trying to init. sos hodoscope plane',plane
              call g_prepend(here,errmsg)
              return
          endif

        enddo
        endif      ! end test on zero hits
        return
        end
