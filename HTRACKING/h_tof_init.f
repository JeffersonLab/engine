      subroutine h_tof_init(abort,err)

*-------------------------------------------------------------------
* author: John Arrington
* created: 2/22/94
*
* h_tof_init sets up the track independant parameters
* for fitting the tof of the particle.
*
* modifications: 31 Mar 1994    DFG  Check for 0 hits
* $Log$
* Revision 1.6.24.1.2.1  2008/11/17 15:58:44  cdaq
* Removed old tof varaibles
*
* Revision 1.6.24.1  2007/10/24 16:37:16  cdaq
* *** empty log message ***
*
* Revision 1.6.22.1  2007/05/02 21:18:09  jones
* Add new code needed for  adjusting scintillator timing using P Bosted's method.
*
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
          hscin_pos_invadc_offset(ihit) = 
     >      hhodo_pos_invadc_offset(plane,counter)
          hscin_neg_invadc_offset(ihit) = 
     >      hhodo_neg_invadc_offset(plane,counter)
          hscin_pos_invadc_linear(ihit) = 
     >      max(10.,hhodo_pos_invadc_linear(plane,counter))
          hscin_neg_invadc_linear(ihit) = 
     >      max(10.,hhodo_neg_invadc_linear(plane,counter))
          hscin_pos_invadc_adc(ihit) = 
     >      hhodo_pos_invadc_adc(plane,counter)
          hscin_neg_invadc_adc(ihit) = 
     >      hhodo_neg_invadc_adc(plane,counter)

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
