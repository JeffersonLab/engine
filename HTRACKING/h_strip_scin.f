      subroutine h_strip_scin(abort,err)

*-------------------------------------------------------------------
* author: John Arrington
* created: 6/25/94
*
* h_strip_scin converts the raw hits to arrays over hits
* with good TDC values.
* $Log$
* Revision 1.8  1995/05/22 19:39:28  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.7  1995/05/11  15:01:22  cdaq
* (JRA) Cosmetic changes
*
* Revision 1.6  1995/02/02  13:07:17  cdaq
* (JRA) Make hscin_all_adc_pos/neg floating
*
* Revision 1.5  1994/10/17  20:47:03  cdaq
* (DJM) Change hscin_sing_counter index from ihit (wrong) to igoodhit (correct)
*
* Revision 1.4  1994/10/12  18:59:37  cdaq
* (DJM) Fill hscin_sing_counter hit patterns for hodoscope
*
* Revision 1.3  1994/10/11  19:05:59  cdaq
* (JRA) Subtract pedestals from adc's
*
* Revision 1.2  1994/06/29  03:42:15  cdaq
* (JRA) Clear and set nscin_hits_per_plane array
*
* Revision 1.1  1994/06/27  02:41:12  cdaq
* Initial revision
*
*-------------------------------------------------------------------

      implicit none

      include 'hms_data_structures.cmn'
      include 'hms_scin_parms.cmn'
      include 'hms_scin_tof.cmn'

      logical abort
      character*(*) err
      character*12 here
      parameter (here = 'h_strip_scin')

      integer*4 ihit,igoodhit,ind,plane,counter
      integer*4 ip,ic
      save
      abort = .false.
        
      igoodhit = 0
      hscin_tot_hits = 0
      do ind = 1, hnum_scin_planes
        hscin_hits_per_plane(ind) = 0
        hscin_sing_counter(ind) = -1
      enddo
        
      do ihit = 1 , hscin_all_tot_hits  ! pick out 'good' hits.

**    Criteria for good hit is at least one valid tdc value.
        if (((hscin_all_tdc_pos(ihit) .ge. hscin_tdc_min).and.
     1       (hscin_all_tdc_pos(ihit) .le. hscin_tdc_max)) .or. 
     2       ((hscin_all_tdc_neg(ihit) .ge. hscin_tdc_min).and.
     3       (hscin_all_tdc_neg(ihit) .le. hscin_tdc_max))) then !good hit
          
          igoodhit = igoodhit + 1
          hscin_tot_hits = hscin_tot_hits + 1
          ip = hscin_all_plane_num(ihit)
          hscin_plane_num(igoodhit) = ip
          ic = hscin_all_counter_num(ihit)
          hscin_counter_num(igoodhit) = ic
          hscin_adc_pos(igoodhit) = float(hscin_all_adc_pos(ihit)) -
     $         hscin_all_ped_pos(ip,ic)
          hscin_adc_neg(igoodhit) = float(hscin_all_adc_neg(ihit)) -
     $         hscin_all_ped_neg(ip,ic)
          hscin_tdc_pos(igoodhit) = hscin_all_tdc_pos(ihit)
          hscin_tdc_neg(igoodhit) = hscin_all_tdc_neg(ihit)
          hscin_hits_per_plane(hscin_plane_num(igoodhit)) = 
     $         hscin_hits_per_plane(hscin_plane_num(igoodhit)) + 1
*djm register counter which is hit. if more than one counter is hit per event,
* only the last one will be histogrammed. this will bias events which have more
* than one hit per plane, so it's only really useful for looking at single hits.
* if you need to see all the hits, then hardwire it. 
          plane = HSCIN_PLANE_NUM(igoodhit)
          counter = HSCIN_COUNTER_NUM(igoodhit)
          if(plane.ge.1.and.plane.le.4) hscin_sing_counter(plane) = counter
        endif
      enddo

      abort = .false.
      return
      end
