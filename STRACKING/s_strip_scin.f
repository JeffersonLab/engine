      subroutine s_strip_scin(abort,err)

*-------------------------------------------------------------------
* author: John Arrington
* created: 6/25/94
*
* s_strip_scin converts the raw hits to arrays over hits
* with good TDC values.
* $Log: s_strip_scin.f,v $
* Revision 1.7  1999/02/23 19:00:39  csa
* (JRA) Remove sdebugcalcpeds stuff
*
* Revision 1.6  1996/01/17 18:57:36  cdaq
* (JRA) Add sdebugcalcpeds flag
*
* Revision 1.5  1995/08/31 20:44:25  cdaq
* (JRA) Accumulate pedestals from pedestal events.
*
* Revision 1.4  1995/05/22  19:45:56  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.3  1995/05/11  15:02:18  cdaq
* (JRA) Cosmetic changes
*
* Revision 1.2  1995/02/10  19:14:37  cdaq
* JRA) Make sscin_all_adc_pos/neg floating
*
* Revision 1.1  1994/11/23  14:01:45  cdaq
* Initial revision
*
*-------------------------------------------------------------------

      implicit none

      include 'sos_data_structures.cmn'
      include 'sos_scin_parms.cmn'
      include 'sos_scin_tof.cmn'
      include 'sos_tracking.cmn'

      logical abort
      character*(*) err
      character*12 here
      parameter (here = 's_strip_scin')

      integer*4 ihit,igoodhit,ind,plane,counter
      integer*4 ip,ic
      save
      abort = .false.
        
      igoodhit = 0
      sscin_tot_hits = 0
      do ind = 1, snum_scin_planes
        sscin_hits_per_plane(ind) = 0
        sscin_sing_counter(ind) = -1
      enddo
        
      do ihit = 1 , sscin_all_tot_hits  ! pick out 'good' hits.

**    Criteria for good hit is at least one valid tdc value.
        if (((sscin_all_tdc_pos(ihit) .ge. sscin_tdc_min).and.
     1       (sscin_all_tdc_pos(ihit) .le. sscin_tdc_max)) .or. 
     2       ((sscin_all_tdc_neg(ihit) .ge. sscin_tdc_min).and.
     3       (sscin_all_tdc_neg(ihit) .le. sscin_tdc_max))) then !good hit
          
          igoodhit = igoodhit + 1
          sscin_tot_hits = sscin_tot_hits + 1
          ip = sscin_all_plane_num(ihit)
          sscin_plane_num(igoodhit) = ip
          ic = sscin_all_counter_num(ihit)
          sscin_counter_num(igoodhit) = ic
          sscin_adc_pos(igoodhit) = float(sscin_all_adc_pos(ihit)) -
     $         sscin_all_ped_pos(ip,ic)
          sscin_adc_neg(igoodhit) = float(sscin_all_adc_neg(ihit)) -
     $         sscin_all_ped_neg(ip,ic)
          sscin_tdc_pos(igoodhit) = sscin_all_tdc_pos(ihit)
          sscin_tdc_neg(igoodhit) = sscin_all_tdc_neg(ihit)
          sscin_hits_per_plane(sscin_plane_num(igoodhit)) = 
     $         sscin_hits_per_plane(sscin_plane_num(igoodhit)) + 1
*djm register counter which is hit. if more than one counter is hit per event,
* only the last one will be histogrammed. this will bias events which have more
* than one hit per plane, so it's only really useful for looking at single hits.
* if you need to see all the hits, then hardwire it. 
          plane = sscin_PLANE_NUM(igoodhit)
          counter = sscin_COUNTER_NUM(igoodhit)
          if(plane.ge.1.and.plane.le.4) sscin_sing_counter(plane) = counter
        endif
      enddo

      abort = .false.
      return
      end
