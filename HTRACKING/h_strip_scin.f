      subroutine h_strip_scin(abort,err)

*-------------------------------------------------------------------
* author: John Arrington
* created: 6/25/94
*
* h_strip_scin converts the raw hits to arrays over hits
* with good TDC values.
* $Log$
* Revision 1.1  1994/06/27 02:41:12  cdaq
* Initial revision
*
*-------------------------------------------------------------------

      implicit none

      include 'gen_data_structures.cmn'
      include 'hms_scin_parms.cmn'
      include 'hms_scin_tof.cmn'

      logical abort
      character*(*) err
      character*12 here
      parameter (here = 'h_strip_scin')

      integer*4 ihit,igoodhit,ind
      save
        
      igoodhit = 0
      hscin_tot_hits = 0
        
      do ihit = 1 , hscin_all_tot_hits  ! pick out 'good' hits.

**    Criteria for good hit is at least one valid tdc value.
         if (((hscin_all_tdc_pos(ihit) .ge. hscin_tdc_min).and.
     1        (hscin_all_tdc_pos(ihit) .le. hscin_tdc_max)) .or. 
     2        ((hscin_all_tdc_neg(ihit) .ge. hscin_tdc_min).and.
     3        (hscin_all_tdc_neg(ihit) .le. hscin_tdc_max))) then !good hit

            igoodhit = igoodhit + 1
            hscin_tot_hits = hscin_tot_hits + 1
            hscin_plane_num(igoodhit) = hscin_all_plane_num(ihit)
            hscin_counter_num(igoodhit) = hscin_all_counter_num(ihit)
            hscin_adc_pos(igoodhit) = hscin_all_adc_pos(ihit)
            hscin_adc_neg(igoodhit) = hscin_all_adc_neg(ihit)
            hscin_tdc_pos(igoodhit) = hscin_all_tdc_pos(ihit)
            hscin_tdc_neg(igoodhit) = hscin_all_tdc_neg(ihit)
         endif
      enddo
      abort = .false.
      return
      end
