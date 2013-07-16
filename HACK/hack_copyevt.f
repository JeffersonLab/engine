*----------------------------------------------------------------
*-- file: hack_copyevt.f
*- subroutine copies adc and tdc data into fixed array, it allows for
*  sparsified adc and tdc readout
*- this is a complete routine and should have no additional user code
* $Log: hack_copyevt.f,v $
* Revision 1.2  1995/05/24 13:47:29  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.1  1994/07/25  18:03:36  cdaq
* Initial revision
*
*
      subroutine hack_copyevt(ABORT, err)
      implicit none
      include 'hms_data_structures.cmn'
      include 'hack_.cmn'
      logical ABORT
      character*(*) err
*
      integer i,j,k
      integer tdcup,tdcdo,adcup,adcdo,plane,count
*-----------------------------------------------------------------------------
      ABORT = .FALSE.
      err = ' '
*-----------------------------------------------------------------------------
*  initialize array with values of -1
      do k = 1,4                      !4 planes
        do j = 1,16                   !scintillators in a plane
          hack_hmssc_au(j,k) = -1     !"no data" value of -1, adc up
          hack_hmssc_ad(j,k) = -1     !"no data" value of -1, adc down
          hack_hmssc_tu(j,k) = -1     !"no data" value of -1, tdc up
          hack_hmssc_td(j,k) = -1     !"no data" value of -1, tdc down
          hack_hmssc_go(j,k) = -1     !"no data" value of -1, good data indic.
        enddo
      enddo
*
*  copy HMS scintillator data into fixed array 
      do i = 1,hscin_all_tot_hits               !copy all hits to hack array
        tdcup = hscin_all_tdc_pos(i)
        tdcdo = hscin_all_tdc_neg(i)
        adcup = hscin_all_adc_pos(i)
        adcdo = hscin_all_adc_neg(i)
        plane = hscin_all_plane_num(i)
        count = hscin_all_counter_num(i)
        hack_hmssc_au(count,plane) = adcup
        hack_hmssc_ad(count,plane) = adcdo
        hack_hmssc_tu(count,plane) = tdcup
        hack_hmssc_td(count,plane) = tdcdo
        hack_hmssc_go(count,plane) = 0        !detector has data (-1=no data)
        if ((tdcup.gt.0.and.tdcup.lt.4000).or.
     &    (tdcdo.gt.0.and.tdcdo.lt.4000)) 
     &    hack_hmssc_go(count,plane) = 1      !one tdc present
        if ((tdcup.gt.0.and.tdcup.lt.4000).and.
     &    (tdcdo.gt.0.and.tdcdo.lt.4000)) 
     &    hack_hmssc_go(count,plane) = 2      !both tdcs present
      enddo
*
* for non-sparsified readout the data can also be found in the following way
*	hscin_all_adc_pos(1-16)  == S1X(1-16)  (same for _neg)
*                        (17-26) == S1Y(1-10)
*                        (27-42) == S2X(1-16)
*                        (43-52) == S2Y(1-10)
      return
      end
*
