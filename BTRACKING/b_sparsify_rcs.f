      subroutine b_sparsify_rcs(ABORT,err)

      implicit none
      save

      logical ABORT
      character*(*) err
      character*15 here
      parameter (here='b_sparsify_rcs')
      
c     loop over all hits, subtract peds, apply thresholds, and 
c     fill decoded data arrays
c     RCS_IY from detector map will be 33-56. Subtract 32 in this routine and 
c     then we can assume from here on out that RCS_IY starts at 1 and goes to 24
      
      integer*4 ihit,icell
      integer*4 ngood
      integer*4 irow,icol
      integer*4 adc_val 

      include 'bigcal_data_structures.cmn'
      include 'bigcal_gain_parms.cmn'

*     check number of hits:
      err=' '
      if(BIGCAL_RCS_NHIT.lt.0.or.BIGCAL_RCS_NHIT.gt.
     $     BIGCAL_RCS_MAXHITS) then 
         write(6,*) here,':bigcal_rcs_nhit=',BIGCAL_RCS_NHIT
         return
      endif
*     "zero" decoded adcs:
      do icell=1,BIGCAL_RCS_MAXHITS
         BIGCAL_RCS_ADC_DECODED(icell)=-100.
      enddo

      ngood = 0

*     loop over raw hits: 
      do ihit=1,BIGCAL_RCS_NHIT
         irow = BIGCAL_RCS_IY(ihit) - BIGCAL_PROT_NY 
         icol = BIGCAL_RCS_IX(ihit)
         icell = icol + BIGCAL_RCS_NX*(irow - 1)
         adc_val = BIGCAL_RCS_ADC_RAW(ihit)
         BIGCAL_RCS_RAW_DET(icell) = adc_val
         if(adc_val.ge.0) then 
            BIGCAL_RCS_ADC_DECODED(icell) = float(adc_val) - 
     $           BIGCAL_RCS_PED_MEAN(icell)
         endif
c     "sparsify" the data
         if(BIGCAL_RCS_ADC_DECODED(icell).gt.
     $        BIGCAL_RCS_ADC_THRESHOLD(icell)) then
            ngood = ngood + 1
            BIGCAL_RCS_ADC_GOOD(ngood) = BIGCAL_RCS_ADC_DECODED(icell)
            BIGCAL_RCS_IYGOOD(ngood) = irow
            BIGCAL_RCS_IXGOOD(ngood) = icol
         endif
      enddo
      
      BIGCAL_RCS_NGOOD = ngood

      return
      end
