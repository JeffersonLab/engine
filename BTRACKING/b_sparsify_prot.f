      subroutine b_sparsify_prot(ABORT,err)

      implicit none
      save

      logical ABORT
      character*(*) err
      character*15 here
      parameter (here='b_sparsify_prot')

c     loop over all hits, subtract peds, apply thresholds, and 
c     fill decoded data arrays

      integer*4 ihit,icell
      integer*4 ngood
      integer*4 irow,icol
      integer*4 adc_val 
*     check number of hits:
      include 'bigcal_data_structures.cmn'
      include 'bigcal_gain_parms.cmn'

      err=' '
      if(BIGCAL_PROT_NHIT.lt.0.or.BIGCAL_PROT_NHIT.gt.
     $     BIGCAL_PROT_MAXHITS) then 
         write(6,*) here,':bigcal_prot_nhit=',BIGCAL_PROT_NHIT
         return
      endif
*     "zero" decoded adcs:
      do icell=1,BIGCAL_PROT_MAXHITS
         BIGCAL_PROT_ADC_DECODED(icell)=-100.
      enddo

      ngood = 0

*     loop over raw hits: 
      if(bigcal_prot_nhit.gt.0) then
        do ihit=1,BIGCAL_PROT_NHIT
          irow = BIGCAL_PROT_IY(ihit)
          icol = BIGCAL_PROT_IX(ihit)
          icell = icol + BIGCAL_PROT_NX*(irow - 1)
          adc_val = BIGCAL_PROT_ADC_RAW(ihit)
          BIGCAL_PROT_RAW_DET(icell) = adc_val
          BIGCAL_ALL_RAW_DET(icell) = adc_val
          if(adc_val.ge.0) then 
            BIGCAL_PROT_ADC_DECODED(icell) = float(adc_val) - 
     $           BIGCAL_PROT_PED_MEAN(icell)
          endif
c     "sparsify" the data
          if(BIGCAL_PROT_ADC_DECODED(icell).gt.
     $         BIGCAL_PROT_ADC_THRESHOLD(icell)) then
            ngood = ngood + 1
            BIGCAL_PROT_ADC_GOOD(ngood) = BIGCAL_PROT_ADC_DECODED(icell)
            BIGCAL_PROT_IYGOOD(ngood) = irow
            BIGCAL_PROT_IXGOOD(ngood) = icol
          endif
        enddo
      endif

      BIGCAL_PROT_NGOOD = ngood

      return
      end
