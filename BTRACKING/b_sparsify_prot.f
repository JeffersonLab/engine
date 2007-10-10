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
      integer*4 ngood,nbad,nbad2
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
      nbad = 0
      nbad2 = 0

*     loop over raw hits: 
      if(bigcal_prot_nhit.gt.0) then
        do ihit=1,BIGCAL_PROT_NHIT
          irow = BIGCAL_PROT_IY(ihit)
          icol = BIGCAL_PROT_IX(ihit)
          icell = icol + BIGCAL_PROT_NX*(irow - 1)
          adc_val = BIGCAL_PROT_ADC_RAW(ihit)

          bigcal_prot_nhit_ch(icell) = bigcal_prot_nhit_ch(icell) + 1

          if(bigcal_prot_nhit_ch(icell).eq.1) then
             BIGCAL_PROT_RAW_DET(icell) = adc_val
          endif
          if(bigcal_prot_nhit_ch(icell).gt.1) then ! fill bad hits array
             nbad = nbad + 1
             nbad2 = nbad2 + 1
             if(bigcal_prot_nhit_ch(icell).eq.2) then ! first bad hit
                bigcal_prot_iybad(nbad) = irow
                bigcal_prot_ixbad(nbad) = icol
c     bigcal_prot_raw_det(icell) should still contain the adc value of the first hit
c     in this channel
                bigcal_prot_adc_bad(nbad) = bigcal_prot_raw_det(icell)
                nbad = nbad + 1
             endif
             bigcal_prot_iybad(nbad) = irow
             bigcal_prot_ixbad(nbad) = icol
             bigcal_prot_adc_bad(nbad) = adc_val
          endif

c          BIGCAL_ALL_RAW_DET(icell) = adc_val
          if(adc_val.ge.0) then 
            BIGCAL_PROT_ADC_DECODED(icell) = float(adc_val) - 
     $           BIGCAL_PROT_PED_MEAN(icell)
          endif
c     "sparsify" the data
          if(BIGCAL_PROT_ADC_DECODED(icell).ge.
     $         BIGCAL_PROT_ADC_THRESHOLD(icell)) then
            ngood = ngood + 1
            BIGCAL_PROT_ADC_GOOD(ngood) = BIGCAL_PROT_ADC_DECODED(icell)
            BIGCAL_PROT_IYGOOD(ngood) = irow
            BIGCAL_PROT_IXGOOD(ngood) = icol
          endif
        enddo
      endif

      BIGCAL_PROT_NGOOD = ngood
      bigcal_prot_nbad = nbad2
      bigcal_prot_badplusgood = nbad

      return
      end
