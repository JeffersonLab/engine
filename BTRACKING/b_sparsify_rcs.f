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
      integer*4 ngood,nbad,nbad2
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
      nbad = 0
      nbad2 = 0

*     loop over raw hits: 
      if(bigcal_rcs_nhit.gt.0) then
        do ihit=1,BIGCAL_RCS_NHIT
          irow = BIGCAL_RCS_IY(ihit) - BIGCAL_PROT_NY 
          icol = BIGCAL_RCS_IX(ihit)
          icell = icol + BIGCAL_RCS_NX*(irow - 1)
          adc_val = BIGCAL_RCS_ADC_RAW(ihit)

c$$$          if(bid_badc(icell+bigcal_prot_maxhits).gt.0) 
c$$$     $         call hf1(bid_badc(icell+bigcal_prot_maxhits),
c$$$     $         float(adc_val),1.0)

c          BIGCAL_ALL_RAW_DET(icell+bigcal_prot_maxhits) = adc_val
          
          bigcal_rcs_nhit_ch(icell) = bigcal_rcs_nhit_ch(icell) + 1

          if(bigcal_rcs_nhit_ch(icell).eq.1) then
             BIGCAL_RCS_RAW_DET(icell) = adc_val
          endif
          if(bigcal_rcs_nhit_ch(icell).gt.1) then
             nbad = nbad + 1
             nbad2 = nbad2 + 1
             if(bigcal_rcs_nhit_ch(icell).eq.2) then ! first bad hit
                bigcal_rcs_iybad(nbad) = irow
                bigcal_rcs_ixbad(nbad) = icol
c     bigcal_prot_raw_det(icell) should still contain the adc value of the first hit
c     in this channel
                bigcal_rcs_adc_bad(nbad) = bigcal_rcs_raw_det(icell)
                nbad = nbad + 1
             endif
             bigcal_rcs_iybad(nbad) = irow
             bigcal_rcs_ixbad(nbad) = icol
             bigcal_rcs_adc_bad(nbad) = adc_val
          endif

          if(adc_val.ge.0) then 
            BIGCAL_RCS_ADC_DECODED(icell) = float(adc_val) - 
     $           BIGCAL_RCS_PED_MEAN(icell)
          endif
c     "sparsify" the data
          if(BIGCAL_RCS_ADC_DECODED(icell).ge.
     $         BIGCAL_RCS_ADC_THRESHOLD(icell)) then
            ngood = ngood + 1
            BIGCAL_RCS_ADC_GOOD(ngood) = BIGCAL_RCS_ADC_DECODED(icell)
            BIGCAL_RCS_IYGOOD(ngood) = irow
            BIGCAL_RCS_IXGOOD(ngood) = icol
          endif
        enddo
      endif
      BIGCAL_RCS_NGOOD = ngood
      bigcal_rcs_nbad = nbad2
      bigcal_rcs_badplusgood = nbad

      return
      end

