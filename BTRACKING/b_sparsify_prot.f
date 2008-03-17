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
      integer*4 nhit29and30
      integer*4 nped29and30
      real*4 sumped29and30
      real*4 sum2ped29and30
      integer*4 igood29and30(64)
      real*4 adc29and30(64)
      real*4 sum_29and30
      real*4 sum2_29and30
*     check number of hits:
      include 'bigcal_data_structures.cmn'
      include 'bigcal_gain_parms.cmn'
      include 'bigcal_bypass_switches.cmn'
      include 'gen_event_info.cmn'

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

      nhit29and30 = 0
      sum_29and30 = 0.
      sum2_29and30 = 0.

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

          if(b_fix_double_ped.ne.0.and.(irow.eq.29.or.irow.eq.30) ) then ! count raw hits and compute the mean and rms
             nhit29and30 = nhit29and30 + 1
             sum_29and30 = sum_29and30 + float(adc_val) - bigcal_prot_ped_mean(icell)
             sum2_29and30 = sum2_29and30 + (float(adc_val) - bigcal_prot_ped_mean(icell))**2
          endif

        enddo
      endif

      if(b_fix_double_ped.ne.0) then 
         
         if(nhit29and30.gt.60) then ! this is a jump that we can detect
            b_mean_ped_jump = sum_29and30 / float(nhit29and30)
            b_rms_ped_jump = sqrt(max(0.,sum2_29and30 / float(nhit29and30) 
     $           - b_mean_ped_jump**2))
            
            if(abs(b_mean_ped_jump).gt.b_ped_jump_threshold) then ! reset the pedestal counter

               write(bluno,*) 'detected pedestal shift event num=',gen_event_id_number
               b_ped_jump_njump_run = b_ped_jump_njump_run + 1
               write(bluno,*) 'njump=',b_ped_jump_njump_run
               write(bluno,*) 'mean,rms jump=',b_mean_ped_jump,b_rms_ped_jump
               do icell=1,64
                  b_ped_jump_nped(icell) = 0
                  b_ped_jump_sum(icell) = 0
                  b_ped_jump_sum2(icell) = 0
                  bigcal_prot_ped_mean(icell+28*32) = bigcal_prot_ped_mean(icell+28*32) + 
     $                 b_mean_ped_jump
                  bigcal_prot_ped_rms(icell+28*32) = b_rms_ped_jump
                  bigcal_prot_adc_threshold(icell+28*32) = min(bigcal_prot_max_thresh,
     $                 max(bigcal_prot_min_thresh,2.5*bigcal_prot_ped_rms(icell)))
                  bigcal_prot_ped_limit(icell+28*32) = 1500
               enddo
               b_ped_jump_in_progress = .true.
               
               if(b_mean_ped_jump.lt.0.) then ! go back and add the hits to the good hits array
                  do ihit=1,bigcal_prot_nhit
                     irow = bigcal_prot_iy(ihit)
                     icol = bigcal_prot_ix(ihit)
                     icell = icol + 32*(irow-1)
                     adc_val = bigcal_prot_adc_raw(ihit)
                     if(irow.eq.29.or.irow.eq.30) then
                        if(float(adc_val)-bigcal_prot_ped_mean(icell)-b_mean_ped_jump
     $                       .ge.bigcal_prot_adc_threshold(icell)) then
                           ngood = ngood + 1
                           bigcal_prot_adc_good(ngood) = float(adc_val) 
     $                          - bigcal_prot_ped_mean(icell)
                           bigcal_prot_iygood(ngood) = irow
                           bigcal_prot_ixgood(ngood) = icol
                        endif
                     endif
                  enddo
               else             ! go through the good hits array and subtract the new pedestal
                  do ihit=1,ngood
                     irow = bigcal_prot_iygood(ihit)
                     icol = bigcal_prot_ixgood(ihit)
                     icell = icol + 32*(irow-1)
                     if(irow.eq.29.or.irow.eq.30) then
                        bigcal_prot_adc_good(ihit) = bigcal_prot_adc_good(ihit)-b_mean_ped_jump
                        if(bigcal_prot_adc_good(ihit).lt.bigcal_prot_adc_threshold(icell))
     $                       then
                           bigcal_prot_adc_good(ihit) = 0.
                        endif
                     endif
                  enddo
               endif
            endif
         endif
         
         if(b_ped_jump_in_progress) then
            do ihit=1,bigcal_prot_nhit
               irow = bigcal_prot_iy(ihit)
               icol = bigcal_prot_ix(ihit)
               icell = icol + 32*(irow-1)
               adc_val = bigcal_prot_adc_raw(ihit)
               
               if(irow.eq.29.or.irow.eq.30) then
                  icell = icell - 28*32
                  
                  if(adc_val.le.bigcal_prot_ped_limit(icell+28*32)) then
                     b_ped_jump_nped(icell) = b_ped_jump_nped(icell) + 1
                     b_ped_jump_sum(icell) = b_ped_jump_sum(icell) + adc_val
                     b_ped_jump_sum2(icell) = b_ped_jump_sum2(icell) + adc_val**2
                     if(float(b_ped_jump_nped(icell)).eq.nint(float(b_ped_jump_min_peds)/5.)) then
                        bigcal_prot_ped_limit(icell+28*32) = 100 + 
     $                       nint(float(b_ped_jump_sum(icell)) / float(b_ped_jump_nped(icell)))
                     endif
                  endif
               endif
            enddo
            
            b_ped_jump_in_progress = .false.
            
            do icell=1,64
               if(b_ped_jump_nped(icell).lt.b_ped_jump_min_peds) then
                  b_ped_jump_in_progress = .true.
               else             ! calculate new pedestal
                  
                  b_ped_jump_new_mean(icell) = float(b_ped_jump_sum(icell)) / 
     $                 float(b_ped_jump_nped(icell)) 
                  b_ped_jump_new_rms(icell) = sqrt(float(b_ped_jump_sum2(icell)) / 
     $                 float(b_ped_jump_nped(icell)) - (b_ped_jump_new_mean(icell))**2)
                  bigcal_prot_ped_mean(icell+28*32) = b_ped_jump_new_mean(icell)
                  bigcal_prot_ped_rms(icell+28*32) = b_ped_jump_new_rms(icell)
                  bigcal_prot_adc_threshold(icell+28*32) = min(bigcal_prot_max_thresh,
     $                 max(bigcal_prot_min_thresh,2.5*bigcal_prot_ped_rms(icell)))
c$$$                  write(bluno,*) 'calculated new pedestal,icell=',icell,
c$$$     $                 'new ped,rms,threshold=',bigcal_prot_ped_mean(icell+28*32),
c$$$     $                 bigcal_prot_ped_rms(icell+28*32),
c$$$     $                 bigcal_prot_adc_threshold(icell+28*32)
                  
c                  b_ped_jump_nped(icell) = 0
c                  b_ped_jump_sum(icell) = 0
c                  b_ped_jump_sum2(icell) = 0
                  bigcal_prot_ped_limit(icell+28*32) = 1500

               endif
            enddo

            if(.not.b_ped_jump_in_progress) then
               write(bluno,*) 'NEW PEDESTALS for evt. number=',gen_event_id_number
               do icell=1,64
                  write(bluno,*) 'calculated new pedestal,icell=',icell,
     $                 'new ped,rms,threshold=',bigcal_prot_ped_mean(icell+28*32),
     $                 bigcal_prot_ped_rms(icell+28*32),
     $                 bigcal_prot_adc_threshold(icell+28*32)
               enddo
            endif
         endif


      endif

      BIGCAL_PROT_NGOOD = ngood
      bigcal_prot_nbad = nbad2
      bigcal_prot_badplusgood = nbad

      return
      end
