      subroutine b_calc_pedestal(ABORT,err)

      implicit none
      save

      character*15 here
      parameter(here='b_calc_pedestal')

      logical ABORT
      character*(*) err

      include 'bigcal_data_structures.cmn'
      include 'bigcal_hist_id.cmn'
      include 'bigcal_gain_parms.cmn'
      include 'bigcal_filenames.cmn'
      include 'gen_run_info.cmn'

      integer spareid
      parameter(spareid=67)

      integer irow,icol,icell
      integer igroup,ihalf,igr64

      integer nchange
      integer roc,slot,signalcount

      real numped,sigma2

      character*132 file

      nchange = 0

      do irow=1,BIGCAL_PROT_NY
         do icol=1,BIGCAL_PROT_NX
            icell = icol + (irow-1)*BIGCAL_PROT_NX
            numped = max(1.,float(bigcal_prot_ped_num(icell)))
c            write(*,*) 'prot numped=',numped
            
            bigcal_prot_new_ped(icell)=bigcal_prot_ped_sum(icell)/numped
            sigma2=float(bigcal_prot_ped_sum2(icell))/numped - 
     $           (bigcal_prot_new_ped(icell))**2

            bigcal_prot_new_rms(icell) = sqrt(max(0.,sigma2))

            bigcal_prot_new_threshold(icell)=bigcal_prot_new_ped(icell)
     $           + bigcal_prot_nsparse
            if(bigcal_prot_new_threshold(icell).lt.0) then
               bigcal_prot_new_threshold(icell) = 0
            endif
            if(bigcal_prot_new_threshold(icell).gt.4000) then
               bigcal_prot_new_threshold(icell) = 4000
            endif

            if(abs(bigcal_prot_ped_mean(icell) - 
     $           bigcal_prot_new_ped(icell)).gt. 2.0 *
     $           bigcal_prot_new_rms(icell)) then
               nchange = nchange + 1
               bigcal_prot_change_irow(nchange) = irow
               bigcal_prot_change_icol(nchange) = icol
               bigcal_prot_ped_change(nchange) = 
     $              bigcal_prot_new_ped(icell) - 
     $              bigcal_prot_ped_mean(icell)
            endif

            if(numped.gt.bigcal_prot_min_peds.and.bigcal_prot_min_peds
     $           .ne.0) then
               bigcal_prot_ped_mean(icell)=bigcal_prot_new_ped(icell)
               bigcal_prot_ped_rms(icell)=bigcal_prot_new_rms(icell)
               bigcal_prot_adc_threshold(icell)=min(bigcal_prot_max_thresh,
     $              max(bigcal_prot_min_thresh,2.5*bigcal_prot_new_rms(icell)))

               if(bid_bcal_ped_mean_prot.gt.0) then
                  call hf1(bid_bcal_ped_mean_prot,float(icell),
     $                 bigcal_prot_ped_mean(icell))
               endif
               
               if(bid_bcal_ped_rms_prot.gt.0) then
                  call hf1(bid_bcal_ped_rms_prot,float(icell),
     $                 bigcal_prot_ped_rms(icell))
               endif

               if(bid_bcal_pedw_prot.gt.0) then
                  call hf1(bid_bcal_pedw_prot,bigcal_prot_ped_rms(icell),1.)
               endif

            endif
         enddo
      enddo

      bigcal_prot_num_ped_changes = nchange

      nchange = 0

      do irow=1,BIGCAL_RCS_NY
         do icol=1,BIGCAL_RCS_NX
            icell = icol + (irow-1)*BIGCAL_RCS_NX
            numped = max(1.,float(bigcal_rcs_ped_num(icell)))
            
c            write(*,*) 'rcs numped=',numped

            bigcal_rcs_new_ped(icell)=bigcal_rcs_ped_sum(icell)/numped
            sigma2=float(bigcal_rcs_ped_sum2(icell))/numped - 
     $           (bigcal_rcs_new_ped(icell))**2

            bigcal_rcs_new_rms(icell) = sqrt(max(0.,sigma2))

            bigcal_rcs_new_threshold(icell)=bigcal_rcs_new_ped(icell)
     $           + bigcal_rcs_nsparse
            if(bigcal_rcs_new_threshold(icell).lt.0) then
               bigcal_rcs_new_threshold(icell) = 0
            endif
            if(bigcal_rcs_new_threshold(icell).gt.4000) then
               bigcal_rcs_new_threshold(icell) = 4000
            endif
            if(abs(bigcal_rcs_ped_mean(icell) - 
     $           bigcal_rcs_new_ped(icell)).gt. 2.0 *
     $           bigcal_rcs_new_rms(icell)) then
               nchange = nchange + 1
               bigcal_rcs_change_irow(nchange) = irow
               bigcal_rcs_change_icol(nchange) = icol
               bigcal_rcs_ped_change(nchange) = 
     $              bigcal_rcs_new_ped(icell) - 
     $              bigcal_rcs_ped_mean(icell)
            endif

            if(numped.gt.bigcal_rcs_min_peds.and.bigcal_rcs_min_peds
     $           .ne.0) then
               bigcal_rcs_ped_mean(icell)=bigcal_rcs_new_ped(icell)
               bigcal_rcs_ped_rms(icell)=bigcal_rcs_new_rms(icell)
               bigcal_rcs_adc_threshold(icell)=min(bigcal_rcs_max_thresh,
     $              max(bigcal_rcs_min_thresh,2.5*bigcal_rcs_new_rms(icell)))
               if(bid_bcal_ped_mean_rcs.gt.0) then
                  call hf1(bid_bcal_ped_mean_rcs,float(icell),
     $                 bigcal_rcs_ped_mean(icell))
               endif
               
               if(bid_bcal_ped_rms_rcs.gt.0) then
                  call hf1(bid_bcal_ped_rms_rcs,float(icell),
     $                 bigcal_rcs_ped_rms(icell))
               endif
               
               if(bid_bcal_pedw_rcs.gt.0) then
                  call hf1(bid_bcal_pedw_rcs,bigcal_rcs_ped_rms(icell),1.)
               endif
               
            endif
         enddo
      enddo

      bigcal_rcs_num_ped_changes = nchange

      nchange = 0

      do igroup=1,BIGCAL_ATRIG_MAXHITS/2
         do ihalf=1,2
            igr64 = ihalf + 2*(igroup-1)
            numped = max(1.,float(bigcal_trig_ped_num(igr64)))
            
c            write(*,*) 'trig numped=',numped

            bigcal_trig_new_ped(igr64)=bigcal_trig_ped_sum(igr64)/numped
            sigma2=float(bigcal_trig_ped_sum2(igr64))/numped - 
     $           (bigcal_trig_new_ped(igr64))**2

            bigcal_trig_new_rms(igr64) = sqrt(max(0.,sigma2))

            bigcal_trig_new_threshold(igr64)=bigcal_trig_new_ped(igr64)
     $           + bigcal_trig_nsparse
            if(bigcal_trig_new_threshold(igr64).lt.0) then
               bigcal_trig_new_threshold(igr64) = 0
            endif
            if(bigcal_trig_new_threshold(igr64).gt.4000) then
               bigcal_trig_new_threshold(igr64) = 4000
            endif
            if(abs(bigcal_trig_ped_mean(igr64) - 
     $           bigcal_trig_new_ped(igr64)).gt. 2.0 *
     $           bigcal_trig_new_rms(igr64)) then
               nchange = nchange + 1
               bigcal_trig_change_irow(nchange) = irow
               bigcal_trig_change_icol(nchange) = icol
               bigcal_trig_ped_change(nchange) = 
     $              bigcal_trig_new_ped(igr64) - 
     $              bigcal_trig_ped_mean(igr64)
            endif

            if(numped.gt.bigcal_trig_min_peds.and.bigcal_trig_min_peds
     $           .ne.0) then
               bigcal_trig_ped_mean(igr64)=bigcal_trig_new_ped(igr64)
               bigcal_trig_ped_rms(igr64)=bigcal_trig_new_rms(igr64)
               bigcal_trig_adc_threshold(igr64)=min(bigcal_trig_max_thresh,
     $              max(bigcal_trig_min_thresh,3.*bigcal_trig_new_rms(igr64)))
               if(bid_bcal_ped_mean_trig.gt.0) then
                  call hf1(bid_bcal_ped_mean_trig,float(igr64),
     $                 bigcal_trig_ped_mean(igr64))
               endif
               
               if(bid_bcal_ped_rms_trig.gt.0) then
                  call hf1(bid_bcal_ped_rms_trig,float(igr64),
     $                 bigcal_trig_ped_rms(igr64))
               endif

               if(bid_bcal_pedw_trig.gt.0) then
                  call hf1(bid_bcal_pedw_trig,bigcal_trig_ped_rms(igr64),1.)
               endif

            endif
         enddo
      enddo

      bigcal_trig_num_ped_changes = nchange

c     now we write thresholds to file for hardware sparsification:

      if(b_roc11_threshold_output_filename.ne.' ') then
         file = b_roc11_threshold_output_filename
         call g_sub_run_number(file,gen_run_number)
         open(unit=SPAREID,file=file,status='unknown')

c$$$         write(SPAREID,*) '# This is the ADC threshold file generated '// 
c$$$     $       'automatically'
c$$$         write(SPAREID,666)'# from the pedestal data, run ',gen_run_number
c$$$         write(SPAREID,*) '# ROC11 (BigCal Protvino and trigger ADCs):'
c$$$ 666     format(A31,I8)
         roc=11
c     protvino ADCs are NO LONGER in ROC11, slots 3-10 and 14-21
c     protvino ADCs are now in ROC11, slots 3-19

         signalcount=1
c     change slot range to 3-20 since now we have several cables in slot 20
         do slot=3,18
            write(spareid,*) 'slot=',slot
            call g_output_thresholds(spareid,roc,slot,signalcount,
     $           BIGCAL_PROT_NX,bigcal_prot_new_threshold,0,
     $           bigcal_prot_new_rms,0)
         enddo

         slot=19
         write(spareid,*) 'slot=',slot
         call g_output_thresholds(spareid,roc,slot,signalcount,
     $        2,bigcal_trig_new_threshold,0,
     $        bigcal_trig_new_rms,0)
         

         slot=20
         write(spareid,*) 'slot=',slot
         call g_output_thresholds(spareid,roc,slot,signalcount,
     $        bigcal_prot_nx,bigcal_prot_new_threshold,0,
     $        bigcal_prot_new_rms,0)


         close(spareid)
      endif
            
c$$$         do slot=14,21
c$$$            write(spareid,*) 'slot=',slot
c$$$            call g_output_thresholds(spareid,roc,slot,signalcount,
c$$$     $           BIGCAL_PROT_NX,bigcal_prot_new_threshold,0,
c$$$     $           bigcal_prot_new_rms,0)
c$$$         enddo
c     trigger ADCs are in roc 11, slot 22
c$$$         slot=22
c$$$         write(spareid,*) 'slot=',slot
c$$$         call g_output_thresholds(spareid,roc,slot,signalcount,
c$$$     $        2,bigcal_trig_new_threshold,0,
c$$$     $        bigcal_trig_new_rms,0)

c     rcs ADCs are in ROC12, slots 6-11 and 15-20

c$$$         write(SPAREID,*) '# ROC12 (BigCal RCS ADCs):'

      if(b_roc12_threshold_output_filename.ne.' ') then
         file=b_roc12_threshold_output_filename
         call g_sub_run_number(file,gen_run_number)
         open(unit=SPAREID,file=file,status='unknown')
         roc=12
         do slot=6,11
            write(spareid,*) 'slot=',slot
            call g_output_thresholds(spareid,roc,slot,signalcount,
     $           BIGCAL_RCS_NX,bigcal_rcs_new_threshold,0,
     $           bigcal_rcs_new_rms,0)
         enddo
         
         do slot=15,20
            write(spareid,*) 'slot=',slot
            call g_output_thresholds(spareid,roc,slot,signalcount,
     $           BIGCAL_RCS_NX,bigcal_rcs_new_threshold,0,
     $           bigcal_rcs_new_rms,0)
         enddo

         close(spareid)

      endif

      return
      end
