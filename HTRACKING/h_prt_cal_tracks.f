*=======================================================================
      subroutine h_prt_cal_tracks
*=======================================================================

c     A special version of h_prt_cal_tracks dedicated to HMS calorimeter
c     calibration with electrons.

*
      implicit none
*
      include 'hms_data_structures.cmn'
      include 'hms_calorimeter.cmn'
      include 'hms_tracking.cmn'
      include 'gen_run_info.cmn'
      include 'gen_event_info.cmn'
*
      integer ihit
      integer nblk
      real adc_pos,adc_neg

      integer hcal_calib_event  !max.number of events to be calibrated.
      parameter (hcal_calib_event = 50 000)

      integer nct_hit_blk(78),ipmt
      logical write_out
c
      common/hcal_calib/nct_hit_blk,ncall,spare_id
      integer ncall
      data ncall/0/

      real thr_lo,thr_hi        !thresholds on sammed raw calorimeter signal.
c
      integer spare_id
      logical ABORT
      character*80 err
c
      ncall=ncall+1

      if (ncall .eq. 1) then
	call g_IO_control(spare_id,'ANY',ABORT,err)  !get IO channel
         open(spare_id)
         do ipmt=1,78
            nct_hit_blk(ipmt)=0
         enddo
      endif

c     Choose clean single electron tracks within HMS momentum acceptance.
      if(  (hntracks_fp.eq.1).and.
     &     (hnclusters_cal.eq.1).and.
     &     (hntracks_cal.eq.1).and.
     &     (hcer_npe_sum.gt.4).and.
     &     (abs(hdelta_tar(1)).lt.10.).and.
     &     (abs(hbeta(1)-1.).lt.0.05)  .and.
     &     spare_id .ne. 0 ) then
***     &     (hbeta_chisq(1).ge.0.).and.(hbeta_chisq(1).lt.1.)  ) then

c
         write_out = .false.
         do ihit=1,hcal_num_hits
            nblk=(hcal_cols(ihit)-1)*hmax_cal_rows+hcal_rows(ihit)
            nct_hit_blk(nblk) = nct_hit_blk(nblk) + 1
            if (nct_hit_blk(nblk) .lt. 4000) write_out = .true.
         enddo
c
         if (write_out) then
c
         write(spare_id,'(i2,1x,f7.4,2(1x,f5.1,1x,f9.6))')
     &        hcal_num_hits,hp_tar(1),
     &        htrack_xc(1),hxp_fp(1),htrack_yc(1),hyp_fp(1)

         do ihit=1,hcal_num_hits

            if(hcal_cols(ihit).le.hcal_num_neg_columns) then
               adc_neg=hcal_adcs_neg(ihit)
            else
               adc_neg=0.
            end if
            adc_pos=hcal_adcs_pos(ihit)
            nblk=(hcal_cols(ihit)-1)*hmax_cal_rows+hcal_rows(ihit)

            write(spare_id,'(2(f9.3,1x),i2)'),
     &           adc_pos,adc_neg,nblk

         end do
       endif ! if write_out
c
      end if

      if((ncall/hcal_calib_event)*hcal_calib_event.eq.ncall) then

         close(spare_id)


         print*,'=========================================================='
         print*,'Calibrating HMS Calorimeter at event #',gen_event_id_number

         call hcal_raw_thr(spare_id,thr_lo,thr_hi)
         print*,'lo & hi thresholds:', thr_lo,thr_hi
         call hcal_clb_det(spare_id,gen_run_number,thr_lo,thr_hi)

         print*,'=========================================================='

D         pause
           
c         spare_id = 0
         open(spare_id,access='append')

      end if

      end

