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

      common/hcal_calib/ncall
      integer ncall
      data ncall/0/

      real thr_lo,thr_hi        !thresholds on sammed raw calorimeter signal.

      ncall=ncall+1

c     Choose clean single electron tracks within HMS momentum acceptance.
      if(  (hntracks_fp.eq.1).and.
     &     (hnclusters_cal.eq.1).and.
     &     (hntracks_cal.eq.1).and.
     &     (hcer_npe_sum.gt.4).and.
     &     (abs(hdelta_tar(1)).lt.10.).and.
     &     (abs(hbeta(1)-1.).lt.0.05)  ) then
***     &     (hbeta_chisq(1).ge.0.).and.(hbeta_chisq(1).lt.1.)  ) then

         write(hlun_dbg_cal,'(i2,1x,f7.4,2(1x,f5.1,1x,f9.6))')
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

            write(hlun_dbg_cal,'(2(f9.3,1x),i2)'),
     &           adc_pos,adc_neg,nblk

         end do

      end if

      if((ncall/hcal_calib_event)*hcal_calib_event.eq.ncall) then

         close(hlun_dbg_cal)

         print*,'=========================================================='
         print*,'Calibrating HMS Calorimeter at event #',gen_event_id_number

         call hcal_raw_thr(hlun_dbg_cal,thr_lo,thr_hi)
         print*,'lo & hi thresholds:', thr_lo,thr_hi
         call hcal_clb_det(hlun_dbg_cal,gen_run_number,thr_lo,thr_hi)

         print*,'=========================================================='

D         pause

         open(hlun_dbg_cal,access='append')

      end if

      end

*=======================================================================
      include 'hcal_raw_thr.f'
      include 'hcal_clb_det.f'
