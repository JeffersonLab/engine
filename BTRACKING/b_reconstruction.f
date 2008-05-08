      subroutine B_reconstruction(ABORT,err)

********************************************************      
      IMPLICIT NONE
      SAVE
********************************************************
 
      character*16 here
      parameter (here= 'B_reconstruction')
      
c      logical last_time
      logical ABORT
      logical mc_trig  ! check if at least one trig. sum is above b_cluster_cut
      integer isum64
      integer ngood64
      integer revert(5)
      integer imax64,rowmax64,colmax64
      character*(*) err

      include 'bigcal_data_structures.cmn'
      include 'bigcal_bypass_switches.cmn'
      include 'bigcal_gain_parms.cmn'
      include 'bigcal_geometry.cmn'
      include 'bigcal_filenames.cmn'
      include 'bigcal_tof_parms.cmn'
      include 'hms_data_structures.cmn'
      include 'gen_constants.par'
      include 'gen_units.par'
      include 'gen_scalers.cmn'
      include 'gen_run_info.cmn'
      include 'gen_event_info.cmn'

      revert(1) = b_use_bad_chan_list
      revert(2) = bbypass_find_clusters
      revert(3) = bbypass_calc_cluster_time
      revert(4) = bbypass_calc_shower_coord
      revert(5) = bbypass_calc_physics

c      last_time = .false.

c     for certain event types, disable parts of the analysis:
c     don't add cells in the bad channel list to clusters for type 5
c     (bigcal singles) events. We only want to use the bad channel list 
c     to improve the efficiency of coincidence events.
      if(gen_event_type.eq.5) then
         b_use_bad_chan_list = 0
c         last_time = .true.
      endif
      if(gen_event_type.gt.6) then
         bbypass_find_clusters = 1
         bbypass_calc_cluster_time = 1
         bbypass_calc_shower_coord = 1
         bbypass_calc_physics = 1
      endif
        
************  dump raw data ****************************
      call b_raw_dump_all(ABORT,err)
      if(ABORT) then
         call g_add_path(here,err)
         return
      endif

      !bigcal_max_adc = 0.
      !bigcal_iymax_adc = 0
      !bigcal_ixmax_adc = 0
************  convert Protvino raw ADC to Protvino decoded ADC *******
      if(bbypass_prot.eq.0) then
         call b_trans_PROT(ABORT,err)
         if(ABORT) then
            call g_add_path(here,err)
            return
         endif
      endif
************  convert RCS raw ADC to RCS decoded ADC ****
      if(bbypass_rcs.eq.0) then 
         call b_trans_RCS(ABORT,err)
         if(ABORT) then 
            call g_add_path(here,err)
            return
         endif
      endif

      if(bigcal_iymax_adc.gt.0.and.bigcal_ixmax_adc.gt.0) then
         rowmax64 = (bigcal_iymax_adc-1)/3 + 1
         if(bigcal_iymax_adc.le.32) then
            colmax64 = (bigcal_ixmax_adc-1)/16 + 1
         else
            colmax64 = bigcal_ixmax_adc/16 + 1
         endif

         imax64 = colmax64 + 2*(rowmax64-1)

         if(mod(bigcal_iymax_adc-1,3).eq.0.and.bigcal_iymax_adc.gt.1) 
     $        then              ! overlap row,
c     take group with bigger sum:
            if(bigcal_atrig_sum64(imax64-2).gt.bigcal_atrig_sum64(imax64)) then
               imax64 = imax64 - 2
            endif
         endif
         bigcal_itrigmax_adc = imax64
      endif
      bigcal_all_ngood = bigcal_prot_ngood + bigcal_rcs_ngood

***********  convert BigCal group-of-8 raw TDC to go8 decoded TDC *********
      if(bbypass_sum8.eq.0) then
         call b_trans_tdc(ABORT,err)
         if(ABORT) then 
            call g_add_path(here,err)
            return
         endif
      endif
***********  convert BigCal raw trigger signals to decoded *****************
      if(bbypass_sum64.eq.0) then 
         call b_trans_trig(ABORT,err)
         if(ABORT) then
            call g_add_path(here,err)
            return
         endif
      endif

      if(bdebug_print_bad.ne.0) then
         call b_print_raw_bad(abort,err)
      endif
  
*     special check for monte carlo event analysis: check trigger sums:
      if(gen_bigcal_mc.ne.0) then
         mc_trig = .false.

         ngood64 = 0
*     fill hit array (special for monte carlo)
         do isum64=1,bigcal_atrig_maxhits
c$$$         write(*,*) 'igroup,ihalf,sum64 = ',(isum64+1)/2,mod(isum64,2)+1
c$$$     $        ,bigcal_atrig_sum64(isum64)
            if(bigcal_atrig_sum64(isum64).ge.b_trig_cut) then 
               mc_trig=.true.
               ngood64 = ngood64 + 1
               bigcal_atrig_esum(ngood64) = bigcal_atrig_sum64(isum64)
               bigcal_atrig_good_igroup(ngood64) = (isum64-1)/2 + 1
               bigcal_atrig_good_ihalf(ngood64) = mod(isum64-1,2) + 1
            endif
         enddo
         
         bigcal_atrig_ngood = ngood64

         if(.not.mc_trig) return
      endif

c$$$      do isum64=1,bigcal_atrig_maxhits
c$$$c$$$         write(*,*) 'igroup,ihalf,sum64 = ',(isum64+1)/2,mod(isum64,2)+1
c$$$c$$$     $        ,bigcal_atrig_sum64(isum64)
c$$$         if(bigcal_atrig_sum64(isum64).ge.b_cluster_cut) then 
c$$$            mc_trig=.true.
c$$$            
c$$$         endif
c$$$      enddo

c$$$      if(gen_bigcal_mc.ne.0.and. .not.mc_trig) return

*     find_clusters: fills the cluster arrays, calculates sums and moments
      if(bbypass_find_clusters.eq.0.and.bbypass_prot.eq.0.and.
     $     bbypass_rcs.eq.0) then

         !write(*,*) 'entering b_fill_bigcal_arrays'

c$$$         call b_fill_bigcal_arrays(abort,err)
c$$$         if(ABORT) then 
c$$$            call g_add_path(here,err)
c$$$            return
c$$$         endif

c         write(*,*) 'entering b_find_clusters'

         call b_find_clusters(bigcal_all_nclstr,bigcal_nmaxima,ABORT,err)
         if(ABORT) then 
            call g_add_path(here,err)
            return
         endif
      endif
*     check cluster timing: looks at all the hits in a cluster and calculates 
*     the average time associated with sum8 tdcs and sum64 tdcs
      if(bbypass_sum8.eq.0.and.bbypass_sum64.eq.0.and.
     $     bbypass_calc_cluster_time.eq.0.and.bbypass_find_clusters
     $     .eq.0) then
         call b_calc_cluster_time(ABORT,err)
         if(ABORT) then
            call g_add_path(here,err)
            return
         endif
      endif
*     calculate shower coordinates and energy from ADC information
      if(bbypass_calc_shower_coord.eq.0.and.bbypass_prot.eq.0.and.
     $     bbypass_rcs.eq.0.and.bbypass_find_clusters.eq.0) then
         call b_calc_shower_coord(ABORT,err)
         if(ABORT) then 
            call g_add_path(here,err)
            return
         endif
      endif
*
      bigcal_all_nclust_good = bigcal_all_nclstr

      if(bbypass_prune_clusters.eq.0.and.bbypass_find_clusters.eq.0)then
         call b_prune_clusters(ABORT,err)
         if(abort) then
            call g_add_path(here,err)
            return
         endif
      endif
*     
      if(bbypass_calc_physics.eq.0.and.bbypass_find_clusters.eq.0) then
         
         if(gen_event_type.ne.6.or.(gen_event_type.eq.6.and.
     $        hsnum_fptrack.eq.0)) call b_calc_physics(ABORT,err)
         if(ABORT) then
            call g_add_path(here,err)
            return
         endif
      endif

c     if dealing with real data and not monte carlo, then the appropriate
c     place to do the calibration matrix is in gep_reconstruction, because
c     we need to know the incident electron energy, hence we need the hms
c     info.
      if(gen_bigcal_mc.ne.0.and.bigcal_do_calibration.ne.0.and.
     $     gen_bigcal_mc.ne.3) then
         call b_matrix_accum(abort,err)
         if(abort) then
            call g_add_path(here,err)
            return
         endif
      endif
      
      if(gen_event_type.eq.5) then
         b_use_bad_chan_list = revert(1)
      endif
      if(gen_event_type.gt.6) then
         bbypass_find_clusters = revert(2)
         bbypass_calc_cluster_time = revert(3)
         bbypass_calc_shower_coord = revert(4)
         bbypass_calc_physics = revert(5)
      endif
      !write(*,*) 'done with reconstruction'      
*
      return
      end
