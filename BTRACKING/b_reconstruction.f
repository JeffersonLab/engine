      subroutine B_reconstruction(ABORT,err)

********************************************************      
      IMPLICIT NONE
      SAVE
********************************************************
 
      character*16 here
      parameter (here= 'B_reconstruction')
      
      logical ABORT
      logical mc_trig  ! check if at least one trig. sum is above b_cluster_cut
      integer isum64
      integer ngood64
      character*(*) err

      include 'bigcal_data_structures.cmn'
      include 'bigcal_bypass_switches.cmn'
      include 'bigcal_gain_parms.cmn'
      include 'bigcal_geometry.cmn'
      include 'bigcal_filenames.cmn'
      include 'bigcal_tof_parms.cmn'
      include 'gen_constants.par'
      include 'gen_units.par'
      include 'gen_scalers.cmn'
      include 'gen_run_info.cmn'

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
  
*     special check for monte carlo event analysis: check trigger sums:
      if(gen_bigcal_mc.ne.0) then
         mc_trig = .false.

         ngood64 = 0
*     fill hit array (special for monte carlo)
         do isum64=1,bigcal_atrig_maxhits
c$$$         write(*,*) 'igroup,ihalf,sum64 = ',(isum64+1)/2,mod(isum64,2)+1
c$$$     $        ,bigcal_atrig_sum64(isum64)
            if(bigcal_atrig_sum64(isum64).ge.b_cluster_cut) then 
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

         !write(*,*) 'entering b_find_clusters'

         call b_find_clusters(ABORT,err)
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
      if(bbypass_calc_physics.eq.0.and.bbypass_find_clusters.eq.0) then
         call b_calc_physics(ABORT,err)
         if(ABORT) then
            call g_add_path(here,err)
            return
         endif
      endif

      !write(*,*) 'done with reconstruction'

*
      return
      end
