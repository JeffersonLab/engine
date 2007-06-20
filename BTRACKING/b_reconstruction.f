      subroutine B_reconstruction(ABORT,err)

********************************************************      
      IMPLICIT NONE
      SAVE
********************************************************
 
      character*16 here
      parameter (here= 'B_reconstruction')
      
      logical ABORT
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
      bigcal_max_adc_final = bigcal_max_adc
      bigcal_iymax_final = bigcal_iymax_adc
      bigcal_ixmax_final = bigcal_ixmax_adc

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
  
*     find_clusters: fills the cluster arrays, calculates sums and moments
      if(bbypass_find_clusters.eq.0.and.bbypass_prot.eq.0.and.
     $     bbypass_rcs.eq.0) then
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



*
      return
      end
