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
      
***********  What are the analysis steps? What physics variables do we need
***********  to reconstruct? First do hit variables: timings, 
***********      1. Trigger time/amplitude 
***********      2. Logic group (sum 64) hits and times/amplitudes
***********      3. TDC hit times
***********      4. Cluster finding and moments (also, associate an average 
***********             time with each cluster, average of all associated 
***********             TDC groups)
***********      5. "Good" clusters: Cut on time difference between average 
***********         TDC and trigger time, cut on requiring at least one logic 
***********         group with hit containing the cells in this cluster
***********  Reconstructed physics variables: 
***********      1. "Good" clusters should be ep coin. High background rate, 
***********         ~1 MHz or so, take prescaled singles at about 10-100 Hz 
***********         or so. Trig rate is about 600 Hz per glass bar, should
***********         be able to measure pile-up of signals.
***********      2. Time of cluster as determined by best average of overall
***********         trigger time, TDCs of groups containing the cluster cells,
***********         and TDCs of logic groups containing the cluster cells.
***********      3. x and y coordinates. Assuming elastic electrons, use the 
***********         fit parameters specific to the row and column of the 
***********         central cell of the cluster (5x5)
***********      4. Energy. This will depend on calibrating the calo with ep
***********         elastic, and we will have to determine the calibration 
***********         "constants". Want to be able to interface to database for
***********         this purpose. x and y coordinates and energy are most 
***********         important
***********  Higher-level physics variables (requires HMS info): 
***********      1. Because no spectrometer, we cannot correct for the extended
***********         target unless we have some HMS vertex information as well
***********      2. electron angles theta and phi 
***********      3. missing energy and momentum (requires HMS)
***********      4. Coincidence time (requires HMS)
***********  Cut on missing energy and momentum and coincidence timing 
***********  suppresses most accidentals. 
  
*     find_clusters: fills the cluster arrays, calculates sums and moments
      if(bbypass_find_clusters.eq.0.and.bbypass_prot.eq.0.and.
     $     bbypass_rcs.eq.0) then
         call b_find_clusters(ABORT,err)
         if(ABORT) then 
            call g_add_path(here,err)
            return
         endif
      endif
*     check cluster timing: looks at all the hits in a cluster and checks 
*     timing for consistency, throw out bad hits if there are any, and 
*     calculate average cluster time.
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
      if(bbypass_calc_physics.eq.0.and.bbypass_find_clusters.eq.0.and.
     $     bbypass_calc_cluster_time.eq.0) then
         call b_calc_physics(ABORT,err)
         if(ABORT) then
            call g_add_path(here,err)
            return
         endif
      endif



*
      return
      end
