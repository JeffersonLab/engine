      subroutine b_trans_PROT(ABORT,err)

      implicit none
      save

      logical ABORT
      character*(*) err
      
      character*12 here
      parameter (here='b_trans_PROT')
      
      integer ihit,irow,icol
      integer icell,irow8,icol8,ig8
      integer igroup64,ihalf64,ig64
      real sum8

      include 'bigcal_data_structures.cmn'
      include 'bigcal_gain_parms.cmn'
      include 'bigcal_geometry.cmn'
      
*
*     start by sparsifying the raw data:       
*
      call b_sparsify_prot(ABORT,err)
      if(ABORT) then
         call g_add_path(here,err)
         return
      endif
      
*
*     for now, only additional step will be to convert the decoded
*     and ped-subtracted data to energies by multiplying with the 
*     calibration constants. Actually, we should also have a database of 
*     x and y positions of cell center relative to calorimeter center
*     and we can fill those arrays here too. What sums should we  
*     accumulate?
*      
*     loop over sparsified "good" hits
      
      do ihit=1,BIGCAL_PROT_NGOOD
         irow = BIGCAL_PROT_IYGOOD(ihit)
         icol = BIGCAL_PROT_IXGOOD(ihit)
         icell = icol + BIGCAL_PROT_NX*(irow - 1)
         BIGCAL_PROT_ECELL(ihit) = BIGCAL_PROT_CFAC(icell) *
     $        BIGCAL_PROT_ADC_GOOD(ihit) * BIGCAL_PROT_GAIN_COR(icell)

         if(bigcal_prot_adc_good(ihit).gt.bigcal_max_adc) then
            bigcal_max_adc = bigcal_prot_adc_good(ihit)
            bigcal_iymax_adc = irow
            bigcal_ixmax_adc = icol
         endif

*     question of whether to group by hits or cells. 
*     seems most logical and efficient to go by hits only and not 
*     have to keep passing around the full array of cells with lots of 
*     empties. 
*     
*     Fill also xgood and ygood arrays
         BIGCAL_PROT_XGOOD(ihit) = BIGCAL_PROT_XCENTER(icell)
         BIGCAL_PROT_YGOOD(ihit) = BIGCAL_PROT_YCENTER(icell)
*     Fill detector arrays and increment sums of 8 and sums of 64:
         BIGCAL_PROT_GOOD_DET(icell) = BIGCAL_PROT_ECELL(ihit)
         BIGCAL_PROT_GOOD_HIT(icell) = .true.
         irow8 = irow
         icol8 = (icol - 1)/8 + 1
         ig8 = icol8 + BIGCAL_MAX_GROUPS * (irow8 - 1)
         BIGCAL_TIME_ADC_SUM(ig8) = BIGCAL_TIME_ADC_SUM(ig8) + 
     $        BIGCAL_PROT_ECELL(ihit)
         igroup64 = (irow - 1) / 3 + 1
         ihalf64 = (icol - 1) / 16 + 1
         ig64 = igroup64 + (ihalf64 - 1)*BIGCAL_LOGIC_GROUPS / 2
         BIGCAL_TRIG_ADC_SUM(ig64) = BIGCAL_TRIG_ADC_SUM(ig64) + 
     $        BIGCAL_PROT_ECELL(ihit)
      enddo

      return
      end
      
