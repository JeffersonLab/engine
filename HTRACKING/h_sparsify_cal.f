*=======================================================================
      subroutine h_sparsify_cal(abort,errmsg)
*=======================================================================
*-
*-      Purpose: Sparsifies the calorimeter raw data.
*-
*-      Input Banks: HMS_RAW_CAL, HMS_PEDESTALS_CAL
*-
*-      Output Bank: HMS_SPARSIFIED_CAL
*-
*-      Created: 15 Mar 1994      Tsolak A. Amatuni
*-      Modified 25 Mar 1994      DFG
*-                                Change name of print routines
*-                5 Apr 1994      DFG Move print routine to h_raw_dump_all
* $Log$
* Revision 1.2  1994/09/13 20:31:08  cdaq
* (JRA) Subtract pedestals in sparsified data
*
* Revision 1.1  1994/04/13  16:21:31  cdaq
* Initial revision
*
*-----------------------------------------------------------------------
*
*
      implicit none
      save
*
      logical abort
      character*(*) errmsg
      character*14 here
      parameter (here='H_SPARSIFY_CAL')
*
      integer*4 nh        !Loop variable for raw hits
      integer*4 nb        !Block number
      integer*4 row,col   !Row & column numbers
      integer*4 adc       !ADC value
      integer*4 adc_max   !Max. channel #
      parameter (adc_max=4095)
      real*4    ped       !Pedestal value
*
      include 'gen_data_structures.cmn'
      include 'hms_calorimeter.cmn'
*
*
*
      errmsg=' '
      abort=hcal_tot_hits.lt.0.or.hcal_tot_hits.gt.hmax_cal_blocks
      if(abort) then
         write(errmsg,*) ':hcal_tot_hits = ',hcal_tot_hits
         call g_prepend(here,errmsg)
         return
      endif
*
      hcal_num_hits=0
      if(hcal_tot_hits.le.0) return
*
*      Loop over raw hits
*
      do nh=1,hcal_tot_hits      
         row=hcal_row(nh)
         col=hcal_column(nh)
         adc=hcal_adc(nh)
*
*------Check the validity of raw data
         abort=row.le.0.or.row.gt.hmax_cal_rows
         if(abort) then
            write(errmsg,*) ':hcal_row(',nh,') = ',row
            call g_prepend(here,errmsg)
            return
         endif
*
         abort=col.le.0.or.col.gt.hmax_cal_columns
         if(abort) then
            write(errmsg,*) ':hcal_column(',nh,') = ',col
            call g_prepend(here,errmsg)
            return
         endif
*
         abort=adc.le.0.or.adc.gt.adc_max
         if(abort) then
            write(errmsg,*) ':hcal_adc(',nh,') = ',adc
            call g_prepend(here,errmsg)
            return
         endif
*
*------Sparsify the raw data
         nb =row+hmax_cal_rows*(col-1)
         ped=hcal_ped_mean(nb)
         hcal_realadc(nh)=float(adc)-ped
         if(hcal_realadc(nh).gt.hcal_threshold(nb)) then
            hcal_num_hits           =hcal_num_hits+1
            hcal_rows(hcal_num_hits)=row
            hcal_cols(hcal_num_hits)=col
            hcal_adcs(hcal_num_hits)=hcal_realadc(nh)
         endif
      enddo                      !End loop over raw hits
*
      if(hdbg_sparsified_cal.gt.0) call h_prt_cal_sparsified
*
      return
      end
