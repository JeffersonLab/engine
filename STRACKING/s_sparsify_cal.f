*=======================================================================
      subroutine s_sparsify_cal(abort,errmsg)
*=======================================================================
*-
*-      Purpose: Sparsifies the calorimeter raw data.
*-
*-      Input Banks: SOS_RAW_CAL, SOS_PEDESTALS_CAL
*-
*-      Output Bank: SOS_SPARSIFIED_CAL
*-
*-      Created: 15 Mar 1994      Tsolak A. Amatuni
*-      Modified 25 Mar 1994      DFG
*-                                Change name of print routines
*-                5 Apr 1994      DFG Move print routine to s_raw_dump_all
* $Log$
* Revision 1.3  1995/05/11 14:55:09  cdaq
* (JRA) Add call to s_fill_cal_hist
*
* Revision 1.2  1994/11/23  14:01:04  cdaq
* (SPB) Recopied from hms file and modified names for SOS
*
* Revision 1.1  1994/04/13  18:44:11  cdaq
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
      parameter (here='S_SPARSIFY_CAL')
*
      integer*4 nh        !Loop variable for raw hits
      integer*4 nb        !Block number
      integer*4 row,col   !Row & column numbers
      integer*4 adc       !ADC value
      integer*4 adc_max   !Max. channel #
      parameter (adc_max=4095)

      include 'gen_data_structures.cmn'
      include 'sos_calorimeter.cmn'
*
*
*
      errmsg=' '
      abort=scal_tot_hits.lt.0.or.scal_tot_hits.gt.smax_cal_blocks
      if(abort) then
         write(errmsg,*) ':scal_tot_hits = ',scal_tot_hits
         call g_prepend(here,errmsg)
         return
      endif
*
      scal_num_hits=0
      if(scal_tot_hits.le.0) return
*
*      Loop over raw hits
*
      do nh=1,scal_tot_hits      
         row=scal_row(nh)
         col=scal_column(nh)
         adc=scal_adc(nh)
*
*------Check the validity of raw data
         abort=row.le.0.or.row.gt.smax_cal_rows
         if(abort) then
            write(errmsg,*) ':scal_row(',nh,') = ',row
            call g_prepend(here,errmsg)
            return
         endif
*
         abort=col.le.0.or.col.gt.smax_cal_columns
         if(abort) then
            write(errmsg,*) ':scal_column(',nh,') = ',col
            call g_prepend(here,errmsg)
            return
         endif
*
         abort=adc.le.0.or.adc.gt.adc_max
         if(abort) then
            write(errmsg,*) ':scal_adc(',nh,') = ',adc
            call g_prepend(here,errmsg)
            return
         endif
*
*------Sparsify the raw data
         nb =row+smax_cal_rows*(col-1)

         scal_realadc(nh)=float(adc)-scal_ped_mean(nb)
         if(scal_realadc(nh).gt.scal_threshold(nb)) then
            scal_num_hits           =scal_num_hits+1
            scal_rows(scal_num_hits)=row
            scal_cols(scal_num_hits)=col
            scal_adcs(scal_num_hits)=scal_realadc(nh)
         endif
      enddo                      !End loop over raw hits
*
      if(sdbg_sparsified_cal.gt.0) call s_prt_cal_sparsified
*
      call s_fill_cal_hist(abort,errmsg)
*
      return
      end
