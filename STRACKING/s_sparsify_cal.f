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
* Revision 1.7  1996/01/17 18:58:19  cdaq
* (JRA) Only histogram ADC's that are not 200 above pedestal
*
* Revision 1.6  1995/08/31 18:08:04  cdaq
* (JRA) Add a hist of all adc's into one spectrum
*
* Revision 1.5  1995/07/20  19:04:20  cdaq
* (JRA) Fix typo's, init scal_realadc array
*
* Revision 1.4  1995/05/22  19:45:56  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.3  1995/05/11  14:55:09  cdaq
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

      include 'sos_data_structures.cmn'
      include 'sos_calorimeter.cmn'
      include 'sos_id_histid.cmn'
*
*
*
      errmsg=' '

      if(scal_tot_hits.lt.0.or.scal_tot_hits.gt.smax_cal_blocks) then
         write(6,*) here,':scal_tot_hits = ',scal_tot_hits
         return
      endif
*
      scal_num_hits=0
      do nb = 1 , smax_cal_blocks
        scal_realadc(nb)=-100
      enddo
*
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
c         abort=row.le.0.or.row.gt.smax_cal_rows
c         if(abort) then
c            write(errmsg,*) ':scal_row(',nh,') = ',row
c            call g_prepend(here,errmsg)
c            return
c         endif
*
c         abort=col.le.0.or.col.gt.smax_cal_columns
c         if(abort) then
c            write(errmsg,*) ':scal_column(',nh,') = ',col
c            call g_prepend(here,errmsg)
c            return
c         endif
*
c         abort=adc.le.0.or.adc.gt.adc_max
c         if(abort) then
c            write(errmsg,*) ':scal_adc(',nh,') = ',adc
c            call g_prepend(here,errmsg)
c            return
c         endif
*
*------Sparsify the raw data
         nb =row+smax_cal_rows*(col-1)

         scal_realadc(nb) = float(adc)-scal_ped_mean(nb)
         if (scal_realadc(nb).le.200)
     $        call hf1(sidcalsumadc,scal_realadc(nb),1.)
         if(scal_realadc(nb).gt.scal_threshold(nb)) then
            scal_num_hits           =scal_num_hits+1
            scal_rows(scal_num_hits)=row
            scal_cols(scal_num_hits)=col
            scal_adcs(scal_num_hits)=scal_realadc(nb)
         endif
      enddo                      !End loop over raw hits
*
      if(sdbg_sparsified_cal.gt.0) call s_prt_cal_sparsified
*
      call s_fill_cal_hist(abort,errmsg)
*
      return
      end
