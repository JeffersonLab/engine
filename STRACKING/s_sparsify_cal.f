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
* $Log: s_sparsify_cal.f,v $
* Revision 1.13  2002/07/31 20:20:58  saw
* Only try to fill user hists that are defined
*
* Revision 1.12  1999/06/10 16:57:38  csa
* (JRA) Removed adc_max, added adc sign test, structural and cosmetic changes
*
* Revision 1.11  1999/02/25 20:18:40  saw
* Vardan Tadevosyan shower code updates
*
* Revision 1.10  1999/02/23 19:00:04  csa
* (JRA) Add neg cal hf1 call
*
* Revision 1.9  1999/02/03 21:13:45  saw
* Code for new Shower counter tubes
*
* Revision 1.8  1999/01/29 17:34:59  saw
* Add variables for second tubes on shower counter
*
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
      implicit none
      save

      logical abort
      character*(*) errmsg
      character*14 here
      parameter (here='S_SPARSIFY_CAL')

      integer*4 nh        !Loop variable for raw hits
      integer*4 nb        !Block number
      integer*4 row,col   !Row & column numbers
      integer*4 adc_pos   !ADC value
      integer*4 adc_neg   !ADC value

      include 'sos_data_structures.cmn'
      include 'sos_calorimeter.cmn'
      include 'sos_id_histid.cmn'


      errmsg=' '
      if(scal_tot_hits.lt.0.or.scal_tot_hits.gt.smax_cal_blocks) then
        write(6,*) here,':scal_tot_hits = ',scal_tot_hits
        return
      endif

      scal_num_hits=0
      do nb = 1 , smax_cal_blocks
        scal_realadc_pos(nb)=-100
        scal_realadc_neg(nb)=-100
      enddo
      if(scal_tot_hits.le.0) return
*
*      Loop over raw hits
*
      do nh=1,scal_tot_hits      
        row=scal_row(nh)
        col=scal_column(nh)
        nb =row+smax_cal_rows*(col-1)
        adc_pos=scal_adc_pos(nh)
        adc_neg=scal_adc_neg(nh)

        if (adc_pos.ge.0) then        ! =-1 if no ADC value was read.
          scal_realadc_pos(nb) = float(adc_pos) - scal_pos_ped_mean(nb)
          if (scal_realadc_pos(nb).le.200.and.sidcalsumadc.gt.0)
     &        call hf1(sidcalsumadc,scal_realadc_pos(nb),1.)
	endif

        if (adc_neg.ge.0) then        ! =-1 if no ADC value was read.
          scal_realadc_neg(nb) = float(adc_neg) - scal_neg_ped_mean(nb)
          if (scal_realadc_neg(nb).le.200.and.sidcalsumadc.gt.0)
     &        call hf1(sidcalsumadc,scal_realadc_neg(nb),1.)
	endif
*
*      Sparsify the raw data
*
        if(scal_realadc_pos(nb).gt.scal_pos_threshold(nb) .or.
     &        scal_realadc_neg(nb).gt.scal_neg_threshold(nb)) then

           scal_num_hits=scal_num_hits+1
           scal_rows(scal_num_hits)=row
           scal_cols(scal_num_hits)=col
           if(scal_realadc_pos(nb).lt.scal_pos_threshold(nb)) then
              scal_adcs_pos(scal_num_hits)= 0.0
           else
              scal_adcs_pos(scal_num_hits)=scal_realadc_pos(nb)
           endif
           if(scal_realadc_neg(nb).lt.scal_neg_threshold(nb)) then
              scal_adcs_neg(scal_num_hits)= 0.0
           else
              scal_adcs_neg(scal_num_hits)=scal_realadc_neg(nb)
           endif
        endif
      enddo                      !End loop over raw hits

      if(sdbg_sparsified_cal.gt.0) call s_prt_cal_sparsified

      call s_fill_cal_hist(abort,errmsg)

      return
      end
