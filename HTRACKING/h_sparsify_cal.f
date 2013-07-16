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
* $Log: h_sparsify_cal.f,v $
* Revision 1.13  2002/10/02 13:42:43  saw
* Check that user hists are defined before filling
*
* Revision 1.12  1999/06/10 16:51:25  csa
* (JRA) Removed adc_max, added adc sign test, structural and cosmetic changes
*
* Revision 1.11  1999/02/25 20:10:48  saw
* Vardan Tadevosyan shower code updates
*
* Revision 1.10  1999/02/23 18:48:45  csa
* (JRA) Add neg cal hf1 call
*
* Revision 1.9  1999/02/03 21:13:24  saw
* Code for new Shower counter tubes
*
* Revision 1.8  1998/12/17 22:02:40  saw
* Support extra set of tubes on HMS shower counter
*
* Revision 1.7  1996/01/16 21:58:51  cdaq
* (JRA) Onlys  histogram ADC's that are not 200 above pedestal
*
* Revision 1.6  1995/08/30 18:12:12  cdaq
* (JRA) Add a hist of all adc's into one spectrum
*
* Revision 1.5  1995/07/19  20:04:25  cdaq
* (JRA) Remove calorimeter raw data validity check
*
* Revision 1.4  1995/05/22  19:39:27  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.3  1995/05/11  14:54:05  cdaq
* (JRA) Add call to h_fill_cal_hist
*
* Revision 1.2  1994/09/13  20:31:08  cdaq
* (JRA) Subtract pedestals in sparsified data
*
* Revision 1.1  1994/04/13  16:21:31  cdaq
* Initial revision
*
*-----------------------------------------------------------------------
*
      implicit none
      save

      logical abort
      character*(*) errmsg
      character*14 here
      parameter (here='H_SPARSIFY_CAL')

      integer*4 nh        !Loop variable for raw hits
      integer*4 nb        !Block number
      integer*4 row,col   !Row & column numbers
      integer*4 adc_pos    !ADC value
      integer*4 adc_neg    !ADC value

      include 'hms_data_structures.cmn'
      include 'hms_calorimeter.cmn'
      include 'hms_id_histid.cmn'


      errmsg=' '
      if(hcal_tot_hits.lt.0.or.hcal_tot_hits.gt.hmax_cal_blocks) then
        write(6,*) here,':hcal_tot_hits = ',hcal_tot_hits
        return
      endif

      hcal_num_hits=0
      do nb = 1 , hmax_cal_blocks
        hcal_realadc_pos(nb)=-100
        hcal_realadc_neg(nb)=-100
      enddo
      if(hcal_tot_hits.le.0) return
*
*      Loop over raw hits
*
      do nh=1,hcal_tot_hits      
        row=hcal_row(nh)
        col=hcal_column(nh)
        nb =row+hmax_cal_rows*(col-1)
        adc_pos=hcal_adc_pos(nh)
        adc_neg=hcal_adc_neg(nh)

        if (adc_pos.ge.0) then        ! =-1 if no ADC value was read.
          hcal_realadc_pos(nb) = float(adc_pos) - hcal_pos_ped_mean(nb)
          if (hcal_realadc_pos(nb).le.200.and.hidcalsumadc.gt.0)
     &        call hf1(hidcalsumadc,hcal_realadc_pos(nb),1.)
        endif

        if (adc_neg.ge.0) then
          hcal_realadc_neg(nb) = float(adc_neg) - hcal_neg_ped_mean(nb)
          if (hcal_realadc_neg(nb).le.200.and.hidcalsumadc.gt.0)
     &        call hf1(hidcalsumadc,hcal_realadc_neg(nb),1.)
        endif
*
*      Sparsify the raw data
*
        if(hcal_realadc_pos(nb).gt.hcal_pos_threshold(nb) .or.
     &      hcal_realadc_neg(nb).gt.hcal_neg_threshold(nb)) then

           hcal_num_hits=hcal_num_hits+1
           hcal_rows(hcal_num_hits)=row
           hcal_cols(hcal_num_hits)=col
           if(hcal_realadc_pos(nb).lt.hcal_pos_threshold(nb)) then
              hcal_adcs_pos(hcal_num_hits)=0.0
           else
              hcal_adcs_pos(hcal_num_hits)=hcal_realadc_pos(nb)
           endif

           if(hcal_realadc_neg(nb).lt.hcal_neg_threshold(nb)) then
              hcal_adcs_neg(hcal_num_hits)=0.0
           else
              hcal_adcs_neg(hcal_num_hits)=hcal_realadc_neg(nb)
           endif
        endif
      enddo                      !End loop over raw hits

      if(hdbg_sparsified_cal.gt.0) call h_prt_cal_sparsified

      call h_fill_cal_hist(abort,errmsg)

      return
      end
