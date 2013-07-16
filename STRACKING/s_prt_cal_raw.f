*=======================================================================
      subroutine s_prt_cal_raw
*=======================================================================
*-
*-      Dumps the raw calorimeter data
*-
*-      Created: 19 Mar 1994      Tsolak A. Amatuni
*-      Modified 25 Mar 1994      DFG
*-                                Change name
*-                                Change lun
*-                7 Apr 1994      DFG  Change print order
* $Log: s_prt_cal_raw.f,v $
* Revision 1.4  1999/01/29 17:34:59  saw
* Add variables for second tubes on shower counter
*
* Revision 1.3  1995/08/31 20:41:54  cdaq
* (JRA) Subtract pedestal in raw data dump
*
* Revision 1.2  1995/05/22  19:45:48  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.1  1994/04/13  18:20:03  cdaq
* Initial revision
*
*-----------------------------------------------------------------------
*
      implicit none
      save
*
      integer*4 hit      !Hit number
      integer*4 row,col,nb
      real*4 adc_pos, adc_neg

*
      include 'sos_data_structures.cmn'
      include 'sos_calorimeter.cmn'
      include 'sos_tracking.cmn'
*
      write(slun_dbg_cal,10) scal_tot_hits
   10 format(///'      SOS Calorimeter Raw Data      ',/,
     &          '      Total Number of Hits:',i3,     //,
     &          ' Hit #   Column #   Row #   ADC Value')
*
*
      if(scal_tot_hits.le.0) return
*
      do hit=1,scal_tot_hits
        row=scal_row(hit)
        col=scal_column(hit)
        nb =row+smax_cal_rows*(col-1)
        adc_pos=float(scal_adc_pos(hit))-scal_pos_ped_mean(nb)
        adc_neg=float(scal_adc_neg(hit))-scal_neg_ped_mean(nb)
        if(col.le.scal_num_neg_columns) then
          write(slun_dbg_cal,20)
     &         hit,scal_column(hit),scal_row(hit),adc_pos,adc_neg
 20       format(i5,3x,i5,4x,i5,7x,2f8.1)
        else
          write(slun_dbg_cal,20)
     &         hit,scal_column(hit),scal_row(hit),adc_pos
        endif
      enddo
*
      return
      end
