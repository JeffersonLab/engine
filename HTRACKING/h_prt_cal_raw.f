*=======================================================================
      subroutine h_prt_cal_raw
*=======================================================================
*-
*-      Dumps the raw calorimeter data
*-
*-      Created: 19 Mar 1994      Tsolak A. Amatuni
*-      Modified 25 Mar 1994      DFG
*-                                Change name
*-                                Change lun
*-                7 Apr 1884      DFG   Change print order
* $Log$
* Revision 1.2  1995/01/27 20:26:00  cdaq
* (JRA) Subtract pedestal from ADC value
*
* Revision 1.1  1994/04/13  15:41:33  cdaq
* Initial revision
*
*-----------------------------------------------------------------------
*
      implicit none
      save
*
      integer*4 hit      !Hit number
      integer*4 row,col,nb
      real*4 adc
*
      include 'gen_data_structures.cmn'
      include 'hms_calorimeter.cmn'
      include 'hms_tracking.cmn'
*
      write(hlun_dbg_cal,10) hcal_tot_hits
   10 format(///'      HMS Calorimeter Raw Data      ',/,
     &          '      Total Number of Hits:',i3,     //,
     &          ' Hit #   Column #   Row #   ADC Value')
*
*
      if(hcal_tot_hits.le.0) return
*
      do hit=1,hcal_tot_hits
        row=hcal_row(hit)
        col=hcal_column(hit)
        nb =row+hmax_cal_rows*(col-1)
        adc=hcal_adc(hit)-hcal_ped_mean(nb)
        write(hlun_dbg_cal,20)
     &       hit,hcal_column(hit),hcal_row(hit),adc
 20     format(i5,3x,i5,4x,i5,7x,i5)
      enddo
*
      return
      end
