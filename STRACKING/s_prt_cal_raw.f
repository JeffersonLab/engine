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
* $Log$
* Revision 1.2  1995/05/22 19:45:48  cdaq
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
         write(slun_dbg_cal,20)
     &   hit,scal_column(hit),scal_row(hit),scal_adc(hit)
   20    format(i5,3x,i5,4x,i5,7x,i5)
      enddo
*
      return
      end
