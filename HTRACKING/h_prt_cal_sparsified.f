*=======================================================================
      subroutine h_prt_cal_sparsified
*=======================================================================
*-
*-      Dumps the sparsified calorimeter data
*-
*-      Created: 19 Mar 1994      Tsolak A. Amatuni
*-      Modified 25 Mar 1994      DFG
*-                                change name and lun
* $Log$
* Revision 1.1  1994/04/13 15:41:58  cdaq
* Initial revision
*
*-----------------------------------------------------------------------
*
      implicit none
      save
*
      integer*4 hit      !Hit number
*
      include 'gen_data_structures.cmn'
      include 'hms_calorimeter.cmn'
      include 'hms_tracking.cmn'
*
      write(hlun_dbg_cal,10) hcal_num_hits
   10 format(///'   HMS Calorimeter Sparsified Data   ',/,
     &          '      Total Number of Hits:',i7,      //,
     &          ' Hit #   Row #   Column #   ADC - PED')
*
*
      if(hcal_num_hits.le.0) return
*
      do hit=1,hcal_num_hits
         write(hlun_dbg_cal,20)
     &   hit,hcal_rows(hit),hcal_cols(hit),hcal_adcs(hit)
   20    format(i5,3x,i5,4x,i5,6x,f8.2)
      enddo
*
      return
      end
