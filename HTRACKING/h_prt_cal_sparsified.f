*=======================================================================
      subroutine h_prt_cal_sparsified
*=======================================================================
*-
*-      Dumps the sparsified calorimeter data
*-
*-      Created: 19 Mar 1994      Tsolak A. Amatuni
*-      Modified 25 Mar 1994      DFG
*-                                change name and lun
* $Log: h_prt_cal_sparsified.f,v $
* Revision 1.3  1998/12/17 22:02:39  saw
* Support extra set of tubes on HMS shower counter
*
* Revision 1.2  1995/05/22 19:39:21  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.1  1994/04/13  15:41:58  cdaq
* Initial revision
*
*-----------------------------------------------------------------------
*
      implicit none
      save
*
      integer*4 hit      !Hit number
*
      include 'hms_data_structures.cmn'
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
     &   hit,hcal_rows(hit),hcal_cols(hit),hcal_adcs_pos(hit)
     &   ,hcal_adcs_neg(hit)
   20    format(i5,3x,i5,4x,i5,6x,2f8.2)
      enddo
*
      return
      end
