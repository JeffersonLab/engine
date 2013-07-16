*=======================================================================
      subroutine s_prt_cal_sparsified
*=======================================================================
*-
*-      Dumps the sparsified calorimeter data
*-
*-      Created: 19 Mar 1994      Tsolak A. Amatuni
*-      Modified 25 Mar 1994      DFG
*-                                change name and lun
* $Log: s_prt_cal_sparsified.f,v $
* Revision 1.3  1999/01/29 17:34:59  saw
* Add variables for second tubes on shower counter
*
* Revision 1.2  1995/05/22 19:45:49  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.1  1994/04/13  18:20:30  cdaq
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
      write(slun_dbg_cal,10) scal_num_hits
   10 format(///'   SOS Calorimeter Sparsified Data   ',/,
     &          '      Total Number of Hits:',i7,      //,
     &          ' Hit #   Row #   Column #   ADC - PED')
*
*
      if(scal_num_hits.le.0) return
*
      do hit=1,scal_num_hits
         write(slun_dbg_cal,20)
     &   hit,scal_rows(hit),scal_cols(hit),scal_adcs_pos(hit)
     &   ,scal_adcs_neg(hit)
   20    format(i5,3x,i5,4x,i5,6x,2f8.2)
      enddo
*
      return
      end
