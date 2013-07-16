*=======================================================================
      subroutine h_prt_cal_decoded
*=======================================================================
*-
*-      Dumps the decoded calorimeter data
*-
*-      Created: 19 Mar 1994      Tsolak A. Amatuni
*-      Modified: 25 March 1994   DFG
*-                                Change name
*-                                Change lun
* $Log: h_prt_cal_decoded.f,v $
* Revision 1.2  1995/05/22 19:39:19  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.1  1994/04/13  15:41:16  cdaq
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
      write(hlun_dbg_cal,10) hnhits_cal
   10 format(///'          HMS Calorimeter Decoded Data      ',/,
     &          '             Total Number of Hits:',i4,      //,
     &          ' Hit #   X[cm]   Z[cm]   Energy Deposition[GeV]')
*
*
      if(hnhits_cal.le.0) return
*
      do hit=1,hnhits_cal
         write(hlun_dbg_cal,20)
     &   hit,hblock_xc(hit),hblock_zc(hit),hblock_de(hit)
   20    format(i5,3x,f6.2,1x,f7.2,5x,f9.4)
      enddo
*
      write(hlun_dbg_cal,30) hcal_e1,hcal_e2,hcal_e3,hcal_e4,hcal_et
   30 format( /,' Column #   Energy Deposition[GeV]',/,
     &          '    1         ',f9.4               ,/,
     &          '    2         ',f9.4               ,/,
     &          '    3         ',f9.4               ,/,
     &          '    4         ',f9.4               ,/,
     &          '        Total:',f9.4)
*
      return
      end
