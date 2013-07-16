*=======================================================================
      subroutine s_prt_cal_decoded
*=======================================================================
*-
*-      Dumps the decoded calorimeter data
*-
*-      Created: 19 Mar 1994      Tsolak A. Amatuni
*-      Modified: 25 March 1994   DFG
*-                                Change name
*-                                Change lun
* $Log: s_prt_cal_decoded.f,v $
* Revision 1.2  1995/05/22 19:45:48  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.1  1994/04/13  18:19:52  cdaq
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
      write(slun_dbg_cal,10) snhits_cal
   10 format(///'          SOS Calorimeter Decoded Data      ',/,
     &          '             Total Number of Hits:',i4,      //,
     &          ' Hit #   X[cm]   Z[cm]   Energy Deposition[GeV]')
*
*
      if(snhits_cal.le.0) return
*
      do hit=1,snhits_cal
         write(slun_dbg_cal,20)
     &   hit,sblock_xc(hit),sblock_zc(hit),sblock_de(hit)
   20    format(i5,3x,f6.2,1x,f7.2,5x,f9.4)
      enddo
*
      write(slun_dbg_cal,30) scal_e1,scal_e2,scal_e3,scal_e4,scal_et
   30 format( /,' Column #   Energy Deposition[GeV]',/,
     &          '    1         ',f9.4               ,/,
     &          '    2         ',f9.4               ,/,
     &          '    3         ',f9.4               ,/,
     &          '    4         ',f9.4               ,/,
     &          '        Total:',f9.4)
*
      return
      end
