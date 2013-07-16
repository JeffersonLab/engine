*=======================================================================
      subroutine s_prt_cal_tests
*=======================================================================
*-
*-      Dumps the calorimeter particle ID information
*-
*-      Created: 20 Mar 1994      Tsolak A. Amatuni
*-      Modified 25 Mar 1994      DFG
*-                                Change name and lun
* $Log: s_prt_cal_tests.f,v $
* Revision 1.2  1995/05/22 19:45:49  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.1  1994/04/13  18:20:44  cdaq
* Initial revision
*
*-----------------------------------------------------------------------
*
*
      implicit none
      save
*
      integer*4 nt      !Detector track number
*
      include 'sos_data_structures.cmn'
      include 'sos_calorimeter.cmn'
      include 'sos_tracking.cmn'
*
*
      write(slun_dbg_cal,10) sntracks_fp
   10 format(///'      SOS Calorimeter Particle ID Quantities', /,
     &          '         Total Number of Detector Tracks:',i3,//,
     &' Track #  N-blocks  E1[GeV]  E2[GeV]  E3[GeV]  E4[GeV]  Et[GeV]')
*
      if(sntracks_fp.le.0) return
*
      do nt=1,sntracks_fp
         write(slun_dbg_cal,20)
     &   nt,
     &   snblocks_cal(nt),
     &   strack_e1(nt),
     &   strack_e2(nt),
     &   strack_e3(nt),
     &   strack_e4(nt),
     &   strack_et(nt)
   20    format(3x,i5,5x,i5,5(1x,f8.4))
      enddo
*
      return
      end
