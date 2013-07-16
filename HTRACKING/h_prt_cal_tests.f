*=======================================================================
      subroutine h_prt_cal_tests
*=======================================================================
*-
*-      Dumps the calorimeter particle ID information
*-
*-      Created: 20 Mar 1994      Tsolak A. Amatuni
*-      Modified 25 Mar 1994      DFG
*-                                Change name and lun
* $Log: h_prt_cal_tests.f,v $
* Revision 1.2  1995/05/22 19:39:22  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.1  1994/04/13  15:42:13  cdaq
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
      include 'hms_data_structures.cmn'
      include 'hms_calorimeter.cmn'
      include 'hms_tracking.cmn'
*
*
      write(hlun_dbg_cal,10) hntracks_fp
   10 format(///'      HMS Calorimeter Particle ID Quantities', /,
     &          '         Total Number of Detector Tracks:',i3,//,
     &' Track #  N-blocks  E1[GeV]  E2[GeV]  E3[GeV]  E4[GeV]  Et[GeV]')
*
      if(hntracks_fp.le.0) return
*
      do nt=1,hntracks_fp
         write(hlun_dbg_cal,20)
     &   nt,
     &   hnblocks_cal(nt),
     &   htrack_e1(nt),
     &   htrack_e2(nt),
     &   htrack_e3(nt),
     &   htrack_e4(nt),
     &   htrack_et(nt)
   20    format(3x,i5,5x,i5,5(1x,f8.4))
      enddo
*
      return
      end
