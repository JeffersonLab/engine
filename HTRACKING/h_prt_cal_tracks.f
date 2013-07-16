*=======================================================================
      subroutine h_prt_cal_tracks
*=======================================================================
*-
*-      Dumps the calorimeter track quantities
*-
*-      Created: 20 Mar 1994      Tsolak A. Amatuni
*-      Modified 25 Mar 1994      DFG
*-                                Change name and lun
* $Log: h_prt_cal_tracks.f,v $
* Revision 1.5  2003/03/21 22:21:51  jones
* Modified and rearrange routines to calibrate the HMS calorimeter (V. Tadevosyan)
*
* Revision 1.2  1995/05/22 19:39:23  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.1  1994/04/13  15:42:40  cdaq
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
   10 format(///'      HMS  Calorimeter  Track  Quantities', /,
     &          '      Total Number of Detector Tracks:',i3,//,
     &          ' Track #   Cluster #   X[cm]   Y[cm]')
*
      if(hntracks_fp.le.0) return
*
      do nt=1,hntracks_fp
         write(hlun_dbg_cal,20)
     &   nt,hcluster_track(nt),htrack_xc(nt),htrack_yc(nt)
   20    format(3x,i5,7x,i5,2(2x,f6.2))
      enddo
*
      write(hlun_dbg_cal,30) hntracks_cal
   30 format(' Total Number of Calorimeter Tracks:',i3)
*
      return
      end
