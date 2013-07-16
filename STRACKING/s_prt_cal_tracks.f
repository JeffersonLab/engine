*=======================================================================
      subroutine s_prt_cal_tracks
*=======================================================================
*-
*-      Dumps the calorimeter track quantities
*-
*-      Created: 20 Mar 1994      Tsolak A. Amatuni
*-      Modified 25 Mar 1994      DFG
*-                                Change name and lun
* $Log: s_prt_cal_tracks.f,v $
* Revision 1.2  1995/05/22 19:45:49  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.1  1994/04/13  18:21:11  cdaq
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
   10 format(///'      SOS  Calorimeter  Track  Quantities', /,
     &          '      Total Number of Detector Tracks:',i3,//,
     &          ' Track #   Cluster #   X[cm]   Y[cm]')
*
      if(sntracks_fp.le.0) return
*
      do nt=1,sntracks_fp
         write(slun_dbg_cal,20)
     &   nt,scluster_track(nt),strack_xc(nt),strack_yc(nt)
   20    format(3x,i5,7x,i5,2(2x,f6.2))
      enddo
*
      write(slun_dbg_cal,30) sntracks_cal
   30 format(' Total Number of Calorimeter Tracks:',i3)
*
      return
      end
