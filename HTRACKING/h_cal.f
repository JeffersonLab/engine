*=======================================================================
      subroutine h_cal(abort,errmsg)
*=======================================================================
*-
*-      Purpose: Computes the calorimeter particle ID quantities.
*-               Corrects the energy depositions for impact point 
*-               coordinate dependence.
*-
*-      Input Bank: HMS_TRACKS_CAL
*-
*-      Output Bank: HMS_TRACK_TESTS
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
*-      Created: 15 Mar 1994      Tsolak A. Amatuni
*
* $Log$
* Revision 1.5  1998/12/17 22:02:38  saw
* Support extra set of tubes on HMS shower counter
*
* Revision 1.4  1995/05/22 19:39:04  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.3  1994/09/13  19:39:14  cdaq
* (JRA) Add preshower energy
*
* Revision 1.2  1994/04/12  21:24:55  cdaq
* (DFG) Put in real code and change name of print routine.
*
* Revision 1.1  1994/02/19  06:12:35  cdaq
* Initial revision
*
*--------------------------------------------------------
      implicit none
      save
*     
      logical abort
      character*(*) errmsg
      character*5 here
      parameter (here='H_CAL')
*
      integer*4 nt           !Detector track number
      integer*4 nc           !Calorimeter cluster number
      real*4    cor          !Correction factor for X,Y dependence
      real*4 h_correct_cal   !External function to compute "cor" 
*
      include 'hms_data_structures.cmn'
      include 'hms_calorimeter.cmn'
*
      do nt=1,hntracks_fp
         htrack_e1(nt)=0.
         htrack_e2(nt)=0.
         htrack_e3(nt)=0.
         htrack_e4(nt)=0.
         htrack_et(nt)=0.
         htrack_preshower_e(nt)=0.
      enddo
*
      call h_clusters_cal(abort,errmsg)
      if(abort) then
         call g_add_path(here,errmsg)
         return
      endif
*
      call h_tracks_cal(abort,errmsg)
      if(abort) then
         call g_add_path(here,errmsg)
         return
      endif
*
*      Return if there are no tracks found or none of the found
*      tracks matches a cluster in the calorimeter.
*
      if(hntracks_fp .le.0) go to 100   !Return
      if(hntracks_cal.le.0) go to 100   !Return
*
      do nt =1,hntracks_fp
         nc=hcluster_track(nt)
* None of this deals with two tube blocks properly
         if(nc.gt.0) then
            cor=h_correct_cal(htrack_xc(nt),htrack_yc(nt))
*
            hnblocks_cal(nt)=hcluster_size(nc)
*
            htrack_e1(nt)=cor*hcluster_e1(nc)
            htrack_e2(nt)=cor*hcluster_e2(nc)
            htrack_e3(nt)=cor*hcluster_e3(nc)
            htrack_e4(nt)=cor*hcluster_e4(nc)
            htrack_et(nt)=cor*hcluster_et(nc)
            htrack_preshower_e(nt)=cor*hcluster_e1(nc)
         endif                          !End ... if nc > 0
      enddo                             !End loop over detector tracks
*
  100 continue
      if(hdbg_tests_cal.gt.0) call h_prt_cal_tests
*
      return
      end
