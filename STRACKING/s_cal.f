      SUBROUTINE S_CAL(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Analyze Calorimeter information for each track 
*-
*-      Required Input BANKS     SOS_RAW_CAL
*-                               SOS_DECODED_CAL
*-                               SOS_FOCAL_PLANE
*-
*-      Output BANKS             SOS_TRACK_TESTS
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
*-   Created 19-JAN-1994   D. F. Geesaman
*-                           Dummy Shell routine
* $Log$
* Revision 1.4  1995/05/22 19:45:31  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.3  1995/05/11  21:02:26  cdaq
* (JRA) Add call to s_tracks_cal
*
* Revision 1.2  1994/11/22  21:05:51  cdaq
* (SPB) Recopied from hms file and modified names for SOS
*
* Revision 1.1  1994/02/21  16:06:52  cdaq
* Initial revision
*-
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
      INCLUDE 'sos_data_structures.cmn'
      INCLUDE 'sos_calorimeter.cmn'
*
      character*50 here
      parameter (here= 'S_CAL')
*
      logical ABORT
      character*(*) err
*
      integer*4 nt                      !Detector track number
      integer*4 nc                      !Calorimeter cluster number
      real*4    cor                     !Correction factor for X,Y dependence
      real*4 s_correct_cal              !External function to compute "cor"
*
*
*--------------------------------------------------------
*
      do nt=1, sntracks_fp
        strack_e1(nt)=0.
        strack_e2(nt)=0.
        strack_e3(nt)=0.
        strack_e4(nt)=0.
        strack_et(nt)=0.
        strack_preshower_e(nt)=0.
      enddo
*      
      call s_clusters_cal(abort,err)
      if(abort) then
        call g_add_path(here,err)
        return
      endif
*
      call s_tracks_cal(abort,err)
      if (abort) then
        call g_add_path(here,err)
        return
      endif
*
*     Return if there are no tracks found or none of the found tracks
*     matches a cluster in the calorimeter
*            
      if(sntracks_fp .le.0) go to 100   !Return
      if(sntracks_cal.le.0) go to 100   !Return
*             
      do nt =1,sntracks_fp
        nc=scluster_track(nt)
        if(nc.gt.0) then
          cor=s_correct_cal(strack_xc(nt),strack_yc(nt))
*
          snblocks_cal(nt)=scluster_size(nc)
*
          strack_e1(nt)=cor*scluster_e1(nc)
          strack_e2(nt)=cor*scluster_e2(nc)       
          strack_e3(nt)=cor*scluster_e3(nc)       
          strack_e4(nt)=cor*scluster_e4(nc)       
          strack_et(nt)=cor*scluster_et(nc)
          strack_preshower_e(nt)=cor*scluster_e1(nc)
        endif                           !End ... if nc > 0
      enddo                             !End loop over detetor tracks       
*
 100  continue
      if(sdbg_tests_cal.gt.0) call s_prt_cal_tests
*

      return
      end             
