       SUBROUTINE S_TRANS_DC(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Translate SOS raw drift and start time 
*-                                to decoded information 
*-
*-      Required Input BANKS     SOS_RAW_DC
*-                               SOS_DECODED_SCIN
*-
*-      Output BANKS             SOS_DECODED_DC
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
*-   Created 19-JAN-1994   D. F. Geesaman
*-                           Dummy Shell routine
* $Log$
* Revision 1.2  1994/02/22 14:22:58  cdaq
* (SAW) replace err='' with ' '
*
* Revision 1.1  1994/02/21  16:42:58  cdaq
* Initial revision
*
*-
*-
*--------------------------------------------------------
       IMPLICIT NONE
       SAVE
*
       character*50 here
       parameter (here= 'S_TRANS_DC')
*
       logical ABORT
       character*(*) err
*
       include 'gen_data_structures.cmn'
       include 'gen_constants.par'
       include 'gen_units.par'
       include 'sos_tracking.cmn'
       include 'sos_geometry.cmn'          
*
*--------------------------------------------------------
       real*4 s_drift_time_calc,s_drift_dist_calc,s_wire_center_calc
       external s_drift_time_calc
       external s_drift_dist_calc
       external s_wire_center_calc
       integer*4 ihit,goodhit,old_wire,old_plane,wire,plane
*
       ABORT= .FALSE.
       err= ' '
       old_wire = -1
       old_plane = -1
       goodhit = 0
*      Are there any raw hits
       if(SDC_RAW_TOT_HITS.gt.0) then
*      loop over all raw hits
         do ihit=1,SDC_RAW_TOT_HITS
           plane = SDC_RAW_PLANE_NUM(ihit)
           wire  = SDC_RAW_WIRE_NUM(ihit)
*      test for multiple hit on the same wire
           if(plane .eq. old_plane .and. wire .eq. old_wire ) then
                swire_mult(wire,plane) = swire_mult(wire,plane)+1
           else
*          valid hit proceed with decoding
                goodhit = goodhit + 1
                SDC_PLANE_NUM(goodhit) = SDC_RAW_PLANE_NUM(ihit)
                SDC_WIRE_NUM(goodhit) = SDC_RAW_WIRE_NUM(ihit)
                SDC_TDC(goodhit) = SDC_RAW_TDC(ihit)
                SDC_WIRE_CENTER(goodhit) = s_wire_center_calc(plane,wire)
     &
                SDC_DRIFT_TIME(goodhit) = 
     &               s_drift_time_calc(plane,wire,SDC_TDC(goodhit))
                SDC_DRIFT_DIS(goodhit) =
     &               s_drift_dist_calc(plane,wire,SDC_DRIFT_TIME(goodhit))
                SDC_HITS_PER_PLANE(plane) = SDC_HITS_PER_PLANE(plane) + 1
*
            endif                       ! end test on duplicate wire
            old_plane = plane
            old_wire  = wire
         enddo                          ! end loop over raw hits
*
*      set total number of good hits
*
            SDC_TOT_HITS = goodhit

                
       endif                !  end test on SDC_RAW_TOT_HITS.gt.0
       RETURN
       END
