       SUBROUTINE H_TRANS_DC(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Translate HMS raw drift and start time 
*-                                to decoded information 
*-
*-      Required Input BANKS     HMS_RAW_DC
*-                               HMS_DECODED_SCIN
*-
*-      Output BANKS             HMS_DECODED_DC
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
*-   Created 19-JAN-1994   D. F. Geesaman
*-                           Dummy Shell routine
* $Log$
* Revision 1.3  1994/03/24 19:48:48  cdaq
* (DFG) add print routines and flags
*       check plane number and wire number for validity
*
* Revision 1.2  1994/02/22  05:27:06  cdaq
* (SAW) Make err ' ' instead of ''
*
* Revision 1.1  1994/02/19  06:21:23  cdaq
* Initial revision
*
*--------------------------------------------------------
       IMPLICIT NONE
       SAVE
*
       character*50 here
       parameter (here= 'H_TRANS_DC')
*
       logical ABORT
       character*(*) err
*
       include 'gen_data_structures.cmn'
       include 'gen_constants.par'
       include 'gen_units.par'
       include 'hms_tracking.cmn'
       include 'hms_geometry.cmn'          
*
*--------------------------------------------------------
       real*4 h_drift_time_calc,h_drift_dist_calc,h_wire_center_calc
       external h_drift_time_calc
       external h_drift_dist_calc
       external h_wire_center_calc
       integer*4 ihit,goodhit,old_wire,old_plane,wire,plane
*
       ABORT= .FALSE.
       err= ' '
*      Dump raw bank if debug flag set
       if(hdebugprintrawdc.ne.0) then
          call h_print_raw_dc(ABORT,err)
       endif
       old_wire = -1
       old_plane = -1
       goodhit = 0
*      Are there any raw hits
       if(HDC_RAW_TOT_HITS.gt.0) then
*      loop over all raw hits
         do ihit=1,HDC_RAW_TOT_HITS
           plane = HDC_RAW_PLANE_NUM(ihit)
           wire  = HDC_RAW_WIRE_NUM(ihit)
*      check valid plane and wire number
          if(plane.gt.0 .and. plane.le. hdc_num_planes) then
*      test for valid wire number
            if(wire .gt. 0 .and. wire .le. hdc_nrwire(plane) ) then
*      test for multiple hit on the same wire
             if(plane .eq. old_plane .and. wire .eq. old_wire ) then
                hwire_mult(wire,plane) = hwire_mult(wire,plane)+1
             else

*          valid hit proceed with decoding
                goodhit = goodhit + 1
                HDC_PLANE_NUM(goodhit) = HDC_RAW_PLANE_NUM(ihit)
                HDC_WIRE_NUM(goodhit) = HDC_RAW_WIRE_NUM(ihit)
                HDC_TDC(goodhit) = HDC_RAW_TDC(ihit)
                HDC_WIRE_CENTER(goodhit) = h_wire_center_calc(plane,wire)
     &
                HDC_DRIFT_TIME(goodhit) = 
     &               h_drift_time_calc(plane,wire,HDC_TDC(goodhit))
                HDC_DRIFT_DIS(goodhit) =
     &               h_drift_dist_calc(plane,wire,HDC_DRIFT_TIME(goodhit))
                HDC_HITS_PER_PLANE(plane) = HDC_HITS_PER_PLANE(plane) + 1
             endif                       ! end test on duplicate wire
             old_plane = plane
             old_wire  = wire
            endif                     ! end test on valid wire number
          endif                       ! end test on valid plane number
         enddo                        ! end loop over raw hits
*
*      set total number of good hits
*
            HDC_TOT_HITS = goodhit

                
       endif                !  end test on HDC_RAW_TOT_HITS.gt.0
*      Dump decoded banks if flag is set
       if(hdebugprintdecodeddc.ne.0) then
          call h_print_decoded_dc(ABORT,err)
       endif
*
       RETURN
       END
