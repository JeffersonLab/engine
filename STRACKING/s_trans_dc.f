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
* $Log$
* Revision 1.5  1994/11/23 15:08:04  cdaq
* * (SPB) Recopied from hms file and modified names for SOS
*
* Revision 1.4  1994/04/13  18:56:40  cdaq
* (DFG) Add call to s_fill_dc_dec_hist, remove s_raw_dump_all call
*
* Revision 1.3  1994/03/24  19:59:03  cdaq
* (DFG) add print routines and flags
*       check plane number and wire number for validity
*
* Revision 1.2  1994/02/22  14:22:58  cdaq
* (SAW) replace err='' with ' '
*
* Revision 1.1  1994/02/21  16:42:58  cdaq
* Initial revision
*
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
      integer*4 ihit,goodhit,old_wire,old_plane,wire,plane,chamber
*     
      ABORT= .FALSE.
      err= ' '
      old_wire = -1
      old_plane = -1
      goodhit = 0
      if (sdc_center(1).eq.0.) then   !initialize hdc_center if not yet set.
        do plane = 1, smax_num_dc_planes
          chamber = sdc_chamber_planes(plane)
          sdc_center(plane) = sdc_xcenter(chamber)
     $         *sin(sdc_alpha_angle(plane))+sdc_ycenter(chamber)
     $         *cos(sdc_alpha_angle(plane))
        enddo
      endif
*     Are there any raw hits
      if(SDC_RAW_TOT_HITS.gt.0) then
*     loop over all raw hits
        do ihit=1,SDC_RAW_TOT_HITS
          plane = SDC_RAW_PLANE_NUM(ihit)
          wire  = SDC_RAW_WIRE_NUM(ihit)
*     check valid plane and wire number
          if(plane.gt.0 .and. plane.le. sdc_num_planes) then
*     test if tdc value less than lower limit for good hits
            if(SDC_RAW_TDC(ihit) .lt. sdc_tdc_min_win(plane))  then
              swire_early_mult(wire,plane)
     $             = swire_early_mult(wire,plane)+1
            else
              if(SDC_RAW_TDC(ihit) .gt. sdc_tdc_max_win(plane))  then
                swire_late_mult(wire,plane)
     $               = swire_late_mult(wire,plane)+1
              else
*     test for valid wire number
                if(wire .gt. 0 .and. wire .le. sdc_nrwire(plane) ) then
*     test for multiple hit on the same wire
                  if(plane .eq. old_plane .and. wire .eq. old_wire ) then
                    swire_extra_mult(wire,plane) =
     $                   swire_extra_mult(wire,plane)+1
                  else

*     valid hit proceed with decoding
                    goodhit = goodhit + 1
                    SDC_PLANE_NUM(goodhit) = SDC_RAW_PLANE_NUM(ihit)
                    SDC_WIRE_NUM(goodhit) = SDC_RAW_WIRE_NUM(ihit)
                    SDC_TDC(goodhit) = SDC_RAW_TDC(ihit)
                    SDC_WIRE_CENTER(goodhit) = s_wire_center_calc(plane
     $                   ,wire)
                    SDC_DRIFT_TIME(goodhit) = 
     &                   s_drift_time_calc(plane,wire,SDC_TDC(goodhit))
                    SDC_DRIFT_DIS(goodhit) =
     &                   s_drift_dist_calc(plane,wire
     $                   ,SDC_DRIFT_TIME(goodhit))
                    SDC_HITS_PER_PLANE(plane) = SDC_HITS_PER_PLANE(plane)
     $                   + 1
                  endif                 ! end test on duplicate wire
                  old_plane = plane
                  old_wire  = wire
                endif                   ! end test on hdc_tdc_max_win
              endif                     ! end test on hdc_tdc_min_win
            endif                       ! end test on valid wire number
          endif                         ! end test on valid plane number
        enddo                           ! end loop over raw hits

*     
*     set total number of good hits
*     
        SDC_TOT_HITS = goodhit

*     Histogram SDC_DECODED_DC
        call s_fill_dc_dec_hist(ABORT,err)

      endif                             !  end test on SDC_RAW_TOT_HITS.gt.0
*     Dump decoded banks if flag is set
      if(sdebugprintdecodeddc.ne.0) then
        call s_print_decoded_dc(ABORT,err)
      endif
*     
      RETURN
      END
