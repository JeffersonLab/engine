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
* $Log$
* Revision 1.9  1995/05/17 14:02:20  cdaq
* (JRA) Add hist for all dc tdc's in one histogram.
*
* Revision 1.8  1995/04/06  19:34:34  cdaq
* (JRA) HMAX_NUM_DC_PLANES -> HDC_NUM_PLANES
*
* Revision 1.7  1994/09/14  14:10:49  cdaq
* (JRA) Initialize hdc_center array first time.
*
* Revision 1.6  1994/08/16  13:24:58  cdaq
* (DJA) Move call to h_fill_dc_dec_hist to h_pattern_recognition
*
* Revision 1.5  1994/06/15  20:35:59  cdaq
* (DFG) Add upper and lower limit for valid TDC
*
* Revision 1.4  1994/04/13  17:59:46  cdaq
* (DFG) add histogram call and remove raw dump
*
* Revision 1.3  1994/03/24  19:48:48  cdaq
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
      include 'hms_track_histid.cmn'
*
*--------------------------------------------------------
      real*4 h_drift_time_calc,h_drift_dist_calc,h_wire_center_calc
      external h_drift_time_calc
      external h_drift_dist_calc
      external h_wire_center_calc
      integer*4 ihit,goodhit,old_wire,old_plane,wire,plane,chamber
      real*4 histval
*
      ABORT= .FALSE.
      err= ' '
      old_wire = -1
      old_plane = -1
      goodhit = 0
      
      if (hdc_center(1).eq.0.) then   !initialize hdc_center if not yet set.
        do plane = 1, hdc_num_planes
          chamber = hdc_chamber_planes(plane)
          hdc_center(plane) = hdc_xcenter(chamber)*sin(hdc_alpha_angle(plane))+
     &                        hdc_ycenter(chamber)*cos(hdc_alpha_angle(plane))
        enddo
      endif
      
*     Are there any raw hits
      if(HDC_RAW_TOT_HITS.gt.0) then
*     loop over all raw hits
        do ihit=1,HDC_RAW_TOT_HITS
          plane = HDC_RAW_PLANE_NUM(ihit)
          wire  = HDC_RAW_WIRE_NUM(ihit)
*     check valid plane and wire number
          if(plane.gt.0 .and. plane.le. hdc_num_planes) then
*     test if tdc value less than lower limit for good hits
            histval=float(hdc_raw_tdc(ihit))
            call hf1(hidrawtdc,histval,1.)
            if(HDC_RAW_TDC(ihit) .lt. hdc_tdc_min_win(plane))  then
              hwire_early_mult(wire,plane)
     $             = hwire_early_mult(wire,plane)+1
            else
              if(HDC_RAW_TDC(ihit) .gt. hdc_tdc_max_win(plane))  then
                hwire_late_mult(wire,plane)
     $               = hwire_late_mult(wire,plane)+1
              else
*     test for valid wire number
                if(wire .gt. 0 .and. wire .le. hdc_nrwire(plane) )
     $               then
*     test for multiple hit on the same wire
                  if(plane .eq. old_plane .and. wire .eq. old_wire )
     $                 then
                    hwire_extra_mult(wire,plane) =
     $                   hwire_extra_mult(wire,plane)+1
                  else
                    
*     valid hit proceed with decoding
                    goodhit = goodhit + 1
                    HDC_PLANE_NUM(goodhit) = HDC_RAW_PLANE_NUM(ihit)
                    HDC_WIRE_NUM(goodhit) = HDC_RAW_WIRE_NUM(ihit)
                    HDC_TDC(goodhit) = HDC_RAW_TDC(ihit)
                    HDC_WIRE_CENTER(goodhit) =
     $                   h_wire_center_calc(plane,wire)
                    HDC_DRIFT_TIME(goodhit) = 
     &                   h_drift_time_calc(plane,wire
     $                   ,HDC_TDC(goodhit))
                    HDC_DRIFT_DIS(goodhit) = h_drift_dist_calc
     $                   (plane,wire,HDC_DRIFT_TIME(goodhit))
                    HDC_HITS_PER_PLANE(plane) =
     $                   HDC_HITS_PER_PLANE(plane) + 1
                    hwire_mult(wire,plane) = hwire_mult(wire,plane)+1
                    
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
        HDC_TOT_HITS = goodhit
         
      endif                             !  end test on HDC_RAW_TOT_HITS.gt.0
*     
*     Dump decoded banks if flag is set
      if(hdebugprintdecodeddc.ne.0) then
        call h_print_decoded_dc(ABORT,err)
      endif
*     
      RETURN
      END
*********
*     Local Variables:
*     mode: fortran
*     fortran-if-indent: 2
*     fortran-do-indent: 2
*     End:
