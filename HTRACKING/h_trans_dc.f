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
* $Log: h_trans_dc.f,v $x1
* Revision 1.15  2002/10/02 13:42:43  saw
* Check that user hists are defined before filling
*
* Revision 1.14  1996/09/04 14:23:38  saw
* (??) Cosmetic
*
* Revision 1.13  1996/01/16 21:37:13  cdaq
* (JRA) Change sign on hstart_time
*
* Revision 1.12  1995/10/11 13:51:04  cdaq
* (JRA) Cleanup, add bypass switch to h_dc_eff call
*
* Revision 1.11  1995/08/30 15:27:39  cdaq
* (JRA) Add call to h_dc_eff, warn about invalid plane numbers
*
* Revision 1.10  1995/05/22  19:39:32  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.9  1995/05/17  14:02:20  cdaq
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
      character*10 here
      parameter (here= 'H_TRANS_DC')
*
      logical ABORT
      character*(*) err
*
      include 'hms_data_structures.cmn'
      include 'gen_constants.par'
      include 'gen_units.par'
      include 'hms_tracking.cmn'
      include 'hms_geometry.cmn'
      include 'hms_track_histid.cmn'
      include 'hms_bypass_switches.cmn'
*
*--------------------------------------------------------
      real*4 h_drift_dist_calc
      external h_drift_dist_calc
      integer*4 ihit,goodhit,old_wire,old_pln,wire,pln,chamber
      real*4 histval
      integer j
*
      ABORT= .FALSE.
      err= ' '
      old_wire = -1
      old_pln = -1
      goodhit = 0
      
      if (hdc_center(1).eq.0.) then   !initialize hdc_center if not yet set.
        do pln = 1, hdc_num_planes
          chamber = hdc_chamber_planes(pln)
          hdc_center(pln) = hdc_xcenter(chamber)*sin(hdc_alpha_angle(pln))+
     &                      hdc_ycenter(chamber)*cos(hdc_alpha_angle(pln))
        enddo
      endif
*     Are there any raw hits
      if(hdc_raw_tot_hits.gt.0) then
*     loop over all raw hits
        do ihit=1,hdc_raw_tot_hits
          pln = hdc_raw_plane_num(ihit)
          wire  = hdc_raw_wire_num(ihit)
*     check valid plane and wire number
          if(pln.gt.0 .and. pln.le. hdc_num_planes) then
            histval=float(hdc_raw_tdc(ihit))
            if(hidrawtdc.gt.0) call hf1(hidrawtdc,histval,1.)
*     test if tdc value less than lower limit for good hits
            if(hdc_raw_tdc(ihit) .lt. hdc_tdc_min_win(pln))  then
              hwire_early_mult(wire,pln) = hwire_early_mult(wire,pln)+1
            else
              if(hdc_raw_tdc(ihit) .gt. hdc_tdc_max_win(pln))  then
                hwire_late_mult(wire,pln) = hwire_late_mult(wire,pln)+1
              else
*     test for valid wire number
                if(wire .gt. 0 .and. wire .le. hdc_nrwire(pln) ) then
*     test for multiple hit on the same wire
                  if(pln .eq. old_pln .and. wire .eq. old_wire ) then
                    hwire_extra_mult(wire,pln) = hwire_extra_mult(wire,pln)+1
                  else
                    
*     valid hit proceed with decoding
                    goodhit = goodhit + 1
                    hdc_plane_num(goodhit) = hdc_raw_plane_num(ihit)
                    hdc_wire_num(goodhit) = hdc_raw_wire_num(ihit)
                    hdc_tdc(goodhit) = hdc_raw_tdc(ihit)

                    if(hdc_wire_counting(pln).eq.0) then    !normal ordering
                      hdc_wire_center(goodhit) = hdc_pitch(pln)
     &                     * (float(wire)-hdc_central_wire(pln))
     &                     - hdc_center(pln)
                    else
                      hdc_wire_center(goodhit) = hdc_pitch(pln)
     &                     * ((hdc_nrwire(pln)+(1-wire))
     &                     - hdc_central_wire(pln)) - hdc_center(pln)
                    endif
                    hdc_drift_time(goodhit) = - hstart_time
     &                   - float(hdc_tdc(goodhit))*hdc_tdc_time_per_channel
     &                   + hdc_plane_time_zero(pln)
*  find dist in pattern_recognition, after apply propogation correction.
*                    hdc_drift_dis(goodhit) = h_drift_dist_calc
*     $                   (pln,wire,hdc_drift_time(goodhit))
                    hdc_hits_per_plane(pln) = hdc_hits_per_plane(pln) + 1
                    hwire_mult(wire,pln) = hwire_mult(wire,pln)+1
                  endif                 ! end test on duplicate wire
                  old_pln = pln
                  old_wire  = wire
                endif                   ! end test on valid wire number
              endif                     ! end test on hdc_tdc_max_win
            endif                       ! end test on hdc_tdc_min_win
          else                          ! if not a valid plane number
            write(6,*) 'H_TRANS_DC: invalid plane number = ',pln
          endif                         ! end test on valid plane number
        enddo                           ! end loop over raw hits
*     
*     set total number of good hits
*     
        HDC_TOT_HITS = goodhit
*
        if (hbypass_dc_eff.eq.0) call h_dc_eff !only call if there is a hit.
*
      endif                             !  end test on hdc_raw_tot_hits.gt.0
*
*
*     Dump decoded banks if flag is set

      if(hdebugprintdecodeddc.ne.0) then
        call h_print_decoded_dc(ABORT,err)
      endif
      if(hdebugprintrawdc.ne.0 .and. HDC_TOT_HITS.GT.0) then
       write(hluno,'(''     HDC_TOT_HITS='',I4,f15.5)') HDC_TOT_HITS,hstart_time
         write(hluno,'('' Num  Plane     Wire          TDC Value'')')
         do j=1,HDC_TOT_HITS
         write(hluno,'(3i5,2(f10.4,1x),f8.6,1x,2(f10.4,1x))')
     &     j,HDC_PLANE_NUM(j),HDC_WIRE_NUM(j),
     &        HDC_drift_time(j),HDC_wire_center(j)
     &   ,hdc_pitch(HDC_PLANE_NUM(j)),hdc_center(HDC_PLANE_NUM(j)),hdc_central_wire(HDC_PLANE_NUM(j))
           enddo
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
