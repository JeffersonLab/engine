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
* $Log: s_trans_dc.f,v $
* Revision 1.13  2002/07/31 20:20:58  saw
* Only try to fill user hists that are defined
*
* Revision 1.12  1996/09/04 20:18:35  saw
* (??) Cosmetic
*
* Revision 1.11  1996/01/17 18:44:30  cdaq
* (JRA) Change sign on sstart_time
*
* Revision 1.10  1995/10/11 13:54:18  cdaq
* (JRA) Cleanup, add bypass switch to s_dc_eff call
*
* Revision 1.9  1995/08/31 15:04:12  cdaq
* (JRA) Add call to s_dc_eff, warn about invalid plane numbers
*
* Revision 1.8  1995/05/22  19:46:02  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.7  1995/05/17  16:47:43  cdaq
* (JRA) Add hist for all dc tdc's in one histogram.
*
* Revision 1.6  1995/04/06  19:52:15  cdaq
* (JRA) SMAX_NUM_DC_PLANES -> SDC_NUM_PLANES
*
* Revision 1.5  1994/11/23  15:08:04  cdaq
* (SPB) Recopied from hms file and modified names for SOS
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
      character*10 here
      parameter (here= 'S_TRANS_DC')
*     
      logical ABORT
      character*(*) err
*     
      include 'sos_data_structures.cmn'
      include 'gen_constants.par'
      include 'gen_units.par'
      include 'sos_tracking.cmn'
      include 'sos_geometry.cmn'
      include 'sos_track_histid.cmn'
      include 'sos_bypass_switches.cmn'
*     
*--------------------------------------------------------
      real*4 s_drift_dist_calc
      external s_drift_dist_calc
      integer*4 ihit,goodhit,old_wire,old_pln,wire,pln,chamber
      real*4 histval
*     
      ABORT= .FALSE.
      err= ' '
      old_wire = -1
      old_pln = -1
      goodhit = 0
      if (sdc_center(1).eq.0.) then   !initialize hdc_center if not yet set.
        do pln = 1, sdc_num_planes
          chamber = sdc_chamber_planes(pln)
          sdc_center(pln) = sdc_xcenter(chamber)
     $         *sin(sdc_alpha_angle(pln))+sdc_ycenter(chamber)
     $         *cos(sdc_alpha_angle(pln))
        enddo
      endif
*     Are there any raw hits
      if(sdc_raw_tot_hits.gt.0) then
*     loop over all raw hits
        do ihit=1,sdc_raw_tot_hits
          pln = sdc_raw_plane_num(ihit)
          wire  = sdc_raw_wire_num(ihit)
*     check valid plane and wire number
          if(pln.gt.0 .and. pln.le. sdc_num_planes) then
            histval=float(sdc_raw_tdc(ihit))
            if(sidrawtdc.gt.0) call hf1(sidrawtdc,histval,1.)
*     test if tdc value less than lower limit for good hits
            if(sdc_raw_tdc(ihit) .lt. sdc_tdc_min_win(pln))  then
              swire_early_mult(wire,pln)
     $             = swire_early_mult(wire,pln)+1
            else
              if(sdc_raw_tdc(ihit) .gt. sdc_tdc_max_win(pln))  then
                swire_late_mult(wire,pln)
     $               = swire_late_mult(wire,pln)+1
              else
*     test for valid wire number
                if(wire.gt.0 .and. wire.le.sdc_nrwire(pln)) then
*     test for multiple hit on the same wire
                  if(pln.eq.old_pln .and. wire.eq.old_wire) then
                    swire_extra_mult(wire,pln) =
     $                   swire_extra_mult(wire,pln)+1
                  else

*     valid hit proceed with decoding
                    goodhit = goodhit + 1
                    sdc_plane_num(goodhit) = sdc_raw_plane_num(ihit)
                    sdc_wire_num(goodhit) = sdc_raw_wire_num(ihit)
                    sdc_tdc(goodhit) = sdc_raw_tdc(ihit)

* if sdc_wire_counting(pln) is 1 then wires are number in reverse order
                    if(sdc_wire_counting(pln).eq.0) then !normal ordering
                      sdc_wire_center(goodhit) = sdc_pitch(pln)
     &                  * (float(wire) - sdc_central_wire(pln))
     &                  - sdc_center(pln)
                    else
                      sdc_wire_center(goodhit) = sdc_pitch(pln)
     &                  * ( (sdc_nrwire(pln) + (1 - wire))
     &                  - sdc_central_wire(pln) ) - sdc_center(pln)
                    endif

                    sdc_drift_time(goodhit) = - sstart_time
     &                  - float(sdc_tdc(goodhit))*sdc_tdc_time_per_channel
     &                  + sdc_plane_time_zero(pln)

*  find dist in pattern_recognition, after apply propogation correction.
*                    sdc_drift_dis(goodhit) =
*     &                  s_drift_dist_calc(pln,wire,sdc_drift_time(goodhit))
                    sdc_hits_per_plane(pln)=sdc_hits_per_plane(pln)+1
                  endif                 ! end test on duplicate wire
                  old_pln = pln
                  old_wire = wire
                endif                   ! end test on valid wire number
              endif                     ! end test on hdc_tdc_max_win
            endif                       ! end test on hdc_tdc_min_win
          else                          ! if not a valid plane number
            write(6,*) 'S_TRANS_DC: invalid plane number = ',pln
          endif                         ! end test on valid plane number
        enddo                           ! end loop over raw hits

*     
*     set total number of good hits
*     
        sdc_tot_hits = goodhit
*
        if (sbypass_dc_eff.eq.0) call s_dc_eff !only call if there was a hit.
*
      endif                     !  end test on sdc_raw_tot_hits.gt.0
*
*
*     Dump decoded banks if flag is set
      if(sdebugprintdecodeddc.ne.0) then
        call s_print_decoded_dc(ABORT,err)
      endif
*     
      RETURN
      END
