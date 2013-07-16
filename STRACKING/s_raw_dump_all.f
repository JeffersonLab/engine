       SUBROUTINE  s_raw_dump_all(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Dump all raw SOS banks
*-
*-      Required Input BANKS     SOS_RAW_SCIN,SOS_RAW_CAL,SOS_RAW_DC
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
*-   Created 5-APR-1994   D. F. Geesaman
* $Log: s_raw_dump_all.f,v $
* Revision 1.2  1995/05/22 19:45:53  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.1  1994/04/13  16:07:03  cdaq
* Initial revision
*
*--------------------------------------------------------
       IMPLICIT NONE
       SAVE
*
       character*50 here
       parameter (here= 's_raw_dump_all')
*
       logical ABORT
       character*(*) err
*
       include 'sos_data_structures.cmn'
       include 'sos_scin_parms.cmn'
       include 'sos_tracking.cmn'
       include 'sos_calorimeter.cmn'
*
*--------------------------------------------------------
       ABORT = .FALSE.
       err = ' '
*  Dump raw bank if sdebugprintscinraw is set
        if( sdebugprintscinraw .ne. 0) then
          call s_prt_raw_scin(ABORT,err)
        endif
*
*
        if(sdbg_raw_cal.gt.0) call s_prt_cal_raw
*       call s_prt_raw_cer
*      Dump raw bank if debug flag set
       if(sdebugprintrawdc.ne.0) then
          call s_print_raw_dc(ABORT,err)
       endif
       RETURN
       END
