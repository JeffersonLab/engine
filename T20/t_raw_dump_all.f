       SUBROUTINE  t_raw_dump_all(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Dump all raw T20 banks
*-
*-      Required Input BANKS     SOS_RAW_SCIN,SOS_RAW_CAL,SOS_RAW_DC
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
*-   Created 25-Jan-1997   S. A. Wood
* $Log$
* Revision 1.1  1998/12/01 20:57:09  saw
* Initial revision
*
*--------------------------------------------------------
       IMPLICIT NONE
       SAVE
*
       character*14 here
       parameter (here= 't_raw_dump_all')
*
       logical ABORT
       character*(*) err
*
       include 't20_data_structures.cmn'
       include 't20_hodo_parms.cmn'
       include 't20_tracking.cmn'
c       include 'sos_calorimeter.cmn'
*
*--------------------------------------------------------
       ABORT = .FALSE.
       err = ' '
*  Dump raw bank if tdebugprinthodoraw is set
       if( tdebugprinthodoraw .ne. 0) then
         call t_prt_raw_hodo(ABORT,err)
       endif
*
*      Dump raw bank if debug flag set
       if(tdebugprintmwpcraw.ne.0) then
         call t_prt_raw_mwpc(ABORT,err)
       endif
       RETURN
       END
