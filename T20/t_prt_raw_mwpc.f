       SUBROUTINE  t_prt_raw_mwpc(ABORT,err)
*--------------------------------------------------------
*-
*-    Purpose and Methods : Dump POLDER_RAW_MWPC BANKS
*-
*-      Required Input BANKS     POLDER_RAW_MWPC
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
*-   Created 25-Jan-1997    S. A. Wood
* $Log$
* Revision 1.1  1998/12/01 20:57:33  saw
* Initial revision
*
*--------------------------------------------------------
       IMPLICIT NONE
       SAVE
*
       character*14 here
       parameter (here= 't_prt_raw_mwpc')
*
       logical ABORT
       character*(*) err
*
       integer*4 j
       include 't20_data_structures.cmn'
       include 'gen_constants.par'
       include 'gen_units.par'
       include 't20_tracking.cmn'
c       include 'hms_geometry.cmn'          
*
*--------------------------------------------------------
       ABORT = .FALSE.
       err = ' '
       write(tluno,'(''        POLDER_RAW_MWPC BANKS'')')
       write(tluno,'(''     TMWPC_RAW_TOT_HITS='',I4)') TMWPC_RAW_TOT_HITS
       if(TMWPC_RAW_TOT_HITS.GT.0) then
         write(tluno,'('' Num  Plane     Wire          TDC Value'')')
         write(tluno,'(1x,i2,2x,i3,7x,i4,5x,i10)')
     &     (j,TMWPC_RAW_PLANE_NUM(j),TMWPC_RAW_WIRE_NUM(j),
     &        TMWPC_RAW_TDC(j),j=1,TMWPC_RAW_TOT_HITS)    
       endif
       RETURN
       END
