       SUBROUTINE H_TRANS_SCIN(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Translate HMS raw scintilators 
*-                                to decoded information and start time
*-
*-      Required Input BANKS     HMS_RAW_SCIN
*-
*-      Output BANKS             HMS_DECODED_SCIN
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
*-   Created 19-JAN-1994   D. F. Geesaman
*-                           Dummy Shell routine
* $Log$
* Revision 1.1  1994/02/19 06:21:37  cdaq
* Initial revision
*
*-
*-
*--------------------------------------------------------
       IMPLICIT NONE
       SAVE
*
       character*50 here
       parameter (here= 'H_TRANS_SCIN')
*
       logical ABORT
       character*(*) err
*
       INCLUDE 'gen_data_structures.cmn'
       INCLUDE 'gen_constants.par'
       INCLUDE 'gen_units.par'
*
*--------------------------------------------------------
*
       ABORT= .FALSE.
       err= ':dummy routine!'
       RETURN
       END
