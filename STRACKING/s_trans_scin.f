       SUBROUTINE S_TRANS_SCIN(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Translate SOS raw scintilators 
*-                                to decoded information and start time
*-
*-      Required Input BANKS     SOS_RAW_SCIN
*-
*-      Output BANKS             SOS_DECODED_SCIN
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
*-   Created 19-JAN-1994   D. F. Geesaman
*-                           Dummy Shell routine
* $Log$
* Revision 1.1  1994/02/21 16:43:53  cdaq
* Initial revision
*
*-
*--------------------------------------------------------
       IMPLICIT NONE
       SAVE
*
       character*50 here
       parameter (here= 'S_TRANS_SCIN')
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
