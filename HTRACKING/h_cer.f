       SUBROUTINE H_CER(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Analyze Cerenkov information for each track 
*-
*-      Required Input BANKS     HMS_RAW_CER
*-                               HMS_FOCAL_PLANE
*-
*-      Output BANKS             HMS_TRACK_TESTS
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
*-   Created 19-JAN-1994   D. F. Geesaman
*-                           Dummy Shell routine
* $Log: h_cer.f,v $
* Revision 1.2  1995/05/22 19:39:06  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.1  1994/02/19  06:13:01  cdaq
* Initial revision
*
*-
*-
*--------------------------------------------------------
       IMPLICIT NONE
       SAVE
*
       character*50 here
       parameter (here= 'H_CER')
*
       logical ABORT
       character*(*) err
*
       INCLUDE 'hms_data_structures.cmn'
       INCLUDE 'gen_constants.par'
       INCLUDE 'gen_units.par'
*
*--------------------------------------------------------
*
       ABORT= .FALSE.
       err= ':dummy routine!'
       RETURN
       END
