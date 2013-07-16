       SUBROUTINE S_CER(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Analyze Cerenkov information for each track 
*-
*-      Required Input BANKS     SOS_RAW_CER
*-                               SOS_FOCAL_PLANE
*-
*-      Output BANKS             SOS_TRACK_TESTS
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
*-   Created 19-JAN-1994   D. F. Geesaman
*-                           Dummy Shell routine
* $Log: s_cer.f,v $
* Revision 1.2  1995/05/22 19:45:33  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.1  1994/02/21  16:07:11  cdaq
* Initial revision
*
*-
*-
*--------------------------------------------------------
       IMPLICIT NONE
       SAVE
*
       character*50 here
       parameter (here= 'S_CER')
*
       logical ABORT
       character*(*) err
*
       INCLUDE 'sos_data_structures.cmn'
       INCLUDE 'gen_constants.par'
       INCLUDE 'gen_units.par'
*
*--------------------------------------------------------
*
       ABORT= .FALSE.
       err= ':dummy routine!'
       RETURN
       END
