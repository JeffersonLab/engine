       SUBROUTINE C_initialize(ABORT,err)
*--------------------------------------------------------
*-       Prototype C analysis routine
*-
*-
*-   Purpose and Methods : Initializes COIN quantities 
*-
*-   Output: ABORAT          - success or failure
*-         : err             - reason for failure, if any
*- 
*-   Created  8-Nov-1993   Kevin B. Beard
*-    $Log$
*-    Revision 1.1  1994/02/04 21:06:11  cdaq
*-    Initial revision
*-
* Revision 1.1  1994/02/04  21:06:11  cdaq
* Initial revision
*
*- 
*-
*- All standards are from "Proposal for Hall C Analysis Software
*- Vade Mecum, Draft 1.0" by D.F.Geesamn and S.Wood, 7 May 1993
       IMPLICIT NONE
       SAVE
*--------------------------------------------------------
       character*12 here
       parameter (here= 'C_initialize')
*
       logical ABORT
       character*(*) err
*
       INCLUDE 'gen_data_structures.cmn'
*
      INCLUDE 'gen_data_structures.cmn'
      INCLUDE 'gen_constants.par'
       ABORT= .FALSE.    !can't think of anything in COIN to initialize
      CEBEAM=SQRT(CPBEAM**2 + mass_electron**2)
       IF(ABORT) THEN
         err= ':failure!'
*
       ELSE
*
       ENDIF
         call G_add_path(here,err)
       RETURN
       END
      ENDIF
*
      RETURN
      END
