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
*-    Revision 1.5  1995/05/22 20:50:43  cdaq
*-    (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*-
* Revision 1.4  1995/05/11  13:44:50  cdaq
* (SAW) Add calculation of s from beam and target info
*
* Revision 1.3  1994/06/14  03:16:09  cdaq
* (DFG) Add CEBEAM calculation
*
* Revision 1.2  1994/04/12  17:08:54  cdaq
* (KBB) Add ntuple call
*
* Revision 1.1  1994/02/04  21:06:11  cdaq
* Initial revision
*
*- 
*-
*- All standards are from "Proposal for Hall C Analysis Software
*- Vade Mecum, Draft 1.0" by D.F.Geesamn and S.Wood, 7 May 1993
*-
*-
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*12 here
      parameter (here= 'C_initialize')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'gen_data_structures.cmn'
**      INCLUDE 'coin_data_structures.cmn'
      INCLUDE 'gen_constants.par'
*
*--------------------------------------------------------
*
      ABORT= .FALSE.
*
      CEBEAM=SQRT(CPBEAM**2 + mass_electron**2)
      g_beam_target_s = (TMASS_TARGET + CEBEAM)**2 - CPBEAM**2
*
      call c_ntuple_init(ABORT,err)
*
      IF(ABORT) THEN
         call G_add_path(here,err)
      ELSE
         err= ' '
      ENDIF
*
      RETURN
      END
