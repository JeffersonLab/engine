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
* $Log$
* Revision 1.7  1996/01/22 15:04:19  saw
* (JRA) Change cebeam and cpbeam to gebeam and gpbeam
*
* Revision 1.6  1996/01/16 20:59:39  cdaq
* no change
*
* Revision 1.5  1995/05/22 20:50:43  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
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

*
      REAL*4 celoss
*
*--------------------------------------------------------
*
      ABORT= .FALSE.
*
      gebeam=sqrt(gpbeam**2 + mass_electron**2)
      if(gtarg_z(gtarg_num).gt.0.)then
        call total_eloss(0,.true.,gtarg_z(gtarg_num),
     $       gtarg_a(gtarg_num),gtarg_thick(gtarg_num),
     $       gtarg_dens(gtarg_num),
     $       0.0,gtarg_theta,1.0,celoss)
      else
        celoss=0.
      endif
      gebeam = gebeam - celoss
      g_beam_target_s = (gtarg_mass(gtarg_num) + gebeam)**2 - gpbeam**2
*
      IF(ABORT) THEN
         call G_add_path(here,err)
      ELSE
         err= ' '
      ENDIF
*
      RETURN
      END
