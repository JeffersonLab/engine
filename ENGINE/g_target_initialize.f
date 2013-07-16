      SUBROUTINE g_target_initialize(ABORT,err)
*--------------------------------------------------------
*-       Prototype target analysis routine
*-
*-
*-   Purpose and Methods : Initializes target quantities
*-
*-   Output: ABORAT          - success or failure
*-         : err             - reason for failure, if any
*-
*-
*- All standards are from "Proposal for Hall C Analysis Software
*- Vade Mecum, Draft 1.0" by D.F.Geesamn and S.Wood, 7 May 1993
*-
* $Log: g_target_initialize.f,v $
* Revision 1.1  1996/01/22 15:11:55  saw
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*18 here
      parameter (here= 'g_target_initialize')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'gen_data_structures.cmn'
      INCLUDE 'gen_constants.par'
*
*--------------------------------------------------------
*
      ABORT= .FALSE.

      if ((gtarg_num.lt.1).OR.(gtarg_num.gt.gmax_targets)) then
        print*, 'No target or invalid target number given'
        ABORT=.true.
      endif
*
      IF(ABORT) THEN
         err = 'No target or invalid target number given'
         call G_add_path(here,err)
      ELSE
         err= ' '
      ENDIF
*
      RETURN
      END
