       SUBROUTINE H_initialize(ABORT,err)
*--------------------------------------------------------
*-       Prototype C analysis routine
*-
*-
*-   Purpose and Methods : Initializes HMS quantities 
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
*-   Created  8-Nov-1993   Kevin B. Beard
*-   Modified 20-Nov-1993   KBB for new errors
*-    $Log$
*-    Revision 1.6  1994/04/12 17:20:27  cdaq
*-    (KBB) Add ntuple call
*-
* Revision 1.5  1994/02/22  15:12:37  cdaq
* (DFG) Add call call to h_generate_geometry
*
* Revision 1.4  1994/02/11  18:35:40  cdaq
* Split off CTP variables registration from initialize routines
*
* Revision 1.3  1994/02/04  17:35:56  cdaq
* KBB replaced flag with title
*
*-
*- All standards are from "Proposal for Hall C Analysis Software
*- Vade Mecum, Draft 1.0" by D.F.Geesamn and S.Wood, 7 May 1993
*-
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*12 here
      parameter (here= 'H_initialize')
*
      logical ABORT
      character*(*) err
*
*
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*
      call h_generate_geometry          ! Tracking routine
*
      call h_ntuple_init(ABORT,err)
*
      if(ABORT) then
         call g_add_path(here,err)
      endif
*
      return
      end
