       SUBROUTINE S_initialize(ABORT,err)
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
*-   Modified 20-Nov-1993  KBB for new errors
*-    $Log$
*-    Revision 1.7  1994/04/13 18:15:23  cdaq
*-    (DFG) Add scin and cal init
*-
* Revision 1.6  1994/04/12  17:31:09  cdaq
* (KBB) Add ntuple call
*
* Revision 1.5  1994/02/22  15:14:03  cdaq
* (DFG) Add calls to s_generate_geometry and s_initialize_fitting
*
* Revision 1.4  1994/02/11  18:36:35  cdaq
* Split off CTP variables registration from initialize routines
*
* Revision 1.3  1994/02/04  20:47:40  cdaq
* Add read titles to regpar calls
*
* Revision 1.2  1994/02/03  14:28:27  cdaq
* Make clear that last arg of reg calls is a title.  Use null for now.
*
* Revision 1.1  1994/02/02  21:37:55  cdaq
* Initial revision
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
      parameter (here= 'S_initialize')
*
      logical ABORT
      character*(*) err
*
*--------------------------------------------------------
      ABORT = .FALSE.
      err= ' '
*
      call s_generate_geometry          ! Tracking routine
*
      call s_initialize_fitting         ! Minuit initialization
*
*     calculate secondary scintillator and time of flight parameters
      call s_init_scin(ABORT,err)
*
*     calculate secondary calorimeter parameters
      call s_init_cal(ABORT,err)
*
      ABORT = .FALSE.
*
      call s_ntuple_init(ABORT,err)
*
      if(ABORT) then
         call g_add_path(here,err)
      endif
*
      return
      end
