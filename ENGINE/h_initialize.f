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
*-    Revision 1.8  1994/05/12 19:34:06  cdaq
*-    (DFG) Add call to h_targ_trans_init
*-
* Revision 1.7  1994/04/13  04:31:00  cdaq
* (DFG) Add initialize for scin and cal
*
* Revision 1.6  1994/04/12  17:20:27  cdaq
* (KBB) Add ntuple call
*
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
      character*20  err1
      integer*4 istat
*
*
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*
      call h_generate_geometry          ! Tracking routine
*
*     calculate secondary scintillator and time of flight parameters
      call h_init_scin(ABORT,err)
      if(ABORT) then
         call g_add_path(here,err)
      endif
*
*     calculate secondary calorimeter parameters
      call h_init_cal(ABORT,err)
      if(ABORT) then
         call g_add_path(here,err)
      endif
*     read in Optical matrix elements
      call h_targ_trans_init(ABORT,err,istat)
      if(ABORT) then
         call g_build_note(':istat=@','@',istat,' ',1.,'(I3)',err1)
         call G_prepend(err1,err)
         call g_rep_err(ABORT,err)
         call g_add_path(here,err)
      endif

*
      call h_ntuple_init(ABORT,err)
*
      if(ABORT) then
         call g_add_path(here,err)
      endif
*
      return
      end
