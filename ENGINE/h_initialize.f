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
*-    Revision 1.13  1995/09/01 13:37:27  cdaq
*-    (JRA) Initialize Cerenkov parameters
*-
* Revision 1.12  1995/01/27  20:09:07  cdaq
* (SAW) Add call to sieve slit ntuple initialization
*
* Revision 1.11  1994/10/11  18:44:11  cdaq
* (SAW) Add hacks for event display
*
* Revision 1.10  1994/06/17  04:01:35  cdaq
* (KBB) Upgrade error reporting
*
* Revision 1.9  1994/06/14  04:02:13  cdaq
* (DFG) Add call to h_init_physics
*
* Revision 1.8  1994/05/12  19:34:06  cdaq
* (DFG) Add call to h_targ_trans_init
*
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
      character*20  mss
      integer*4 istat
*
      logical FAIL
      character*1000 why
*HDISPLAY*
*HDISPLAY      include 'one_ev_io.cmn'
*
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*
      call h_generate_geometry          ! Tracking routine
*
*-calculate secondary scintillator and time of flight parameters
      call h_init_scin(FAIL,why)
      if(why.NE.' ') then
        err= why
      endif
      ABORT= ABORT .or. FAIL
*
*-calculate secondary cerenkov parameters
      call h_init_cer(FAIL,why)
      if(err.NE.' ' .and. why.NE.' ') then
        call G_append(err,' & '//why)
      elseif(why.NE.' ') then
        err= why
      endif
      ABORT= ABORT .or. FAIL
*
*-calculate secondary calorimeter parameters
      call h_init_cal(FAIL,why)
      if(err.NE.' ' .and. why.NE.' ') then
        call G_append(err,' & '//why)
      elseif(why.NE.' ') then
        err= why
      endif
      ABORT= ABORT .or. FAIL
*
*HDISPLAY*     If one_ev flag on, initialize the event display package
*HDISPLAY      if(one_ev.ne.0) call one_ev_init  ! One event display init
*
*-read in Optical matrix elements
      call h_targ_trans_init(FAIL,why,istat)
      if(FAIL) then
         call g_build_note(';istat=@','@',istat,' ',1.,'(I3)',mss)
         call G_append(why,mss)
      endif
      if(err.NE.' ' .and. why.NE.' ') then  !keep warnings
        call G_append(err,' & '//why)
      elseif(why.NE.' ') then
        err= why
      endif
      ABORT= ABORT .or. FAIL
*
*-calculate physics singles constants
      call h_init_physics(FAIL,why)
      if(err.NE.' ' .and. why.NE.' ') then  !keep warnings
        call G_append(err,' & '//why)
      elseif(why.NE.' ') then
        err= why
      endif
      ABORT= ABORT .or. FAIL
*
      call h_ntuple_init(FAIL,why)
      if(err.NE.' ' .and. why.NE.' ') then
        call G_append(err,' & '//why)
      elseif(why.NE.' ') then
        err= why
      endif
      ABORT= ABORT .or. FAIL
*
      if(ABORT .or. err.NE.' ') call g_add_path(here,err)
*
      call h_sv_nt_init(FAIL,why)
      if(err.NE.' ' .and. why.NE.' ') then
        call G_append(err,' & '//why)
      elseif(why.NE.' ') then
        err= why
      endif
      ABORT= ABORT .or. FAIL
*
      if(ABORT .or. err.NE.' ') call g_add_path(here,err)
*
      return
      end
