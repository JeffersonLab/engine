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
* $Log: s_initialize.f,v $
* Revision 1.16  1996/09/04 15:17:01  saw
* (JRA) Reorder calls
*
* Revision 1.15  1996/01/16 16:12:41  cdaq
* (JRA) Comment out SOS minuit initialization
*
* Revision 1.14  1995/10/09 18:47:01  cdaq
* (SAW) Move ntuple initialization into g_ntuple_init
*
* Revision 1.13  1995/08/11 15:37:05  cdaq
* (DD) Add sos sieve slit ntuple
*
* Revision 1.12  1994/11/22  20:15:10  cdaq
* (SAW) Cosmetic change
*
* Revision 1.11  1994/06/17  04:02:58  cdaq
* (KBB) Upgrade error reporting
*
* Revision 1.10  1994/06/16  03:46:17  cdaq
* *** empty log message ***
*
* Revision 1.9  1994/06/14  04:03:48  cdaq
* (DFG) Add call to s_init_physics
*
* Revision 1.8  1994/05/13  03:13:45  cdaq
* (DFG) Add call to s_targ_trans_init
*
* Revision 1.7  1994/04/13  18:15:23  cdaq
* (DFG) Add scin and cal init
*
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
      character*20 err1
      integer*4 istat 
*
      logical FAIL
      character*1000 why
*SDISPLAY*
*SDISPLAY   include 'one_ev_io.cmn'
*
*--------------------------------------------------------
      ABORT = .FALSE.
      err= ' '
*
*-calculate physics singles constants
      call s_init_physics(FAIL,why)
      if(err.NE.' ' .and. why.NE.' ') then
        call G_append(err,' & '//why)
      elseif(why.NE.' ') then
        err= why
      endif
      ABORT= ABORT .or. FAIL
*
*
      call s_generate_geometry          ! Tracking routine
*
c      call s_initialize_fitting         ! Minuit initialization
*
*-calculate secondary scintillator and time of flight parameters
      call s_init_scin(FAIL,why)
      if(why.NE.' ') then
        err= why
      endif
      ABORT= ABORT .or. FAIL
*
*-calculate secondary cerenkov parameters
      call s_init_cer(FAIL,why)
      if(err.NE.' ' .and. why.NE.' ') then
        call G_append(err,' & '//why)
      elseif(why.NE.' ') then
        err= why
      endif
      ABORT= ABORT .or. FAIL
*
*-calculate secondary calorimeter parameters
      call s_init_cal(FAIL,why)
      if(err.NE.' ' .and. why.NE.' ') then   !keep warnings
        call G_append(err,' & '//why)
      elseif(why.NE.' ') then
        err= why
      endif
      ABORT= ABORT .or. FAIL
*SDISPLAY*     If one_ev flag on, initialize the event display package
*           if(one_ev.ne.0) call one_ev_init  !One event display unit
*
*
*-read in Optical matrix elements
      call s_targ_trans_init(FAIL,why,istat)
      if(FAIL) then
        write(err1,'(":istat=",i2)') istat
        call G_prepend(err1,why)
      endif
      if(err.NE.' ' .and. why.NE.' ') then   !keep warnings
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
