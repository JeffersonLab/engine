      SUBROUTINE t_initialize(ABORT,err)
*--------------------------------------------------------
*-       Prototype C analysis routine
*-
*-
*-   Purpose and Methods : Initializes T20 quantities 
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
*-   Created  22-Jan-1997  Stephen A. Wood
* $Log$
* Revision 1.1  1998/12/01 20:56:51  saw
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
      parameter (here= 't_initialize')
*
      include 't20_bypass_switches.cmn'
*

      logical ABORT
      character*(*) err
c      character*20 err1
c      integer*4 istat 
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
      call t_init_physics(FAIL,why)
      if(err.NE.' ' .and. why.NE.' ') then
        call G_append(err,' & '//why)
      elseif(why.NE.' ') then
        err= why
      endif
      ABORT= ABORT .or. FAIL

      if (tbypass_polder.ge.0 .and. tbypass_polder.lt.4) then
        call t_polder_initialize(FAIL,why)
        if(why.NE.' ') then
          err= why
        endif
        ABORT= ABORT .or. FAIL
      endif

*
*
c      call s_generate_geometry          ! Tracking routine
*
c      call s_initialize_fitting         ! Minuit initialization
*
*-calculate secondary scintillator and time of flight parameters
c      call s_init_scin(FAIL,why)
c      if(why.NE.' ') then
c        err= why
c      endif
c      ABORT= ABORT .or. FAIL
*
*SDISPLAY*     If one_ev flag on, initialize the event display package
*           if(one_ev.ne.0) call one_ev_init  !One event display unit
*
*
*-read in Optical matrix elements
c      call s_targ_trans_init(FAIL,why,istat)
c      if(FAIL) then
c        write(err1,'(":istat=",i2)') istat
c        call G_prepend(err1,why)
c      endif
c      if(err.NE.' ' .and. why.NE.' ') then   !keep warnings
c        call G_append(err,' & '//why)
c      elseif(why.NE.' ') then
c        err= why
c      endif
c      ABORT= ABORT .or. FAIL
*
      if(ABORT .or. err.NE.' ') call g_add_path(here,err)
*
      return
      end
