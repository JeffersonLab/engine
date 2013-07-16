      SUBROUTINE g_ntuple_init(ABORT,err)
*--------------------------------------------------------
*-       Close all ntuples
*-
*-
*-   Purpose and Methods : Close ntuples.
*-      Taken from ?_initialize so that s_initialize, h_initialize,
*-      and c_initialize can be called from event display without mucking
*-      with ntuples.
*- 
*-   Output: ABORT	- success or failure
*-         : err	- reason for failure, if any
*- 
*-Created  6-September-1995 SAW
* $Log: g_ntuple_init.f,v $
* Revision 1.1  1995/10/09 18:43:07  cdaq
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*13 here
      parameter (here= 'g_ntuple_init')
*
      logical ABORT
      character*(*) err
*
      character*500 why
      logical FAIL
*--------------------------------------------------------
      ABORT = .false.
      err = ' '
*
      call h_ntuple_init(FAIL,why)
      if(err.NE.' ' .and. why.NE.' ') then
        call G_append(err,' & '//why)
      elseif(why.NE.' ') then
        err= why
      endif
      ABORT= ABORT .or. FAIL
*
      call h_sv_nt_init(FAIL,why)
      if(err.NE.' ' .and. why.NE.' ') then
        call G_append(err,' & '//why)
      elseif(why.NE.' ') then
        err= why
      endif
      ABORT= ABORT .or. FAIL
*
      call s_ntuple_init(FAIL,why)
      if(err.NE.' ' .and. why.NE.' ') then
        call G_append(err,' & '//why)
      elseif(why.NE.' ') then
        err= why
      endif
      ABORT= ABORT .or. FAIL
*
      call s_sv_nt_init(FAIL,why)
      if(err.NE.' ' .and. why.NE.' ') then
        call G_append(err,' & '//why)
      elseif(why.NE.' ') then
        err= why
      endif
      ABORT= ABORT .or. FAIL
*
      call c_ntuple_init(FAIL,why)
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


