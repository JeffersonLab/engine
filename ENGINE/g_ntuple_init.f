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
* $Log$
* Revision 1.1.24.3  2007/08/22 19:09:16  frw
* added FPP
*
* Revision 1.1.25 frw
* added call for FPP
*
* Revision 1.1.24.2  2007/06/04 14:56:05  puckett
* changed hit array structure for trigger related signals
*
* Revision 1.1.24.1  2007/05/15 02:55:01  jones
* Start to Bigcal code
*
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

      include 'gen_run_info.cmn'

*--------------------------------------------------------
      ABORT = .false.
      err = ' '
*
c      write(*,*) 'about to call h_ntuple_init'
      
      call h_ntuple_init(FAIL,why)
      
      if(err.NE.' ' .and. why.NE.' ') then
        call G_append(err,' & '//why)
      elseif(why.NE.' ') then
        err= why
      endif
      ABORT= ABORT .or. FAIL
*
      call h_fpp_nt_init(FAIL,why)
      if(err.NE.' ' .and. why.NE.' ') then
	call G_append(err,' & '//why)
      elseif(why.NE.' ') then
	err= why
      endif
      ABORT= ABORT .or. FAIL
*     
c     write(*,*) 'about to call h_sv_ntuple_init'
      call h_sv_nt_init(FAIL,why)
      if(err.NE.' ' .and. why.NE.' ') then
        call G_append(err,' & '//why)
      elseif(why.NE.' ') then
        err= why
      endif
      ABORT= ABORT .or. FAIL
*
      
c     write(*,*) 'about to call s_ntuple_init'
      call s_ntuple_init(FAIL,why)
      if(err.NE.' ' .and. why.NE.' ') then
        call G_append(err,' & '//why)
      elseif(why.NE.' ') then
        err= why
      endif
      ABORT= ABORT .or. FAIL
*     
c     !write(*,*) 'about to call s_sv_nt_init'
      call s_sv_nt_init(FAIL,why)
      if(err.NE.' ' .and. why.NE.' ') then
        call G_append(err,' & '//why)
      elseif(why.NE.' ') then
        err= why
      endif
      ABORT= ABORT .or. FAIL
*     
      
      !write(*,*) 'about to call b_ntuple_init'
      call b_ntuple_init(FAIL,why)
      !write(*,*) 'b_ntuple_init successful'
      if(err.ne.' '.and.why.ne.' ')then
        call G_append(err,' & '//why)
      elseif(why.ne.' ') then
        err = why
      endif
      ABORT = ABORT .or. FAIL
     
*     
      
c     !write(*,*) 'about to call c_ntuple_init'
      call c_ntuple_init(FAIL,why)
      if(err.NE.' ' .and. why.NE.' ') then
        call G_append(err,' & '//why)
      elseif(why.NE.' ') then
        err= why
      endif
      ABORT= ABORT .or. FAIL
      
*     
      
C     !write(*,*) 'about to call gep_ntuple_init'
      call gep_ntuple_init(FAIL,why)
      if(err.ne.' '.and.why.ne.' ') then
        call G_append(err,' & '//why)
      else if(why.ne.' ') then
        err=why
      endif
      
*
      if(ABORT .or. err.NE.' ') call g_add_path(here,err)
*
      return
      end


