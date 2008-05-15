      SUBROUTINE g_ntuple_shutdown(ABORT,err)
*--------------------------------------------------------
*-       Close all ntuples
*-
*-
*-   Purpose and Methods : Close ntuples.
*-          Taken from ?_keep_results routines so that g_keep_results can
*-          be called without closing out ntuples.
*- 
*-   Output: ABORT	- success or failure
*-         : err	- reason for failure, if any
*- 
*-   Created  30-June-1995 SAW
*  $Log$
*  Revision 1.2.24.2.2.1  2008/05/15 18:59:22  bhovik
*  1'st version
*
*  Revision 1.2.24.2  2007/08/22 19:09:16  frw
*  added FPP
*
*  Revision 1.2.25 frw
*  added HMS FPP call
*
*  Revision 1.2.24.1  2007/05/15 02:55:01  jones
*  Start to Bigcal code
*
*  Revision 1.2  1995/09/01 15:46:13  cdaq
*  (JRA) Add call to sos sieve slit ntuple
*
* Revision 1.1  1995/07/27  19:00:55  cdaq
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*17 here
      parameter (here= 'g_ntuple_shutdown')
*
      logical ABORT
      character*(*) err
*--------------------------------------------------------
      call h_ntuple_shutdown(ABORT,err)
*
      call h_fpp_nt_shutdown(ABORT,err)
*
      call h_sv_nt_shutdown(ABORT,err)
*
      call s_ntuple_shutdown(ABORT,err)
*
      call s_sv_nt_shutdown(ABORT,err)
*
      call c_ntuple_shutdown(ABORT,err)
*
      call b_ntuple_shutdown(ABORT,err)
*  
      call sane_ntup_shutdown(ABORT,err)
*  
      call gep_ntuple_shutdown(ABORT,err)
*
      return
      end


