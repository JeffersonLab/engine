      SUBROUTINE G_dump_histograms(ABORT,err)
*--------------------------------------------------------
*-    Routine to dump the histograms
*-
*-
*-   Output: ABORT		- success or failure
*-         : err	- reason for failure, if any
*- 
*-   Created  14-Jun-1994   S.A. Wood
* $Log: g_dump_histograms.f,v $
* Revision 1.3  2002/09/25 14:42:06  jones
* Replace call HREND(nametag) with call HRENDC(nametag) and close(IO)
*
* Revision 1.2  1995/04/01 19:45:37  cdaq
* (SAW) Allow %d for run number in filenames
*
* Revision 1.1  1994/06/14  19:13:42  cdaq
* Initial revision
*

      IMPLICIT NONE
      SAVE
*
      character*17 here
      parameter (here= 'G_dump_histograms')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'gen_filenames.cmn'
      INCLUDE 'gen_run_info.cmn'
*     
      character*132 file
      character*80 default_histfile
      parameter (default_histfile='engine_output.hbk')
      integer istat
      character*4 nametag
      parameter (nametag='RAWH')
      integer cycle,IO

      err = ' '
      IO= G_LUN_TEMP               ! temporary IO channel
      file= g_histout_filename		! File to write histograms into
      if(file.EQ.' ') file= default_histfile
      call g_sub_run_number(file,gen_run_number)
*
*      call G_open_HBOOK_file(IO,file,'NEW',ABORT,err) !FORTRAN file open
*
      call hropen(IO,nametag,file,'N',1024,ISTAT)
*
      ABORT = .false.                 ! Need to check ISTAT
*
      IF(.NOT.ABORT) THEN
*        call HRFILE(IO,nametag,'N')   !tell HBOOK to use channel IO (New)
        cycle= 0                      !dummy for internal counting
        call HROUT(0,cycle,' ')	      !CERNLIB flush buffers, all histograms
*        call HREND(nametag)           !done with this channel
        call HRENDC(nametag)           !done with this channel
        close (IO)
      ENDIF
*
      return
      end






