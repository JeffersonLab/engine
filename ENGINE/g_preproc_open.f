      SUBROUTINE g_preproc_open(ABORT,err)
*---------------------------------------------------------------------
*     opens file for output of preprocessed events in CODA format
*
*     Purpose and Methods:  Initialization is done status is returned.
*
*     output: ABORT      success or failure
*             err        reason for fault if any
*
*     created:  apr-29-1996 Dave Meekins
*
*
*     I don't know what other crap I need to fill in here
* $Log: g_preproc_open.f,v $
* Revision 1.1  1996/06/10 17:48:15  saw
* Initial revision
*
*---------------------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
*     variables
*
      character*40 here
      parameter(here='g_preproc_open')
      logical ABORT
      character*(*) err
      integer*4 status    !status
      integer*4 evopen    !CODA ouput file opening routine
      character*132 file
*
*     common files
*
      include 'gen_filenames.cmn'
      include 'gen_run_info.cmn'
*
* inits
*
      err=' '
      g_preproc_in_hndl=0
*
      file = g_preproc_filename
	write(6,*) file
      call g_sub_run_number(file,gen_run_number)

      status=evopen(file,'w',g_preproc_in_hndl)
      if(status.eq.0)then
        g_preproc_opened = .true.
      else
        call cemsg(status,0,err)
        g_preproc_opened = .false.
        write(6,*) 'could not open preprocessor output file '
        err=':error opening"'//file//'"'
        call G_add_path(here,err)
      endif
*
      RETURN
      END
