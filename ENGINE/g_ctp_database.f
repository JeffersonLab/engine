      subroutine g_ctp_database(ABORT, error, run, filename)
*
* USES LUN G_LUN_TEMP as a temporary LUN
*
************************************************************************
*     g_ctp_database(run, filename, ABORT)
* 
*     Find and execute in the given file, all the CTP parameter lines
*     associated  with the given run number.
*     Thiis is done by parsing the individual lines within <filename>,
*     where each line will be one of the following:
*
*     1. A comment.  These lines will start with the character ";", and 
*        will be ignored.
*     2. A run list.  These lines will have the following syntax:
*           <run_list> :: "<number>[-<number>] [,<run_list>]"
*        Note that for the time being, spaces are not allowed.
*     3. Text.  This is anything after the run list, and before the next 
*        run list (or to <EOF>).
* 
* Currently, there is *no* error handling.  I don't make mistakes.
* 
* Creation date: 25 May 1995 (JWP)
*
* Modification History:
* 
* 30 May 1995 (JWP): Two new features --
*                    (1) if run number is not found, set ABORT to .true.
*                    (2) Don't print lines starting with ";"; 
*                        furthermore, ignore embedded comments (i.e., 
*                        don't print out stuff following the ';').
*
* $Log: g_ctp_database.f,v $
* Revision 1.7  1999/11/04 20:35:15  saw
* Linux/G77 compatibility fixes
*
* Revision 1.6  1996/11/05 20:47:06  saw
* (SAW) Change in open statement for porting compatibility
*
* Revision 1.5  1996/09/04 14:33:40  saw
* (SAW) Use G_LUN_TEMP instead of 133 for linux compatibility
*
* Revision 1.4  1996/01/16 18:42:07  cdaq
* (JRA) Minor bug fixes
*
* Revision 1.3  1995/10/11 12:14:49  cdaq
* (JWP) Fix single run number at end of line bug.
*       Don't pass blank lines to thpset.
*
* Revision 1.2  1995/09/01 13:42:04  cdaq
* (JRA) Some corrections
*
* Revision 1.1  1995/07/27  19:08:06  cdaq
* Initial revision
*
************************************************************************
      implicit none
      SAVE
*
      include 'gen_filenames.cmn'
*
      character*14 here
      parameter (here='g_ctp_database')
*
      logical ABORT
      character*(*) error
      integer*4 run
      character*(*) filename

      logical debug
      logical parsing_run_list, spaces_stripped

      integer i
      integer index, number
      logical looking_for_run, found_run, printed_header
      character*132 line, newline
      integer*4 chan
*      integer*4 err
      integer*4 lo_limit, hi_limit

c      write (6,*) 'Debug?'
c      read (5, 1002) i
c      if (i .eq. 1) then
c         debug = .true.
c      else
       debug = .false.
c      endif
         
      if(debug) write(6,*) 'looking for run ',run
      found_run = .FALSE.
      looking_for_run = .TRUE.
      printed_header = .FALSE.

* Again, I don't make mistakes.
      ABORT = .FALSE.

      chan = G_LUN_TEMP

      open (unit=chan, status='old', file=filename)

      read (chan, 1001, end=9999) line
      index = 1
      if (debug) write (6,*) line
 111  do while (looking_for_run)

        do while (line(1:1) .eq. ';')
          read (chan, 1001, end=9999) line
          index = 1
          if (debug) write (6,*) line
        end do

        parsing_run_list = .true.
        do while (parsing_run_list)

* At this point, we should be looking at a run list.  The first thing in 
* the list will be a number, or it may be white space.  Skip the white 
* space and build the number.  After that, skip any white space at the
* end.
          number = 0

          do while ((index .lt. 132) .and.
     $         (line(index:index) .eq. ' '))
            if (debug) write (6,*) 'Found white space!'
            index = index + 1
          end do

          do while ((ichar(line(index:index)) .ge. ichar('0')) .and.
     $         (ichar(line(index:index)) .le. ichar('9')))
            number = 10*number +
     $           ichar(line(index:index)) - ichar('0')
            index = index + 1
          end do
          if (debug) write (6,*) 'Found number:',number

          do while ((index .lt. 132) .and.
     $         (line(index:index) .eq. ' '))
            if (debug) write (6,*) 'Found white space!'
            index = index + 1
          end do

************************************************************************
* Now, we are pointing at one of the following:
* 
*    1.  The end of the line.  This is flagged by (index .eq. 132).  In 
*        this case, check the number we found against <run>.
*    2.  A comma.  This indicates that the number we just built is not 
*        the end of the current line, but *is* the end of the current 
*        run list.  Check it against <run>.
*    3.  A dash ("-").  This indicates that the number we just built is 
*        the lower limit of the current run list.  Build the upper 
*        limit, and check to see if <run> is within the limits.


          if (index .eq. 132) then
            if (debug) write (6,*) 'End of the line!'
            parsing_run_list = .false.
            if (number .eq. run) then
              found_run = .true.
              looking_for_run = .false.
            end if
            
          else if (line(index:index) .eq. ',') then
            if (debug) write (6,*) 'NOT last number in list!'
            if (number .eq. run) then
              found_run = .true.
              looking_for_run = .false.
              parsing_run_list = .false.
            end if
            index = index + 1
          else if (line(index:index) .eq. '-') then
            if (debug) write (6,*) 'Range:'
            lo_limit = number
            number = 0
            index = index + 1
            do while ((ichar(line(index:index)) .ge. ichar('0'))
     $           .and. (ichar(line(index:index)) .le. ichar('9')))
              number = 10*number +
     $             ichar(line(index:index)) - ichar('0')
              index = index + 1
            end do
            hi_limit = number
            if (debug) write (6,*) lo_limit,'-',hi_limit
            if (line(index:index) .eq. ',') then
              index = index+1
            else if (line(index:index) .eq. ' ') then
              parsing_run_list = .false.
            end if
            if ((lo_limit .le. run) .and. (hi_limit .ge. run)) then
              found_run = .true.
              looking_for_run = .false.
              parsing_run_list = .false.
            end if
          else
            write(6,*) 'encountered unexpected character(s)' ! JRA
            stop
          end if
            
        end do
        if (looking_for_run) then
          if (debug) write (6,*)
     $         'Didn''t find run -- skipping to next run list!'
          read (chan, 1001, end=9999) line
          index = 1
          if (debug) write (6,*) line
          do while ((ichar(line(1:1)) .lt. ichar('0')) .or.
     $         (ichar(line(1:1)) .gt. ichar('9')))
            read (chan, 1001, end=9999) line
            index = 1
            if (debug) write (6,*) line
          end do
        end if
      end do

************************************************************************
* At this point, we've found the run number.  Print out the lines 
* following the run number, stripping the leading spaces, until we get 
* to another run list.
      read (chan, 1001, end=9999) line
      index = 1
      if (debug) write (6,*) line
      do while ((ichar(line(1:1)) .lt. ichar('0')) .or.
     $     (ichar(line(1:1)) .gt. ichar('9')))
        do i=1,132
          newline(i:i) = ' '
        end do
        spaces_stripped = .false.
        do while(.not. spaces_stripped)
          if (line(index:index) .eq. ' ') then
            index = index + 1
            if (index .gt. 132) then
              spaces_stripped = .true.
            end if
          else
            spaces_stripped = .true.
          end if
        end do
        if (index .le. 132) then
          if (line(index:index) .ne. ';') then
            i = 1
            do while ((index .lt. 132) .and.
     $           (line(index:index) .ne. ';'))
              newline(i:i) = line(index:index)
              index = index + 1
              i = i+1
            end do
            if(.not.printed_header) then
c     write(6,*)'g_ctp_database is setting the following CTP parameters'
              printed_header = .true.
            endif
            write(6,'(4x,a)') newline(1:70) ! Truncate to keep 1/line
            call thpset(newline)
          end if
        end if
        read (chan, 1001, end=9999) line
        index = 1
        if (debug) write (6,*) line
      end do
      
      looking_for_run = .true.
      parsing_run_list = .true.
      goto 111
* Done with open file.

 9999 close (unit=chan)
      if (.not. found_run) then
        write(6,*) 'cant find run ',run,' in "',filename,'"'
      end if

      return

*============================ Format Statements ===============================

 1001 format(a)
 1002 format(i10)

      end
