      subroutine g_ctp_database(ABORT, error, run, filename)
*
* USES LUN 133 as a temporary LUN
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
* $Log$
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
      character*14 here
      parameter (here='g_ctp_database')
*
      logical ABORT
      character*(*) error
      integer*4 run
      character*(*) filename

      logical debug
      logical parsing_run_list

      integer i
      integer index, number
      logical found_run, printed_header
      character*132 line, newline
      integer*4 chan
*      integer*4 err
      integer*4 lo_limit, hi_limit

      debug = .false.
         
      if(debug) print *,'looking for run ',run
      found_run = .FALSE.
      printed_header = .FALSE.

* Again, I don't make mistakes.
      ABORT = .FALSE.

      chan = 133

      open (unit=chan, status='old', name=filename)

      read (chan, 1001, end=9999) line
      index = 1
      if (debug) write (6,*) line
      do while (.not. found_run)

        do while (line(1:1) .eq. ';')
          read (chan, 1001, end=9999) line
          index = 1
          if (debug) write (6,*) line
        end do

        parsing_run_list = .true.
        do while (parsing_run_list)

* At this point, we should be looking at a run list.  The first thing in 
* the list will be a number, or it may be white space.  Skip the white 
* space and build the number.
          number = 0

          do while ((index .le. 132) .and.
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

* JRA - or nothing, if the line just contained one number.  NOT CHEKCED FOR!!!!

          if (index .eq. 132) then
            if (debug) write (6,*) 'End of the line!'
            parsing_run_list = .false.
            if (number .eq. run) then
              found_run = .true.
            end if
            
          else if (line(index:index) .eq. ',') then
            if (debug) write (6,*) 'NOT last number in list!'
            if (number .eq. run) then
              found_run = .true.
              parsing_run_list = .false.
            end if
            index = index + 1

** Remember, I don't make mistakes.  If we get here, it's a dash.

* JRA - a dash, or just the end of the line if there is only a single number
* at the end of the line, rather than a range
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
              parsing_run_list = .false.
            end if
          else
            print *, 'what do ya know, I make a mistake' ! JRA  
          end if
            
        end do
        if (.not. found_run) then
          if (debug) write (6,*)
     $         'Didn''t find run -- skipping to next run list!'
          read (chan, 1001, end=9999) line
          index = 1
          if (debug) write (6,*) line
          do while ((ichar(line(1:1)) .le. ichar('0')) .or.
     $         (ichar(line(1:1)) .ge. ichar('9')))
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

* JRA - Note, this does not work if there is a blank line after
* the command list.

      
      read (chan, 1001, end=9999) line
      index = 1
      if (debug) write (6,*) line
      do while ((ichar(line(1:1)) .lt. ichar('0')) .or.
     $     (ichar(line(1:1)) .gt. ichar('9')))
        do i=1,132
          newline(i:i) = ' '
        end do
        do while (line(index:index) .eq. ' ')
          index = index + 1
        end do
        if (line(index:index) .ne. ';') then
          i = 1
          do while ((index .le. 132) .and.
     $         (line(index:index) .ne. ';'))
            newline(i:i) = line(index:index)
            index = index + 1
            i = i+1
          end do
          if(.not.printed_header) then
            print *
     $           ,'g_ctp_database is setting the following CTP parameters'
            printed_header = .true.
          endif
          print *,newline(1:79)         ! Truncate to keep 1/line
          call thpset(newline)
        end if
        read (chan, 1001, end=9999) line
        index = 1
        if (debug) write (6,*) line
      end do
      
* Done with open file.

 9999 close (unit=chan)
      if (.not. found_run) then
        ABORT = .true.
        error = here//':can''t find desired run in "'//filename//'"'
      end if

      return

*============================ Format Statements ===============================

 1001 format(a)
 1002 format(i)

      end
