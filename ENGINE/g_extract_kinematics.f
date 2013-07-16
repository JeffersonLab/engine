      subroutine g_extract_kinematics(ebeam,phms,thms,psos,tsos,ntarg)
* $Log: g_extract_kinematics.f,v $
* Revision 1.6  2003/09/05 15:41:05  jones
* Merge in online03 changes (mkj)
*
* Revision 1.5.2.2  2003/04/10 00:41:51  cdaq
* Have the engine take the spectrometer angles from the typed field of the run_info event rather than the epics field
*
* Revision 1.5.2.1  2003/04/09 02:47:57  cdaq
* Update code to look for Target Material instead of Target NUMBER in run info event
*
* Revision 1.5  2002/09/25 14:38:47  jones
*    a. IN subroutine parse_line
*       i.  character*132 line changed to character*(*) line
*       ii. character*20 name changed to character*(*) name
*    b. in function skip_blanks character*132 string changed to  character*(*) string
*
* Revision 1.4  1998/12/01 15:59:47  saw
* (SAW) Make "string" argument of skip_item variable length
*
* Revision 1.3  1996/09/04 14:36:19  saw
* (JRA) Fixes
*
* Revision 1.2  1996/01/16 18:35:02  cdaq
* (JRA) Minor bug fix
*
* Revision 1.1  1995/11/28 19:10:48  cdaq
* Initial revision
*
*--------------------------------------------------------
      implicit none
      save

      character buffer*2000
      equivalence (craw(5), buffer)
      integer find_char
      integer i
      integer j,evlen
      integer mode
      character*20 name
      real*4 epics_value,typed_value
      real*4 ebeam,phms,thms,psos,tsos,ntarg
      integer g_important_length

      include 'gen_craw.cmn'
*--------------------------------------------------------

c
c     Break up the text stored in the event array into individual lines.  If
c     the line is a comment, print it out, otherwise parse it.
c

      evlen=g_important_length(buffer(1:4*(craw(3)-1)))
      i = 1
      mode = 0            ! Mode 0 => Parse each line
      do while (i.le.evlen)
        j = find_char (buffer, i, 10)   ! 10 = NewLine character
        if (i.eq.j) goto 20
        if (buffer(i:i+7).eq.'comment') then
          mode = 1                      ! Mode 1 => Print out each line
          i = i + 24                    ! comment has 24 character of 'header'
        endif
        if (mode.eq.0) then
          call parse_line (buffer(i:j), j-i, name, epics_value, typed_value)
          if (name(1:11).eq.'Beam Energy') then
            ebeam=typed_value
          else if (name(1:12).eq.'HMS Momentum') then
            phms=typed_value
          else if (name(1:9).eq.'HMS Angle') then
            thms=typed_value
          else if (name(1:12).eq.'SOS Momentum') then
            psos=typed_value
          else if (name(1:9).eq.'SOS Angle') then
            tsos=typed_value
          else if (name(1:15).eq.'Target Material') then
            ntarg=typed_value
          endif
        else
          if(i.lt.j-1) write(6,'(4x,a)') buffer(i:j-1)
        endif
 20     i = j + 1
      enddo
 10   continue
      return
      end

c
c     Parse a character string
c
      subroutine parse_line (line, line_len, name, value1, value2)
      implicit none
      character*(*) line
      character*132 tmpline
      integer line_len,new_len
      integer name_start,name_stop
      integer value1_start,value1_stop
      integer value2_start,value2_stop
      character*(*) name
      character*20 value_string
      real*4 value1,value2

! sample line:   sangle {SOS Angle} sangle {29.4} {29.4}

      name_start = index(line,'{')+1
      name_stop = index(line,'}')-1
      if (name_start.le.name_stop) name = line(name_start:name_stop)
      if (name_stop+2.gt.line_len) goto 999
      tmpline = line(name_stop+2:line_len)
      new_len = line_len - name_stop - 1
      value1_start = index(tmpline,'{')+1
      value1_stop = index(tmpline,'}')-1
      if (value1_start.le.value1_stop) value_string = tmpline(value1_start:value1_stop)
      read (value_string,*,err=999) value1
      if (value1_stop+2.gt.new_len) goto 999
      tmpline = tmpline(value1_stop+2:new_len)
      value2_start = index(tmpline,'{')+1
      value2_stop = index(tmpline,'}')-1
      if (value2_start.le.value2_stop) value_string = tmpline(value2_start:value2_stop)
      read (value_string,*,err=999) value2

999   continue        !if non-numerical vaule, skip reading
      return
      end

c
c     Return the offset of the next item
c
      integer function skip_item (string, i)
      character*132 string
      integer i, j, find_char, skip_blanks, skip_nonblanks
      j = skip_blanks (string, i)
      if (string(j:j).eq.'{') then
         j = find_char (string, j, ichar('}')) + 1
      else
         j = skip_nonblanks (string, j)
      endif
      j = skip_blanks (string, j)
      skip_item = j
      return
      end

c
c     Find the offset of character "char_num" in text string "string"
c
      integer function find_char (string, i, char_num)
      character*(*) string
      integer i, j, char_num
      j = i
      do while ((ichar(string(j:j)).ne.char_num).and.(ichar(string(j:j)).ne.0))
         j = j + 1
      enddo
      find_char = j
      return
      end

c
c     Return the offset of the first nonblank character in "string"
c
      integer function skip_blanks (string, i)
      character*(*) string
      integer i, j
      j = i
      do while ((string(j:j).eq.' ').and.(ichar(string(j:j)).ne.0))
         j = j + 1
      enddo
      skip_blanks = j
      return
      end

c
c     Return the offset of the first blank character in "string"
c
      integer function skip_nonblanks (string, i)
      character*(*) string
      integer i, j
      j = i
      do while ((string(j:j).ne.' ').and.(ichar(string(j:j)).ne.0))
         j = j + 1
      enddo
      skip_nonblanks = j
      return
      end

c
c     Convert a text character to a digit
c       If the character is not a digit and is not a ".", return -1
c       If the character is a ".", return -2
c
      real function char_to_digit (letter)
      character letter
      char_to_digit = ichar(letter) - 48
      if ((char_to_digit.lt.0).or.(char_to_digit.gt.9)) char_to_digit = -1
      if (letter.eq.'.') char_to_digit = -2
      return
      end

c
c     Convert a text string to a real number
c
      real function string_to_number (string)
      character*(*) string
      real offset, digit, sign, number
      integer starting_offset
      starting_offset = 1
      sign = 1
      if (string(1:1).eq.'+') starting_offset = 2
      if (string(1:1).eq.'-') then
         starting_offset = 2
         sign = -1
      endif
      number = 0
      offset = 0.1
      do i = starting_offset, 79
         digit = char_to_digit (string(i:i))
         if (digit.lt.0) goto 10
         number = number*10 + digit
      enddo
 10   continue
      if (digit.eq.-2) then
         do j = i+1, 79
            digit = char_to_digit (string(j:j))
            if (digit.lt.0) goto 20
            number = number + offset*digit
            offset = offset/10
         enddo
 20      continue
      endif
      number = number*sign
      string_to_number = number
      return
      end
