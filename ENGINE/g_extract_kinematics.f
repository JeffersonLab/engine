      subroutine g_extract_kinematics(ebeam,phms,thms,psos,tsos)
* $Log$
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
      integer j
      integer mode
      character*20 name
      real*4 value
      real*4 ebeam,phms,thms,psos,tsos

      include 'gen_craw.cmn'
*--------------------------------------------------------

c
c     Break up the text stored in the event array into individual lines.  If
c     the line is a comment, print it out, otherwise parse it.
c
      i = 1
      mode = 0            ! Mode 0 => Parse each line
      do
        j = find_char (buffer, i, 10) ! 10 = NewLine character
        if (i.eq.j) goto 10
        if (buffer(i:i+7).eq.'comment') then
          mode = 1      ! Mode 1 => Print out each line
          i = i + 8
        endif
        if (mode.eq.0) then
          call parse_line (buffer(i:j), j-i, name, value)
          if (name(1:11).eq.'Beam Energy') then
c            write(6,*) name(1:11),'=',value
            ebeam=value
          else if (name(1:12).eq.'HMS Momentum') then
c            write(6,*) name(1:11),'=',value
            phms=value
          else if (name(1:9).eq.'HMS Angle') then
c            write(6,*) name(1:11),'=',value
            thms=value
          else if (name(1:12).eq.'SOS Momentum') then
c            write(6,*) name(1:11),'=',value
            psos=value
          else if (name(1:9).eq.'SOS Angle') then
c            write(6,*) name(1:11),'=',value
            tsos=value
          else if (name(1:14).eq.'B2 Current Set') then  !last field with standard {} format
            goto 10
          endif
        else
          type *, buffer(i:j-1)
        endif
        i = j + 1
        enddo
10    continue
      return
      end

c
c     Parse a character string
c
      subroutine parse_line (line, line_len, name, value)
      implicit none
      character*132 line, tmpline
      integer line_len
      integer name_start,name_stop,value_start,value_stop
      character*20 name,value_string
      real*4 value

! sample line:   sangle {SOS Angle} sangle {29.4} {29.4}

c      write(6,*) line(1:line_len)
      name_start = index(line,'{')+1
      name_stop = index(line,'}')-1
      if (name_start.le.name_stop) name = line(name_start:name_stop)
      tmpline = line(name_stop+2:line_len)
      value_start = index(tmpline,'{')+1
      value_stop = index(tmpline,'}')-1
      if (value_start.le.value_stop) value_string = tmpline(value_start:value_stop)
      read (value_string,*,err=999) value

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
      character*132 string
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
      character*132 string
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
      character*132 string
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
      character*80 string
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
