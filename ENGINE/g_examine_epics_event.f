      subroutine g_examine_epics_event
* $Log$
* Revision 1.1  1996/08/12 18:30:13  saw
* Initial revision
*
*--------------------------------------------------------
      implicit none
      save

      character buffer*12000
      equivalence (craw(5), buffer)
      integer i
      integer j,evlen
      integer g_important_length,find_char
      integer numevent

      include 'gen_craw.cmn'
      include 'gen_run_info.cmn'
*--------------------------------------------------------

c
c     Break up the text stored in the event array into individual lines.  If
c     the line is a comment, print it out, otherwise parse it.
c
      numevent = numevent + 1
      if (gdebugdumpepics.ne.0) write (99,*) 'EPICS EVENT #',numevent

      evlen=g_important_length(buffer(1:4*(craw(3)-1)))
      i = 1
      do while (i.le.evlen)
        j = find_char (buffer, i, 10)   ! 10 = NewLine character
        if (i.eq.j) goto 20
        if(i.lt.j-1 .and. gdebugdumpepics.ne.0) write(99,'(4x,a)') buffer(i:j-1)
 20     i = j + 1
      enddo
 10   continue
      return
      end
