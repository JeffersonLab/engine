      subroutine g_examine_epics_event
* $Log$
* Revision 1.2  1996/11/05 21:40:32  saw
* (JRA) Print out just first epics event
*
* Revision 1.1  1996/08/12 18:30:13  saw
* Initial revision
*
*--------------------------------------------------------
      implicit none
      save

      character buffer*12000
      equivalence (craw(5), buffer)
      integer i,j,evlen
      integer g_important_length,find_char
      integer numevent
      logical dump_event

      include 'gen_craw.cmn'
      include 'gen_run_info.cmn'
      include 'gen_filenames.cmn'

*--------------------------------------------------------

      numevent = numevent + 1

      if (g_epics_output_filename.ne.' ' .and.
     &   (gdebugdumpepics.eq.1 .or. numevent.le.2)) then  !write out event
        dump_event = .true.
      else
        dump_event = .false.
      endif

      if (dump_event) write (G_LUN_EPICS_OUTPUT,*) 'epics event #',numevent
      evlen=g_important_length(buffer(1:4*(craw(3)-1)))
      i = 1
      do while (i.le.evlen)
        j = find_char (buffer, i, 10)   ! 10 = NewLine character
        if (i.eq.j) goto 20
        if(i.lt.j-1 .and. dump_event) write(G_LUN_EPICS_OUTPUT,'(4x,a)') buffer(i:j-1)
 20     i = j + 1
      enddo

      return
      end
