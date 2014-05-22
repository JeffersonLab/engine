      subroutine g_examine_epics_event
* $Log: g_examine_epics_event.f,v $
* Revision 1.5  2003/09/05 15:28:05  jones
* Merge in online03 changes (mkj)
*
* Revision 1.4.2.1  2003/08/14 00:17:55  cdaq
* Modifly so that gdebugdumpepics=1,2,3 means dump 30 sec epics varaibles,
* 2 sec epics variables, or both. (mkj)
*
* Revision 1.4  1999/06/10 14:41:03  csa
* (JRA) Added dump for numevent up to 10
*
* Revision 1.3  1998/12/01 15:55:40  saw
* (SAW) Print out error when event has no data
*
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
      integer evtype
      integer numevent
      logical dump_event
      integer*4 jishft

      include 'gen_craw.cmn'
      include 'gen_run_info.cmn'
      include 'gen_filenames.cmn'
*
* event type =131 30 second epics read 
* event type =132  2 second epics read
*  when  gdebugdumpepics=1,2,3 dump 131,132, both
*--------------------------------------------------------

      numevent = numevent + 1

      if (g_epics_output_filename.ne.' ' .and.
     &   (gdebugdumpepics.ge.1 )) then  !write out event
        dump_event = .true.
      else
        dump_event = .false.
        return
      endif
c
      evtype = jishft(craw(2),-16)
      if (evtype-gdebugdumpepics .gt. 130) dump_event = .false.
  
c
      if (dump_event) write (G_LUN_EPICS_OUTPUT,*) 'epics event #',numevent

      if (craw(3)-1.le.0) then
        write (6,*)
     1  '**g_examine_epics_event: bad record length; numevent=',
     1  numevent,', craw3=',craw(3)
        return
      endif

cccc      write (6,*) 'epics,evlen',evlen,numevent,craw(3)

      evlen=g_important_length(buffer(1:4*(craw(3)-1)))
      i = 1
cccc      write (6,*) 'epics,evlen',evlen,numevent,craw(3)
      do while (i.le.evlen)
        j = find_char (buffer, i, 10)   ! 10 = NewLine character
        if (i.eq.j) goto 20
        if(i.lt.j-1 .and. dump_event) write(G_LUN_EPICS_OUTPUT,'(4x,a)') buffer(i:j-1)
 20     i = j + 1
      enddo

      return
      end
