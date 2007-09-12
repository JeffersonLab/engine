      subroutine g_examine_picture_event
* $Log$
* Revision 1.1.6.3  2007/09/12 14:40:03  brash
* *** empty log message ***
*
* Revision 1.1.6.2  2007/09/10 20:33:37  pcarter
* Implemented changes to allow compilation on RHEL 3,4,5 and MacOSX
*
* Revision 1.1.6.1  2007/05/15 02:54:45  jones
* Start to Bigcal code
*
* Revision 1.1.2.1  2004/06/30 19:31:08  cdaq
* Subroutine to dump HMS/SOS angle pictures (DJG)
*

* Revision 1.1  2004/06/28 18:30:13  saw
* Initial revision
*
*--------------------------------------------------------
      implicit none
      save
      external jishft

      character buffer*100000
      character*80 file
      equivalence (craw(5), buffer)
      integer i,j,evlen
      integer g_important_length,find_char
      integer evtype
      integer numevent
      logical dump_event
      integer*4 jishft

      include 'gen_craw.cmn'
      include 'gen_run_info.cmn'
      include 'sos_filenames.cmn'
      include 'hms_filenames.cmn'
*
* event type =131 30 second epics read 
* event type =132  2 second epics read
*  when  gdebugdumpepics=1,2,3 dump 131,132, both
*--------------------------------------------------------

      numevent = numevent + 1

c      if (g_epics_output_filename.ne.' ' .and.
c     &   (gdebugdumpepics.ge.1 )) then  !write out event
c        dump_event = .true.
c      else
c        dump_event = .false.
c        return
c      endif
c

      dump_event = .true.
      evtype = jishft(craw(2),-16)
c
c      if (dump_event) write (G_LUN_EPICS_OUTPUT,*) 'epics event #',numevent

      if (craw(3)-1.le.0) then
        write (6,*)
     1  '**g_examine_picture_event: bad record length; numevent=',
     1  numevent,', craw3=',craw(3)
        return
      endif

c      write (6,*) 'picture,evlen',evlen,numevent,craw(3)

      evlen=g_important_length(buffer(1:4*(craw(3)-1)))
      i = 1


      if(evtype.eq.146.and.h_angle_output_filename.ne.' ') then
         file = h_angle_output_filename
         call g_sub_run_number(file,gen_run_number)
         open(8,file=file,status='unknown',form='unformatted',access='direct',recl=evlen)
         write(8,rec=1) buffer(i:evlen)
         close(8)
      elseif(evtype.eq.147.and.s_angle_output_filename.ne.' ') then
         file = s_angle_output_filename
         call g_sub_run_number(file,gen_run_number)
         open(8,file=file,status='unknown',form='unformatted',access='direct',recl=evlen)
         write(8,rec=1) buffer(i:evlen)
         close(8)
      endif



      return
      end
