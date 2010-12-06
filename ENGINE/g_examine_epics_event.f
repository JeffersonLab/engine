      subroutine g_examine_epics_event
* $Log$
* Revision 1.5.20.1.2.4  2010/12/06 18:31:13  jones
* Add IF statements to set half wave plate status during analysis
* according run number
* Exception for run 72608 and 72612
*
* Revision 1.5.20.1.2.3  2009/09/02 13:38:35  jones
* add readout of halfwave plate status
*
* Revision 1.5.20.1.2.1  2009/03/31 19:33:00  cdaq
* *** empty log message ***
*
* Revision 1.5.20.1  2007/09/10 20:33:37  pcarter
* Implemented changes to allow compilation on RHEL 3,4,5 and MacOSX
*
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
      external jieor

      character buffer*12000
      equivalence (craw(5), buffer)
      integer i,j,evlen
      integer g_important_length,find_char
      integer evtype
      integer numevent
      logical dump_event
      integer*4 jishft
      integer HALFWAVE
      include 'gen_craw.cmn'
      include 'gen_run_info.cmn'
      include 'gen_filenames.cmn'
      include 'sane_ntuple.cmn'
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
c
      half_plate=+1  ! before run 72412
      if (gen_run_number .ge.   72412) half_plate=  -1
      if (gen_run_number .ge.   72609) half_plate=  +1
      if (gen_run_number .ge.   72613) half_plate=  -1
      if (gen_run_number .ge.   72618) half_plate=  +1
      if (gen_run_number .ge.   72733) half_plate=  -1
      if (gen_run_number .ge.   72832) half_plate=  +1
      if (gen_run_number .ge.   72896) half_plate=  -1
      if (gen_run_number .ge.   72926) half_plate=  +1
      if (gen_run_number .ge.   72953) half_plate=  -1
      if (gen_run_number .ge.   73012) half_plate=  +1
c
cccc      write (6,*) 'epics,evlen',evlen,numevent,craw(3)

      evlen=g_important_length(buffer(1:4*(craw(3)-1)))
      i = 1
cccc      write (6,*) 'epics,evlen',evlen,numevent,craw(3)
c      polarea=-1000.
      polarization_ch = .FALSE.
      do while (i.le.evlen)
        j = find_char (buffer, i, 10)   ! 10 = NewLine character
        if (i.eq.j) goto 20
        if(i.lt.j-1 .and. dump_event) write(G_LUN_EPICS_OUTPUT,'(4x,a)') buffer(i:j-1)
        if (i+11.le.j-1) then   !text line.
c     ********** read out the BPMs (POS values first...) **********
           if (buffer(i:i+11).eq.'hcptNMR_Area') then
              read(buffer(i+13:j-1),*,err=20) polarea
c              write(*,*)"HERE IS POLARIZATION",polarea
              polarization_ch = .TRUE.
           ENDIF             !    hcptNMR_Area   
           if (buffer(i:i+15).eq.'hcptPolarization')then
              read(buffer(i+16:j-1),*,err=20)polarization
              
           endif
           if (buffer(i:i+14).eq.'IGL1I00DI24_24M') then
              read(buffer(i+16:j-1),*,err=20)HALFWAVE
c     write(*,*)'WAVE PLATE ',HALFWAVE
              if ( gen_run_number .eq. 72608 .or. gen_run_number .eq. 72612) then
              half_plate=0.
              if(HALFWAVE.eq.0)half_plate=-1.
              if(HALFWAVE.eq.1)half_plate=1.
              endif
           endif
        ENDIF
 20     i = j + 1
      enddo
c      call system("rm fort.21")
c      call system
c     &     ("tail -n 361 scalers/epics72252.txt | 
c     & grep NMR_A | awk '{ print $2}'>fort.21")
c      read(21,*)polarea
c      write(*,*)'HERE IS EPIC EVENT',polarea
c      close(21)
c        write(*,*) gen_run_number," halfwave = ", half_plate,halfwave
      return
      end
