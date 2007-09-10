      SUBROUTINE G_klugeup_kinematics(ABORT,err)
*--------------------------------------------------------
* $Log$
* Revision 1.3.24.1  2007/09/10 20:33:37  pcarter
* Implemented changes to allow compilation on RHEL 3,4,5 and MacOSX
*
* Revision 1.3  1996/09/04 14:38:32  saw
* (JRA) Initialize problems logical
*
* Revision 1.2  1996/01/22 15:11:46  saw
* (JRA) Change cpbeam to gpbeam
*
* Revision 1.1  1995/12/08 20:12:00  cdaq
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
      external jishft, jiand
*
      character*21 here
      parameter (here= 'g_klugeup_kinematics')
*
      logical ABORT
      character*800 err,mss
*
      include 'gen_filenames.cmn'
      include 'gen_craw.cmn'
      include 'gen_run_info.cmn'
      include 'gen_data_structures.cmn'
      include 'hms_data_structures.cmn'
      include 'sos_data_structures.cmn'
*
      logical problems
      integer ind
      integer evtype
*      integer SPAREID
*      parameter (SPAREID=67)
*
      character*80 g_config_environmental_var
      parameter (g_config_environmental_var= 'ENGINE_CONFIG_FILE')
*
      integer*4 jishft,jiand
      integer*4 status
      integer*4 evclose
*
      real*4 ebeam,phms,thms,psos,tsos
*
*
*--------------------------------------------------------
*
*-attempt to open FASTBUS-CODA file
*
      problems = .false.
      g_data_source_in_hndl= 0
      g_data_source_opened = .false.
      call g_open_source(ABORT,err)
      if(ABORT.or.err.ne.' ') then
         call G_add_path(here,err)
         call G_rep_err(ABORT,err)
         If(ABORT) STOP
         err= ' '
      endif
 
      DO ind=1,3
        mss= ' '
*
c	write(6,*) 'get event ',ind
        call G_get_next_event(ABORT,err) !get and store 1 event 
c	write(6,*) 'got event ',ind
*
*     Check if this is a physics event or a CODA control event.
*
        if(.not.problems) then
          evtype = jishft(craw(2),-16)
c          write(6,'(a,z10)') 'craw(2)=',craw(2)
c          write(6,*) 'evtype=',evtype

          if (evtype.eq.130) then       !run info event (get e,p,theta)
            call g_extract_kinematics(ebeam,phms,thms,psos,tsos)
            if (gpbeam .ge. 7. .and. ebeam.le.7.) then !sometimes ebeam in MeV
              gpbeam=abs(ebeam)
              write(6,*) 'gpbeam=',abs(ebeam),' GeV'
            endif
            if (hpcentral .ge. 7.) then
              write(6,*) 'hpcentral=',abs(phms),' GeV/c'
              hpcentral=abs(phms)
            endif
            if (htheta_lab .le. 0.) then
              write(6,*) 'htheta_lab=',abs(thms),' deg.'
              htheta_lab=abs(thms)*3.14159265/180.
            endif
            if (spcentral .ge. 7.) then
              write(6,*) 'spcentral=',abs(psos),' GeV/c'
              spcentral=abs(psos)
            endif
            if (stheta_lab .le. 0.) then
              write(6,*) 'stheta_lab=',abs(tsos),' deg.'
              stheta_lab=abs(tsos)*3.14159265/180.
            endif
c            write(6,*) 'GOT KINEMATICS!!!'
          endif

c          if(jiand(CRAW(2),'FFFF'x).eq.'10CC'x) then ! Physics event
c            write(6,*) 'AAAARRGGHHHHHH!  Physics EVENT!!!!!'
c          Else
c            if(evtype.eq.129) then
c              write(6,*) 'AAAARRGGHHHHHH!  Scalar EVENT!!!!!'
c            else if (evtype.eq.133) then  !SAW's new go_info events
c              write(6,*) 'AAAARRGGHHHHHH!  SAWs new go_info  EVENT!!!!!'
c            else
c              write(6,*) 'AAAARRGGHHHHHH! well, maybe not so bad. control event'
c              call g_examine_control_event(CRAW,ABORT,err)
c            endif
c          EndIf
        endif
      ENDDO                             !found a problem or end of run
*
*
      status = evclose(g_data_source_in_hndl)
      g_data_source_opened = .false.
      if(ABORT.or.err.ne.' ') then
         call G_add_path(here,err)
         call G_rep_err(ABORT,err)
         If(ABORT) STOP
         err= ' '
      endif

      return
      end
