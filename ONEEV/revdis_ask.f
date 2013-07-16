      integer function revdis_ask(view)
*
*     User interaction.
*
*     Return -1 to exit, 0 to go to the next event, or a number to review
*     the current event with a different view.
*
* $Log: revdis_ask.f,v $
* Revision 1.1  1996/01/17 16:31:52  cdaq
* Initial revision
*
*
      implicit none
      save

      integer view

      INCLUDE 'gen_one_ev_info.cmn'
      include 'gen_routines.dec'

      integer*4 MLEN
      parameter (MLEN=132)
      character*132 oline,line,line_copy
      integer retval,i
      integer NVIEWS
      parameter (NVIEWS=3)
      real*8 deval
      integer*4 ieval
      character*1 cviews(NVIEWS)
      integer*4 iviews(NVIEWS)
      character*20 ffmt
      character*20 ifmt
      data cviews /'A','B','C'/
      data iviews /1,2,3/
      data ffmt/'e12.4'/
      data ifmt/'i10'/
      
      do while (.true.)
        write(6,'($,a)') ': '
        read(5,'(a)',err=99,end=99) oline

        call shiftall(oline,line)
        call no_blanks(line)
        if(line.eq.' ') then            ! Display next event
          retval = 0
          goto 1000                     ! return
        endif

        if(line(1:1).eq.line) then      ! Single character view selection
          do i=1,NVIEWS
            if(line.eq.cviews(i)) then
              retval = iviews(i)
              goto 1000
            endif
          enddo
        endif
        if(line(1:10).eq.'MAXEVENTS=') then
          read(line(11:MLEN),'(i10)') gen_display_wait_events
        else if(line(1:8).eq.'MAXTIME=') then
          read(line(9:MLEN),'(i10)') gen_display_wait_seconds
        else if(line(1:5).eq.'VIEW=') then
          read(line(6:MLEN),'(i10)') retval
          if(retval.le.0) retval=1
          goto 1000
        else if(line(1:5).eq.'IFMT=') then
          ifmt = line(6:MLEN)
        else if(line(1:5).eq.'FFMT=') then
          ffmt = line(6:MLEN)
        else if(line(1:7).eq.'STATUS ') then
          write(6,'("Test=      ",a)') gen_display_interesting(1:67)
          write(6,'("MAXTIME=  ",i7," seconds")') gen_display_wait_seconds
          write(6,'("MAXEVENTS=",i7," events")') gen_display_wait_events
          write(6,'("VIEW=     ",i7)') view
        else if(line(1:1).eq.'?') then
          write(6,*) ' Options:'
          write(6,*) '    MAXTIME= seconds          ! time limit'
          write(6,*) '    MAXEVENTS= n              ! event limit'
          write(6,*) '    STATUS                    ! display info'
          write(6,*) '    QUIT or EXIT              ! exit the program'
          write(6,*) '    =ctp expression           ! Print expression value'
          write(6,*) '    ctp logical expression    ! Event selection test'
          write(6,*) '    a                         ! Display 3D view'
          write(6,*) '    b                         ! Display top and side views'
          write(6,*) '    c                         ! Display a head on view of the chambers'
          write(6,*) ' '
        else if(line(1:1).eq.'=') then
          line_copy = line(2:MLEN)
          if(thevalchk(line_copy).eq.0) then
            deval = dtheval(line_copy)
            ieval = deval
            if(ieval.eq.deval) then
              write(6,'("Expression = "'//ifmt//')') ieval
            else
              write(6,'("Expression = "'//ffmt//')') deval
            endif
          else
            write(6,*) 'Invalid Expression'
          endif
        else if(line(1:5).eq.'EXIT '.or.line(1:5).eq.'QUIT
     $         '.or.line(1:2).eq.'Q ') then
          retval = -1
          goto 1000
        else                            ! Presumed to be a test expression
          if(thevalchk(line).ne.0) then
            write(6,*) "Illegal event selection test"
          else if(index(line,'=').gt.0 .and. 
     $           (index(line,'==').eq.0
     $           .and. index(line,'!=').eq.0
     $           .and. index(line,'>=').eq.0
     $           .and. index(line,'<=').eq.0)) then
            write(6,*) 'Illegal: Use "==" for (is equal to) operator'
          else
            gen_display_interesting = line
            retval=0
            goto 1000
          endif
        endif
      enddo
*
      

 99   continue
      retval = -1
      goto 1000

 1000 continue
      revdis_ask = retval
      return

      end
