      SUBROUTINE G_examine_go_info(buffer,ABORT,err)
*--------------------------------------------------------
*-
*-    Purpose and Methods : examine a control event and gather various
*-                          information from it.
*- 
*-    Input: buffer             - raw data buffer
*-         : ABORT              - success or failure
*-         : err                - reason for failure, if any
*- 
*-   Created  30-Nov-1995   John Arrington, Caltech.
*-
* $Log$
* Revision 1.1  1995/12/08 20:07:54  cdaq
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*18 here
      parameter (here= 'G_examine_go_info')
*     
      INTEGER buffer(*)
      LOGICAL ABORT
      CHARACTER*(*) err
*
      include 'gen_decode_common.cmn'
*
      integer EvType
      integer*4 pointer,subpntr,ind
      integer*4 evlen,sublen,subheader,slotheader,numvals
      integer*4 roc,slot
      integer*4 jiand,jishft
      logical*4 found_thresholds
*
*----------------------------------------------------------------------
      err= ' '
*
c        write(6,'(a,z10)') 'buffer(2)=',buffer(2)
c      if(jiand(buffer(2),'FFFF'x).ne.'01CC'x) then
c         err = 'Event is not a control event'
c         ABORT = .true.
c         call g_add_path(here,err)
c         return
c      endif
      EvType = jISHFT(buffer(2),-16)
      if (evtype.ne.133) then
         err = 'Event is not a control event'
         ABORT = .true.
         call g_add_path(here,err)
         return
      endif
*     
      found_thresholds = .false.
      evlen = buffer(1)


c	write(6,*) 'evlen=',buffer(1)
      pointer = 3                     !1=#/words, 2=event type
      roc= (jiand(buffer(2),'FF'x))
c	write(6,*) 'roc=',roc,'evtype=',evtype

      do while (.not.found_thresholds .and. pointer.le.evlen)
        sublen=buffer(pointer)
c	write(6,*) '  sublen=',sublen
        subheader=buffer(pointer+1)
c	write(6,'(a,z10)') '  subheader=',subheader

        if (jishft(jiand(subheader,'FF0000'x),-16) .eq. '10'x) then !thresholds
          found_thresholds = .true.
c	  write(6,*) '  THRESHOLDS!'
          subpntr=2                            !skip past main subheader.
c	  write(6,*) '  subpntr=',subpntr
          do while (subpntr .lt. sublen)
            slotheader=buffer(pointer+subpntr)
            slot=jishft(jiand(slotheader,'FF000000'x),-24)
c	    write(6,'(a,z10)') '    slotheader=',slotheader
            numvals=jiand(slotheader,'FF'x)
c	    write(6,*) '    slot#',slot,' has ',numvals,' thresholds'
            do ind=1,numvals
              subpntr=subpntr+1
              g_threshold_readback(ind,roc,slot)=buffer(pointer+subpntr)
ccc              write(6,*) 'g_threshold_readback(',ind,roc,slot,')=',g_threshold_readback(ind,roc,slot)
c	      write(6,*) '      threshold#',ind,'=',TMPTHRESHOLD
            enddo
            subpntr=subpntr+1                  !skip to next slotheader
c	    write(6,*) 'subpntr=',subpntr
          enddo   !NEED CHECK FOR NEXT HEADER.
          pointer=pointer+subpntr
        else
c	  write(6,*) '  NOT THRESHOLDS.  WHO CARES'
          pointer=pointer+sublen+1
        endif
      enddo
*
      RETURN
      END
