      subroutine g_decode_config(ABORT, error, fname)
*----------------------------------------------------------------------------------
*
*     Purpose and Methods:
*
*     Build a table that maps (ROC, Slot, Subadd) to (Detector ID, Plane,
*     Counter, Signal type).  Also saves a mask for each slot that is used
*     to extract value from fastbus word.
*
*     Inputs:
*
*     fname      Name of file
*     -          Allowed keywords. roc, slot, detector, nsubadd, mask
*     -          Numerical lines: Subadd, Plane, Counter, Signal
*
*     Outputs:
*
*     ABORT
*     error
*
*     Created  16-NOV-1993   Stephen Wood, CEBAF
*     Modified  3-Dec-1993   Kevin Beard, Hampton Univ.; rewrote parsing
*-    $Log$
*-    Revision 1.1  1994/02/01 20:38:10  cdaq
*-    Initial revision
*-
*----------------------------------------------------------------------------------
      implicit none
      SAVE
*
      character*30 here
      parameter (here= 'g_decode_config')
*
      character*(*) error
      logical ABORT
      character*(*) fname
*
      include 'gen_detectorids.cmn'
      include 'gen_decode_common.cmn'
      integer SPAREID                           ! Need a LUN handler?
      parameter (SPAREID=67)
      integer MAXLINE
      parameter (MAXLINE=300)
*
      character*(MAXLINE) line    
      character*20 key,pad
      logical OK,echo,debug,override,prse
      data echo/.FALSE./
      data debug,override/2*.FALSE./
      integer Nval,val(80)
*
      integer lp, lpeq, m                          ! Line pointers
      integer*4 roc, slot, subadd, mask
      integer*4 did, plane, counter, signal, nsubadd
      integer*4 lastroc, lastslot
      integer N_lines_read
      logical MATCH                           !KB_tricks function
*********************************************************************************
*     Valid data lines are
*    
*     roc=
*     slot=
*     detector=
*     nsubadd=
*     A line with 4 comma separated numbers, Subadd, plane, "wire #", sigtyp
*     sigtyp may be left blank (e.g. for wire chambers) in which case zero
*     is assumed.
*supported integer notation:             example:
*  i,j,k,l,m,n                        5,4,  5,2,  44
*  i::j     i,i+1,...,j-1,j           5::10
*  M*n      n,n,n,n....,n  M times    4*66
*  nnnnX   hexadecimal                ABCDx
*  nnnnO   octal                      7067o
*  nnnn    decimal                    9849
*  nnnnB   binary                     101011b
*********************************************************************************
      ABORT= .TRUE.
*     Need to pass this file name as an argument or through a common block.
      open(unit=SPAREID,type='OLD',READONLY,file=fname,err=999)
*
      roc = -1
      slot = -1
      lastroc = -1
      lastslot = -1
      N_lines_read= 0
      OK= .TRUE.
      DO WHILE (OK)
*
         OK= .FALSE.
         error= ':error reading'
         read(SPAREID, '(a)',err=555,end=666) line
         OK= .TRUE.
         error= ' '
555      N_lines_read= N_lines_read+1
*
         If(OK) Then
*
           if(debug) type *,'.'
           if(echo) call g_log_message(line)
*
           call NO_comments(line)
           call NO_tabs(line)
           call NO_leading_blanks(line)
           call ShiftAll(line,line)
*
           if(line.NE.' ') then
             lpeq = index(line,'=')
*
             IF(lpeq.gt.1) THEN                     ! Setting roc, slot or detector
               key= line(1:lpeq-1)
               pad= line(lpeq+1:)
             ELSE
               key= line
               pad= line
             ENDIF
*
             call data_row(pad)
             m= INDEX(pad,',,')
             IF(m.GT.1) pad(m+1:)= ' '
             call get_values(pad,Nval,val,prse)
             IF(debug) THEN
                If(lpeq.GT.1) Then
                  call G_log_message(key(1:lpeq-1)//' = '//pad)
                Else
                  call G_log_message(pad)
                EndIf
                type *,' ...Nval=',Nval,' ok=',prse
             ENDIF
             OK= Nval.GE.1
*
             IF(MATCH(key,'help')) THEN
                OK= .TRUE.
                type *
                type *,' subroutine: '//here
                type *,' keywords: '
                type *,' ECHO NOECHO         - show input lines'
                type *,' OVERride NOOVERride - ignore problems'
                type *,' ROC = n'
                type *,' SLOT = n'
                type *,' DETector = n'
                type *,' MASK = n'
                type *,' NSUBaddress = n'
                type *,' c,p,e,s '//
     &                 ' [channel,plene,element,signal]'
                type *
             ELSEIF(MATCH(key,'over*ride')) THEN
                override= .TRUE.
                OK= .TRUE.
             ELSEIF(MATCH(key,'noover*ride')) THEN
                override= .FALSE.
                OK= .TRUE.
             ELSEIF(MATCH(key,'debug')) THEN
                debug= .TRUE.
                echo= .TRUE.
                OK= .TRUE.
             ELSEIF(MATCH(key,'nodebug')) THEN
                debug= .FALSE.
                echo= .FALSE.
                OK= .TRUE.
             ELSEIF(MATCH(key,'echo')) THEN
                echo= .TRUE.
                OK= .TRUE.
             ELSEIF(MATCH(key,'noecho')) THEN
                echo= .FALSE.
                OK= .TRUE.
             ELSEIF(OK.and.MATCH(key,'roc')) THEN
                lastroc = roc
                roc= val(1)
                OK= 0.LE.roc .and. roc.LE.G_DECODE_MAXROCS
                If(.NOT.OK) error= ':illegal ROC number'
             ELSEIF(OK.and.MATCH(key,'slot')) THEN
                lastslot = slot
                slot= val(1)
                OK= 1.LE.slot .and. slot.LE.G_DECODE_MAXSLOTS
                If(.NOT.OK) error= ':illecal SLOT number'
             ELSEIF(OK.and.MATCH(key,'det*ector')) THEN
                did= val(1)
                OK= 1.LE.did
                If(.NOT.OK) error= ':illecal DETECTOR number'
             ELSEIF(OK.and.MATCH(key,'nsub*address')) THEN
                nsubadd = val(1)
                OK= 1.LE.Nsubadd
                If(.NOT.OK) error= ':illecal NSUBADDress number'
             ELSEIF(OK.and.MATCH(key,'mask')) THEN
                mask = val(1) !binary, octal, decimal, and 
*                                             hexadecimal supported!
             ELSEIF(OK.and.Nval.GE.3) THEN
*
               If(Nval.LT.4) val(4)= 0
*
               subadd=  val(1)
	       OK= 0.LE.subadd .and. subadd.LT.nsubadd
               If(.NOT.OK) Then
                 error= ':illegal subaddress'
               Else
                 plane=   val(2)
                 OK= plane.GE.1
                 if(.NOT.OK) then
                   error= ':illegal plane number'
                 else
                   counter= val(3)
                   OK= counter.GE.1
                   IF(.NOT.OK) THEN
                     error= ':illegal element number'
                   ELSE
                     signal=  val(4)
	             OK= 0.LE.signal .and. signal.LE.3
                     If(.NOT.OK) Then
                       error= ':illegal signal number'
                     Else
                       error= ' '
                     EndIf
                   ENDIF
                 endif
               EndIf
*
               If(OK .and. roc.ne.lastroc.or.slot.ne.lastslot) Then
                 if(g_decode_slotpointer(roc+1,slot).eq.0) then
                   g_decode_slotpointer(roc+1,slot) =
     &                                           g_decode_nextpointer
                   g_decode_subaddcnt(roc+1,slot) = nsubadd
                   g_decode_slotmask(roc+1,slot) = mask
                   g_decode_nextpointer = g_decode_nextpointer +
     &                                           nsubadd
                 endif
               EndIf

*- beware long lines...............
*        1         2         3         4         5         6         7
*23456789012345678901234567890123456789012345678901234567890123456789012
*
               If(OK) Then
                 g_decode_didmap( g_decode_slotpointer(roc+1,slot)
     &                                   +subadd ) = did
                 g_decode_planemap( g_decode_slotpointer(roc+1,slot)
     &                                   +subadd ) = plane
                 g_decode_countermap( g_decode_slotpointer(roc+1,slot)
     &                                   +subadd ) = counter
                 g_decode_sigtypmap( g_decode_slotpointer(roc+1,slot)
     &                                   +subadd ) = signal
               EndIf
*
             ELSEIF(OK.and.key.NE.' ') THEN
               OK= .FALSE.
               error= ':bad keyword:'//key
             ELSE
               OK= .FALSE.
               error= ':bad line:'//line
             ENDIF
*
           endif              !nonblank line
*
           if(.NOT.OK .and. override) then
             call G_log_message('OVERRIDE: '//error)
             error= ' '
             OK= .TRUE.
           endif
*
         EndIf                !OK read
*
      ENDDO
*  
888   ABORT= .NOT.OK
      IF(ABORT) THEN
        call G_add_path(here,error)
      ELSE
        error= ' '
      ENDIF
      close(unit=SPAREID)
      return
*
666   OK= N_lines_read.GT.0
      error= ':no lines read before End-of-File'
      GOTO 888                       !normal end-of-file?
*
999   continue
      error = ':Unable to open file "'//fname//'"'
      call G_add_path(here,error)
      return
      end
