      INTEGER*4 FUNCTION g_decode_fb_detector(roc,evfrag,length,did,
     $     maxhits,hitcount,planelist,counterlist,signalcount,signal1,
     $     signal2,signal3,signal4)
*----------------------------------------------------------------------
*- Created ?   Steve Wood, CEBAF
*- Corrected  3-Dec-1993 Kevin Beard, Hampton U.
*-    $Log$
*-    Revision 1.1  1994/02/04 21:50:03  cdaq
*-    Initial revision
*-
*----------------------------------------------------------------------
      implicit none
      SAVE
*
*     The following arguments don't get modified.
      integer*4 roc,evfrag(*),length,did,maxhits,signalcount

*     The following arguments get modified.
      integer*4 hitcount,planelist(*),counterlist(*)
      integer*4 signal1(*),signal2(*),signal3(*),signal4(*)
      integer pointer,newdid,subadd,slot,mappointer,plane
      integer counter,signal,sigtyp
*
      include 'gen_decode_common.cmn'
*
      integer dir,oslot,h
      data dir/1/
      logical undetermined_shift
      data undetermined_shift/.TRUE./
*
*------------------------------------------------------------------------
*
      IF(undetermined_shift) THEN         !determine proper shift direction
        If(ISHFT(dir,+1).eq.2) Then       ![machine dependent]
          dir= +1                         !positive shift ~ multiplication
        Else
          dir= -1                         !negative shift ~ multiplication
        EndIf
        undetermined_shift= .FALSE.
      ENDIF
*
      oslot = -1                     !illegal old slot
      pointer = 1
      newdid = did

      do while(pointer.le.length .and. did.eq.newdid)
*
         subadd = iand(ISHFT(evfrag(pointer),-17*dir),'7F'X)
*
         if (subadd .lt. '7F'X) then            ! Only valid slots
            slot = iand(ISHFT(evfrag(pointer),-27*dir),'1F'X)
            if(slot.ne.oslot) then
               mappointer = g_decode_slotpointer(roc+1,slot)
               oslot = slot
            endif
            newdid = g_decode_didmap(mappointer+subadd)
            if(newdid.eq.did) then
               if(hitcount .lt. maxhits) then ! Don't overwrite arrays
                  plane = g_decode_planemap(mappointer+subadd)
                  counter = g_decode_countermap(mappointer+subadd)
                  signal = 
     &              iand(evfrag(pointer),g_decode_slotmask(roc+1,slot))
                  h = hitcount
                  if(signalcount .eq. 1) then ! single signal counter

*     
*     Starting at end of hit list, search back until a hit earlier in
*     the sort order is found.
*     
                     do while(h .gt. 0 .and. (plane .lt. planelist(h)
     $                    .or.(plane .eq. planelist(h).and. counter .lt.
     $                    counterlist(h))))
*     
*     Shift hit to next place in list
*     
                        planelist(h+1) = planelist(h)
                        counterlist(h+1) = counterlist(h)
                        signal1(h+1) = signal1(h)
                        h = h - 1
                     enddo
                     h = h + 1  ! Put hit pointer to blank
                     planelist(h) = plane
                     counterlist(h) = counter
                     signal1(h) = signal
                     hitcount = hitcount + 1
                  else if(signalcount.eq.4) then ! Multiple signal counter
*     
*     Starting at the end of the hist list, search back until a hit on
*     the same counter or earlier in the sort order is found.
*     
                     do while(h .gt. 0 .and. (plane .lt. planelist(h)
     $                    .or.(plane .eq. planelist(h).and. counter .lt.
     $                    counterlist(h))))
                        planelist(h+1) = planelist(h)
                        counterlist(h+1) = counterlist(h)
                        signal1(h+1) = signal1(h)
                        signal2(h+1) = signal2(h)
                        signal3(h+1) = signal3(h)
                        signal4(h+1) = signal4(h)
                        h = h - 1
                     enddo
                     sigtyp = g_decode_sigtypmap(mappointer+subadd)
                     if(h.lt.1 .or. plane.ne.planelist(h)
     $                    .or. counter .ne. counterlist(h)) then
                        h = h + 1
                        planelist(h) = plane
                        counterlist(h) = counter
                        hitcount = hitcount + 1
                     endif
                     if(sigtyp.eq.1) then
                        signal1(h) = signal
                     else if (sigtyp.eq.2) then
                        signal2(h) = signal
                     else if (sigtyp.eq.3) then
                        signal3(h) = signal
                     else if (sigtyp.eq.4) then
                        signal4(h) = signal
                     endif
                  endif
               else
*     
*     Print/generate some kind of error that the hit array has been
*     exceeded.
*     
               endif
               pointer = pointer + 1
            endif
         else
            pointer = pointer + 1
         endif
      enddo

      g_decode_fb_detector = pointer - 1        ! Number of words processed
      
      return
      end
