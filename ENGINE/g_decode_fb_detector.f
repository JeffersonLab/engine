      INTEGER*4 FUNCTION g_decode_fb_detector(roc,evfrag,length,did,
     $     maxhits,hitcount,planelist,counterlist,signalcount,signal0,
     $     signal1,signal2,signal3)
*----------------------------------------------------------------------
*- Created ?   Steve Wood, CEBAF
*- Corrected  3-Dec-1993 Kevin Beard, Hampton U.
*-    $Log$
*-    Revision 1.4  1994/06/18 02:48:04  cdaq
*-    (SAW) Add code for miscleaneous data and uninstrumented channels
*-
* Revision 1.3  1994/04/06  18:03:38  cdaq
* (SAW) # of bits to get channel number is now configurable (g_decode_subaddbit).
* Changed range of signal types from 1:4 to 0:3 to agree with documentation.
*
* Revision 1.2  1994/03/24  22:00:15  cdaq
* Temporarily change shift to get subaddress from 17 to 16
*
* Revision 1.1  1994/02/04  21:50:03  cdaq
* Initial revision
*
*----------------------------------------------------------------------
      implicit none
      SAVE
*
*     The following arguments don't get modified.
      integer*4 roc,evfrag(*),length,did,maxhits,signalcount

*     The following arguments get modified.
      integer*4 hitcount,planelist(*),counterlist(*)
      integer*4 signal0(*),signal1(*),signal2(*),signal3(*)
      integer pointer,newdid,subadd,slot,mappointer,plane
      integer counter,signal,sigtyp
*
      include 'gen_decode_common.cmn'
      include 'gen_detectorids.par'
*
      integer oslot,h
      integer subaddbit
*
      oslot = -1                     !illegal old slot
      pointer = 1
      newdid = did

      do while(pointer.le.length .and. did.eq.newdid)
*
        slot = iand(ISHFT(evfrag(pointer),-27),'1F'X)
        if(slot.ne.oslot) then
          mappointer = g_decode_slotpointer(roc+1,slot)
          oslot = slot
          subaddbit = g_decode_subaddbit(roc+1,slot) ! Usually 16 or 17
        endif
*
        subadd = iand(ISHFT(evfrag(pointer),-subaddbit),'7F'X)
*
*     If an module that uses a shift of 17 for the subaddress is in a slot
*     that we havn't told the map file about, it's data will end up in the
*     unstrimented channel "detector" hit list.  However, the decoder will
*     think that the subaddress starts in channel 16 (since some Lecroy
*     modules do so), The next statement will mean that only the first 64
*     channels will end up in the uninstrumented hit list.  The rest will
*     be lost.  If you don't want to put this module in the map file, put
*     in a single entry for it with a detector id of UNINST_ID (zero) and
*     the proper BSUB value.
*
        if (subadd .lt. '7F'X) then     ! Only valid subaddresses
                                        ! Skips headers for 1881 and 1876/7
          if(mappointer.gt.0) then
            newdid = g_decode_didmap(mappointer+subadd)
          else
            newdid = UNINST_ID
          endif
          if(newdid.eq.did) then
            if(hitcount .lt. maxhits) then ! Don't overwrite arrays
              if(did.ne.UNINST_ID) then
                plane = g_decode_planemap(mappointer+subadd)
                counter = g_decode_countermap(mappointer+subadd)
                signal =iand(evfrag(pointer),g_decode_slotmask(roc+1,slot))
              else
                plane = ishft(roc,16) + slot
                counter = subadd
                signal = evfrag(pointer)
              endif
              h = hitcount
              if(signalcount .eq. 1) then ! single signal counter
*     
*     Starting at end of hit list, search back until a hit earlier in
*     the sort order is found.
*     
                do while(h .gt. 0 .and. (plane .lt. planelist(h)
     $               .or.(plane .eq. planelist(h).and. counter .lt.
     $               counterlist(h))))
*
*     Shift hit to next place in list
*     
                  planelist(h+1) = planelist(h)
                  counterlist(h+1) = counterlist(h)
                  signal0(h+1) = signal0(h)
                  h = h - 1
                enddo
                h = h + 1               ! Put hit pointer to blank
                planelist(h) = plane
                counterlist(h) = counter
                signal0(h) = signal
                hitcount = hitcount + 1
              else if(signalcount.eq.4) then ! Multiple signal counter
*     
*     Starting at the end of the hist list, search back until a hit on
*     the same counter or earlier in the sort order is found.
*     
                do while(h .gt. 0 .and. (plane .lt. planelist(h)
     $               .or.(plane .eq. planelist(h).and. counter .lt.
     $               counterlist(h))))
                  planelist(h+1) = planelist(h)
                  counterlist(h+1) = counterlist(h)
                  signal0(h+1) = signal0(h)
                  signal1(h+1) = signal1(h)
                  signal2(h+1) = signal2(h)
                  signal3(h+1) = signal3(h)
                  h = h - 1
                enddo
                sigtyp = g_decode_sigtypmap(mappointer+subadd)
                if(h.lt.1 .or. plane.ne.planelist(h)
     $               .or. counter .ne. counterlist(h)) then
                  h = h + 1
                  planelist(h) = plane
                  counterlist(h) = counter
                  hitcount = hitcount + 1
                endif
                if(sigtyp.eq.0) then
                  signal0(h) = signal
                else if (sigtyp.eq.1) then
                  signal1(h) = signal
                else if (sigtyp.eq.2) then
                  signal2(h) = signal
                else if (sigtyp.eq.3) then
                  signal3(h) = signal
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

      g_decode_fb_detector = pointer - 1 ! Number of words processed
      
      return
      end
**************
*     Local Variables:
*     mode: fortran
*     fortran-if-indent: 2
*     fortran-do-indent: 2
*     End:
