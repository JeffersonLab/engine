      INTEGER*4 FUNCTION g_decode_fb_detector(oslot,roc,evfrag,length,did,
     $     maxhits,hitcount,planelist,counterlist,signalcount,signal0,
     $     signal1,signal2,signal3)
*----------------------------------------------------------------------
*- Created ?   Steve Wood, CEBAF
*- Corrected  3-Dec-1993 Kevin Beard, Hampton U.
*-    $Log$
*-    Revision 1.10  1995/01/27 20:14:04  cdaq
*-    (SAW) Add assorted diagnostic printouts.  Add hack to look for the headers
*-          on new 1881M/1877 modules while maintaining backward compatibility.
*-
* Revision 1.9  1994/10/20  12:34:55  cdaq
* (SAW) Only print out "Max exceeded, did=" meesage once
*
* Revision 1.8  1994/06/27  02:14:18  cdaq
* (SAW) Ignore all words that start with DC
*
* Revision 1.7  1994/06/22  20:21:24  cdaq
* (SAW) Put -1 in hodoscope signals that don't get any data
*
* Revision 1.6  1994/06/22  20:07:37  cdaq
* (SAW) Fix problems with filling of hodoscope type hit lists (multiple signal)
*
* Revision 1.5  1994/06/21  16:02:54  cdaq
* (SAW) Ignore DCFF0000 headers from Arrington's CRL's
*
* Revision 1.4  1994/06/18  02:48:04  cdaq
* (SAW) Add code for miscleaneous data and uninstrumented channels
*
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
      integer*4 oslot
      integer*4 hitcount,planelist(*),counterlist(*)
      integer*4 signal0(*),signal1(*),signal2(*),signal3(*)
      integer pointer,newdid,subadd,slot,mappointer,plane
      integer counter,signal,sigtyp
*
      include 'gen_decode_common.cmn'
      include 'gen_detectorids.par'
      include 'gen_scalers.cmn'
      integer iscaler,nscalers
*
      integer h,hshift
      integer subaddbit
      logical printerr  !flag to turn off printing of error after 1 time.
      logical lastwordheader
*     
      if(oslot.lt.0) lastwordheader = .false.
      printerr = .true.
      pointer = 1
      newdid = did

      do while(pointer.le.length .and. did.eq.newdid)
*
        if(iand(evfrag(pointer),'FF000000'x).eq.'DC000000'x) then ! Catch arrington's headers
          write(6,*) 'no gate or too much data from DCs.'
          write(6,*) 'Turn off parallel link if this persists'
          pointer = pointer + 1
          goto 987
        endif
*
*     Check for event by event scalers thrown in by the scaler hack.
*
*        if(iand(evfrag(pointer),'FF000000'x).eq.'DA000000'x) then ! Magic header
*          nscalers = iand(evfrag(pointer),'FF'x)
*          do iscaler=1,nscalers
**            type *,iscaler,evfrag(pointer+iscaler)
*            evscalers(iscaler) = evfrag(pointer+iscaler)
*          enddo
*          pointer = pointer + nscalers + 1
*          goto 987
*        endif
        if(evfrag(pointer).le.1.and.evfrag(pointer).ge.0) then
          write(6,'(" BAD FB value evfrag(",i4,")=",z10," ROC=",i2)')
     $         pointer,evfrag(pointer),roc
          pointer = pointer + 1
          goto 987
        endif
        slot = iand(ISHFT(evfrag(pointer),-27),'1F'X)
        if(slot.ne.oslot) then
          if (slot.le.0 .or. slot.ge.26 .or. roc.le.0 .or. roc.ge.9) then
            write (6,*) 'roc,slot=',roc,slot
            write (6,*) 'evfrag(pointer)=',evfrag(pointer)
          endif
          mappointer = g_decode_slotpointer(roc+1,slot)
          oslot = slot
          subaddbit = g_decode_subaddbit(roc+1,slot) ! Usually 16 or 17

c
c     On 1881M's and 1877, a subaddress of zero could be a header word, so
c     we need to put in some hackery to catch these.  We need to make sure
c     that 1881's and 1876's will still work.
c
c     A real ugly hack that looks to see if the first word of an 1881M or
c     1877 has a subaddress of zero, in which case it is the header word and must
c     be discarded.  If it is an 1881 or 1876, then the the first word of a
c     new slot will have a subaddress of '7F' and later be discarded.
c
          if(subaddbit.eq.17) then      ! Is not an 1872A (which has not headers)
            if(iand(evfrag(pointer),'00FE0000'X).eq.0) then ! probably a header
              if(iand(evfrag(pointer),'07FF0000'X).ne.0) then
                print *,
     $               "SHIT: we misidentified a real data word as a header
     $               "
                print *,"DID=",did,", SLOT=",slot,", POINTER=",pointer
              else
*                print *,"FOUND NEW HEADER in roc, slot ",roc,slot

                pointer = pointer + 1
                goto 987
              endif
            endif
          endif
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
                                        ! Skips headers for 1881 and 1876
          if(mappointer.gt.0) then
            newdid = g_decode_didmap(mappointer+subadd)
          else
            newdid = UNINST_ID
          endif
          if(newdid.eq.did) then
            if(did.ne.UNINST_ID) then
              plane = g_decode_planemap(mappointer+subadd)
              counter = g_decode_countermap(mappointer+subadd)
              signal =iand(evfrag(pointer),g_decode_slotmask(roc+1,slot))
            else
              plane = ishft(roc,16) + slot
              counter = subadd
              signal = evfrag(pointer)
            endif
            if(hitcount .lt. maxhits) then ! Don't overwrite arrays
              if(signalcount .eq. 1) then ! single signal counter
*     
*     Starting at end of hit list, search back until a hit earlier in
*     the sort order is found.
*     
                h = hitcount
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
                h = hitcount
                do while(h .gt. 0 .and. (plane .lt. planelist(h)
     $               .or.(plane .eq. planelist(h).and. counter .lt.
     $               counterlist(h))))
                  h = h - 1
                enddo
*
*     If plane/counter match is not found, then need to shift up the array
*     to make room for the new hit.
*                
                if(h.le.0.or.plane.ne.planelist(h) ! Plane and counter
     $               .or.counter.ne.counterlist(h)) then ! not found
                  h = h + 1
                  do hshift=hitcount,h,-1 ! Shift up to make room
                    planelist(hshift+1) = planelist(hshift)
                    counterlist(hshift+1) = counterlist(hshift)
                    signal0(hshift+1) = signal0(hshift)
                    signal1(hshift+1) = signal1(hshift)
                    signal2(hshift+1) = signal2(hshift)
                    signal3(hshift+1) = signal3(hshift)
                  enddo
                  planelist(h) = plane
                  counterlist(h) = counter
                  signal0(h) = -1
                  signal1(h) = -1
                  signal2(h) = -1
                  signal3(h) = -1
                  hitcount = hitcount + 1
                endif
*
                sigtyp = g_decode_sigtypmap(mappointer+subadd)
*
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
            else if(hitcount.eq.maxhits .and. printerr) then ! Only print this message once
              type *,'g_decode_fb_detector: Max exceeded, did=',
     $             did,', max=',maxhits
              printerr = .false.
*     
*     Print/generate some kind of error that the hit array has been
*     exceeded.
*     
            endif
            pointer = pointer + 1
*         else
*           exit and get called back with the correct arrays for the new did
          endif
        else
          pointer = pointer + 1
        endif
 987    continue
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
