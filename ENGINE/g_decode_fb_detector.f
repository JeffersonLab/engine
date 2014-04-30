      INTEGER*4 FUNCTION g_decode_fb_detector(oslot,roc,evfrag,length,did,
     $     maxhits,hitcount,planelist,counterlist,signalcount,signal0,
     $     signal1,signal2,signal3)
*----------------------------------------------------------------------
*- Created ?   Steve Wood, CEBAF
*- Corrected  3-Dec-1993 Kevin Beard, Hampton U.
* $Log: g_decode_fb_detector.f,v $
* Revision 1.23  2003/09/05 15:31:23  jones
* Merge in online03 changes (mkj)
*
* Revision 1.22.2.1  2003/07/24 13:08:11  cdaq
* Changes made for adding scaler ROC 5 during Baryon exp. (MKJ for SAW)
*
* Revision 1.22  2002/09/25 14:40:03  jones
*    Eliminate commented out diagnostic messages and the variables
*      buffer,iscaler,nscalers associated with them.
*
* Revision 1.21  1999/11/04 20:35:16  saw
* Linux/G77 compatibility fixes
*
* Revision 1.20  1998/12/17 21:50:31  saw
* Support extra set of tubes on HMS shower counter
*
* Revision 1.19  1998/12/01 15:54:57  saw
* (SAW) Slight change in debugging output
*
* Revision 1.18  1997/04/03 10:56:05  saw
* (SAW) Better report of DCFE code words.  Prints out roc, slot, event
* number and how many extra events are in the module.
*
* Revision 1.17  96/09/04  14:34:19  14:34:19  saw (Stephen A. Wood)
* (JRA) More error reporting of error codes in FB data stream
* 
* Revision 1.16  1996/04/29 19:46:19  saw
* (JRA) Tweak diagnostic messages
*
* Revision 1.15  1996/01/16 20:51:55  cdaq
* (SAW) Fixes:  Forgot why
*
* Revision 1.14  1995/11/28 18:59:24  cdaq
* (SAW) Change arrays that use roc as index to start with zero.
*
* Revision 1.13  1995/10/09 18:23:29  cdaq
* (JRA) Comment out some debugging statements
*
* Revision 1.12  1995/07/27 19:10:02  cdaq
* (SAW) Use specific bit manipulation routines for f2c compatibility
*
* Revision 1.11  1995/01/31  15:55:52  cdaq
* (SAW) Make sure mappointer and subaddbit are set on program entry.
*
* Revision 1.10  1995/01/27  20:14:04  cdaq
* (SAW) Add assorted diagnostic printouts.  Add hack to look for the headers
*       on new 1881M/1877 modules while maintaining backward compatibility.
*
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
      include 'gen_event_info.cmn'
*
      integer h,hshift
      integer subaddbit
      logical printerr  !flag to turn off printing of error after 1 time.
      logical firsttime
      logical plane_test
*
      integer*4 jishft, jiand, jieor
*     
      printerr = .true.
      pointer = 1
      newdid = did

      firsttime = .true.
      do while(pointer.le.length .and. did.eq.newdid)
*
        if(jieor(jiand(evfrag(pointer),'FFFFFFFF'x),'DCAA0000'x).eq.0) then ! VME/FB event length mismatch
          write(6,'(a,i10)') 'ERROR: VME/Fastbus event length mismatch for event #',gen_event_id_number
          write(6,'(a,z9,a,z9,a)') '   Fastbus event length:',evfrag(pointer+1),
     &        ' VME event length:',evfrag(pointer+2),' (or vice-versa).'
          pointer = pointer + 3
          goto 987
! Check for extra events in FB modules on sync events
        else if(jieor(jiand(evfrag(pointer),'FFFF0000'x),'DCFE0000'x).eq.0) then
          write(6,'(a,i2,a,i3,a,i3,a,i10)') 'ROC',roc,': Slot'
     $         ,jiand(jishft(evfrag(pointer),-11),'1F'x),': '
     $         ,jiand(evfrag(pointer),'7FF'x),' extra events, event=',
     &         gen_event_id_number
          pointer = pointer + 1
          goto 987
        else if(jieor(jiand(evfrag(pointer),'FF000000'x),'DC000000'x).eq.0) then ! Catch arrington's headers
          write(6,'(a,i2,a,i10,a,z10)') 'ROC',roc,': no gate or too much data, event=',
     &         gen_event_id_number,' error dataword=',evfrag(pointer)
          pointer = pointer + 1
          goto 987
        endif


        if(evfrag(pointer).le.1.and.evfrag(pointer).ge.0) then

! on sync events, get zeros at end of event.
          if (gen_event_id_number .eq. 1000*int(gen_event_id_number/1000)) then
            if (evfrag(pointer).ne.0) then

              write(6,'(" ERROR: BAD FB value evfrag(",i4,")=",z10," ROC=",i2,"event=",i7)')
     $         pointer,evfrag(pointer),roc,gen_event_id_number
            endif
          endif
          pointer = pointer + 1
          goto 987
        endif
        slot = jiand(JISHFT(evfrag(pointer),-27),'1F'X)
        if(slot.ne.oslot.or.firsttime) then
          if (slot.le.0 .or. slot.ge.26 .or. roc.le.0 .or. roc.ge.9) then
            write (6,'(a,i3,i3,i3,z10,a,i5,a,i8)') 'roc,slot,oslot,evfrag=',roc,
     &           slot,oslot,evfrag(pointer),
     $           '(p=',pointer,') for event #',gen_event_id_number
            write (6,'(a,i3)') '  Probably after slot',jiand(JISHFT(evfrag(pointer-1),-27),'1F'X)
            pointer = pointer + 1
            goto 987
          else
            mappointer = g_decode_slotpointer(roc,slot)
            subaddbit = g_decode_subaddbit(roc,slot) ! Usually 16 or 17
          endif
        endif
        if(slot.ne.oslot) then
          oslot = slot

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
            if(jiand(evfrag(pointer),'00FE0000'X).eq.0) then ! probably a header
              if(jiand(evfrag(pointer),'07FF0000'X).ne.0) then
                print *,"SHIT:misidentified real data word as a header"
                print *,"DID=",did,", SLOT=",slot,", POINTER=",pointer
              else
                pointer = pointer + 1
                goto 987
              endif
            endif
          endif
        endif
*
        subadd = jiand(JISHFT(evfrag(pointer),-subaddbit),'7F'X)
*
*     If a module that uses a shift of 17 for the subaddress is in a slot
*     that we havn't told the map file about, it's data will end up in the
*     unstrimented channel "detector" hit list.  However, the decoder will
*     think that the subaddress starts in channel 16 (since some Lecroy
*     modules do so), The next statement will mean that only the first 64
*     channels will end up in the uninstrumented hit list.  The rest will
*     be lost.  If you don't want to put this module in the map file, put
*     in a single entry for it with a detector id of UNINST_ID (zero) and
*     the proper BSUB value.
*
c        if (subadd .lt. '7F'X) then     ! Only valid subaddresses
        if (subadd .lt. 255) then       ! Only valid subaddresses
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
              signal =jiand(evfrag(pointer),g_decode_slotmask(roc,slot))
            else
              plane = jishft(roc,16) + slot
              counter = subadd
              signal = evfrag(pointer)
            endif
            if(hitcount .lt. maxhits .or.
     $           (hitcount.eq.maxhits .and. signalcount .gt. 1)) then ! Don't overwrite arrays
              if(signalcount .le. 1) then ! single signal counter
*     
*     Starting at end of hit list, search back until a hit earlier in
*     the sort order is found.
*     
                h = hitcount
                if ( h .gt. 0) then
                do while((plane .lt. planelist(h)
     $               .or.(plane .eq. planelist(h).and. counter .lt.
     $               counterlist(h))))
*
*     Shift hit to next place in list
*     
                  planelist(h+1) = planelist(h)
                  counterlist(h+1) = counterlist(h)
                  signal0(h+1) = signal0(h)
                  h = h - 1
                  if ( h .eq.0) goto 888
                enddo
                endif
 888            h = h + 1       ! Put hit pointer to blank
                planelist(h) = plane
                counterlist(h) = counter
                signal0(h) = signal
                hitcount = hitcount + 1
              else ! Multiple signal counter sigcount= 2 or 4 allowed
*     
*     Starting at the end of the hist list, search back until a hit on
*     the same counter or earlier in the sort order is found.
*     
                h = hitcount
c                  write(*,*) ' mkj' ,h
                if ( h .gt.0) then
                do while( (h .gt. 0) .and. (((h .gt. 0).and.plane .lt. planelist(h))
     $               .or.( (h .gt. 0).and.plane .eq. planelist(h).and. counter .lt.
     $               counterlist(h))))
                  h = h - 1
c                  write(*,*) ' mkj' ,h
                  if (h .le.0) goto878
                enddo
                endif
 878            continue
*
*     If plane/counter match is not found, then need to shift up the array
*     to make room for the new hit.
*                
c                  write(*,*) ' mkj2' ,h
                plane_test=.false.
                if ( h .gt.0) plane_test=(plane.ne.planelist(h).or.counter.ne.counterlist(h))
                if(h.eq.0.or.plane_test) then ! not found
                  if(hitcount.lt.maxhits) then
                    h = h + 1
                    do hshift=hitcount,h,-1 ! Shift up to make room
                      planelist(hshift+1) = planelist(hshift)
                      counterlist(hshift+1) = counterlist(hshift)
                      signal0(hshift+1) = signal0(hshift)
                      signal1(hshift+1) = signal1(hshift)
                      if(signalcount.eq.4) then
                        signal2(hshift+1) = signal2(hshift)
                        signal3(hshift+1) = signal3(hshift)
                      endif
                    enddo
                    planelist(h) = plane
                    counterlist(h) = counter
                    signal0(h) = -1
                    signal1(h) = -1
                    if(signalcount.eq.4) then                  
                      signal2(h) = -1
                      signal3(h) = -1
                    endif
                    hitcount = hitcount + 1
                  else                  ! Too many hits
                    if(printerr) then
                      print *,'g_decode_fb_detector: Max exceeded, did=',
     $                     did,', max=',maxhits,': event',gen_event_id_number
                      print *,'   roc,slot,cntr,sig,subadd=',roc,slot,counter,sigtyp,subadd
                      printerr = .false.
                    endif
                  endif
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
              print *,'g_decode_fb_detector: Max exceeded, did=',
     $             did,', max=',maxhits,': event',gen_event_id_number
              print *,'   roc,slot,cntr=',roc,slot,counter
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
