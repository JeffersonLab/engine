      subroutine g_analyze_scalers(event,ABORT,err)
*
* $Log$
* Revision 1.3  1994/07/07 15:24:24  cdaq
* (SAW) Fix bugs
*
c Revision 1.2  1994/07/07  15:23:16  cdaq
c (SAW) Correct pointers for actual bank structure
c
c Revision 1.1  1994/06/22  21:02:17  cdaq
c Initial revision
c
*
      implicit none
      integer*4 event(*)
*
      character*17 here
      parameter (here='g_analyze_scalers')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'gen_scalers.cmn'
*
*     Scaler events have a header in from of each scaler.  High 16 bits
*     will contain the address (the switch settings).  Address for hall C
*     will be of the form DANN, where NN is the scaler number.  The low 16
*     bits will contain the number of scaler values to follow (this should
*     be no larger than 16, but we will allow more.)
*
      integer evtype, evlen, pointer
      integer scalid, countinmod, address, counter
      
*
      evtype = ishft(event(2),-16)
*
*     Should check against list of known scaler events
*
      evlen = event(1) + 1
      if(evlen.gt.3) then           ! We have a scaler bank
         pointer = 3
*
         do while(pointer.lt.evlen)
*
            scalid = iand(ishft(event(pointer),-16),'FF'x)
            countinmod = iand(event(pointer),'FFFF'x)
*
*     Might want to check that count is not to big.
*
            if(countinmod.ne.16) then
               err = 'Scaler module header word has count<>16'
               ABORT = .true.
               call g_add_path(here,err)
               return                   ! Safest action
            endif
*
            address = scalid*16
            do counter = 1,countinmod
               scalers(address+counter) = event(pointer + counter)
            enddo
            pointer = pointer + countinmod + 1 ! Add 17 to pointer
         enddo
      else
         err = 'Event not big enough to contain scalers'
         ABORT = .true.
         call g_add_path(here,err)
         return
      endif

      return
      end

